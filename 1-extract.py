# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

#Run this whenever restarting.
import pandas;
import re;
import shelve;
import numpy as np;
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet as wn
wnl = WordNetLemmatizer()
from nltk.corpus import cmudict
d = cmudict.dict()
#https://stackoverflow.com/a/4103234
def nsyl(word):
  return [len(list(y for y in x if y[-1].isdigit())) for x in d[word.lower()]] 
import sylcount as sc;

s = shelve.open("rawdata.dat");
allSents = s["allSents"];
s.close();
pandas.set_option("max_columns",40)
#End runthis

#Part I: Converting txt to Python data
#Source of the conllu file: https://github.com/UniversalDependencies/UD_English-EWT
#Created by sticking three files together
devfile = open("en_ewt-ud-all.conllu","r",encoding="utf8");

currentDocID = -1;
currentSentID = -1;
currentWordID = -1;
currentWithinDocSentID = -1;

allSents = [];

#Read the data in those text files into a Python-readable form
for line in devfile:
    if re.match("# newdoc id =",line):
        currentDocID += 1;
        currentDoc = line[14:79];
        currentDoc = line[line.index('=')+2:len(line)-1];
        currentWithinDocSentID = -1;
        #print("New doc:", line[line.index('=')+2:len(line)-1]);
    elif re.match("# sent_id =",line):
        currentWithinDocSentID += 1;
        currentSentID += 1;
        currentDict = {"doc": currentDoc,
                      "sentID": currentWithinDocSentID,
                      "phrase": "",
                      "phraseDF": []};
        allSents.append(currentDict);
        #print("New sent! Text:");
    elif re.match("# text = ",line):
        currentSent = line[line.index('=')+2:len(line)-1];
        allSents[currentSentID]['phrase'] = currentSent;
        allSents[currentSentID]['phraseDF'] = pandas.DataFrame(
                columns=["ID","FORM","LEMMA","UPOS","XPOS","FEATS","HEAD",
                         "DEPREL","DEPS","MISC"]);
        #print(allSents[currentSentID]['phrase']);
    elif re.match("\d+\t",line):
        newData = pandas.DataFrame(line.split("\t")).transpose();
        newData.columns = ["ID","FORM","LEMMA","UPOS","XPOS","FEATS","HEAD",
                           "DEPREL","DEPS","MISC"]
        #print(newData);
        allSents[currentSentID]['phraseDF'] = allSents[currentSentID]['phraseDF'].append(newData);
        print(allSents[currentSentID]['phraseDF']);        

#allSents should be of length 14620

i=0
for sentence in allSents:
    sentence['phraseDF']['ID'] = sentence['phraseDF']['ID'].astype(int);
    print(i);
    i += 1;

#I don't want to be running that monster code ever again, so let's
#store the stupid thing in a dat file
s = shelve.open("rawdata.dat");
s["allSents"] = allSents;
s.close();

#Part II: A bunch of useful functions
def unifyPhrases(phrase1, phrase2):
    if phrase1['doc'] != phrase2['doc']:
        raise ValueError('The two phrases are from different docs.');
    elif phrase1['sentID'] != phrase2['sentID']:
        raise ValueError('The two phrases are from different sentences.');
    else:
        newPhrase = {'doc': phrase1['doc']};
        newPhrase['phraseDF'] = pandas.merge(phrase1['phraseDF'],
                                 phrase2['phraseDF'], how='outer', on=["ID",
                                "FORM","LEMMA","UPOS","XPOS","FEATS","HEAD",
                                "DEPREL","DEPS","MISC"])
        newPhrase['phrase'] = ' '.join((newPhrase['phraseDF']['FORM'].values.tolist()));
        newPhrase['sentID'] = phrase1['sentID']
    return newPhrase;


#Test the function (should just return the sentence)
print(unifyPhrases(allSents[currentSentID],allSents[currentSentID]));
        
#Given the head, get the whole phrase. Recursive.
def getPhrase(headID, phrase):
    df = phrase['phraseDF'];
    head = df.loc[df["ID"]==headID,];
    form = df.loc[df["ID"]==headID,"FORM"];
    form = form.iloc[0];
    
    #Loop through all the words to find ALL the dependents
    #Please 腦補 (=supplement using your brain) the relevant meme
    i = 1;
    newPhrase = {'doc': phrase['doc'], 'phraseDF': head,
                 'phrase': form,
                 'sentID': phrase['sentID']};
    ##print(newPhrase['phraseDF'])
    while i <= df.shape[0]:
        depString = df.loc[df['ID'] == i,df.columns.values=="DEPS"];
        if depString.shape[0] != 0:
            depString = depString.iloc[0,0];
            depString = depString.split("|");
            ##print(depString)
            matchFound = False;
            for pair in depString:
                if re.match(str(headID)+":",pair):
                    matchFound = True;
            if matchFound:
               ## print(df.iloc[i-1,:]);
                newPhrase = unifyPhrases(newPhrase, getPhrase(i,phrase))
               ## print(newPhrase)
                #newPhrase['phraseDF'] = newPhrase['phraseDF'].append(df.iloc[i-1,:]);
                
        i += 1;
    newPhrase['phraseDF'] = newPhrase['phraseDF'].sort_values(by=['ID'])
    newPhrase['phrase'] = ' '.join(newPhrase['phraseDF']['FORM'].values.tolist());
    
    return newPhrase;
    
        
#Test the function
print(getPhrase(19,allSents[currentSentID]));

#Todo: getDependentS
def getDependent(headID, relation, phrase):
    df = phrase['phraseDF'];
    dep = str(headID) + ":" + relation;
    i = 1;
    newPhrase = 0;
    while i <= df.shape[0]:
        searchString = df["DEPS"].iloc[i-1];
        searchString = searchString.split("|");
        matchFound = False;
        for pair in searchString:
            if re.match(dep,pair):
                matchFound = True;
        if matchFound:
            newPhrase = getPhrase(i, phrase);
            break;
        i += 1;
            
    return newPhrase;

def getDependents(headID, relation, phrase):
    df = phrase['phraseDF'];
    dep = str(headID) + ":" + relation;
    i = 1;
    newPhrases = [];
    while i <= df.shape[0]:
        searchString = df["DEPS"].iloc[i-1];
        searchString = searchString.split("|");
        matchFound = False;
        for pair in searchString:
            if re.match(dep,pair):
                matchFound = True;
        if matchFound:
            newPhrases.append(getPhrase(i, phrase));
        i += 1;
            
    return newPhrases;
#Test the function
getDependents(7,"obj",allSents[0])

#This guy returns a row rather than a full phrase object
#At least for now
def getParents(childID, phrase):
    df = phrase['phraseDF'];
    depString = df["DEPS"].iloc[childID-1];
    depString = depString.split("|");
    
    parents = [None] * len(depString);
    i = 0
    for string in depString:
        string = string.split(":");
        parents[i] = df.iloc[int(string[0])-1,:];
        i += 1;
    
    return parents;

print(getParents(2,allSents[currentSentID]));

def getInfoFromFeats(feats):
    info = {};
    feats = feats.split("|");
    for feat in feats:
        name = feat.split("=")[0];
        value = feat.split("=")[1];
        info[name] = value;
    return(info);

def getValueFromInfo(info, key):
    if key in info:
        value = info['key'];
    else:
        value = "/";
    return(value);


def getHead(parentID, phrase):
    i = 0;
    df = phrase['phraseDF'];
    head = 0;
    while i < df.shape[0]:
        if re.search(str(parentID),df.iloc[i,]["DEPS"]):
            head = df.iloc[i,];
        i += 1;
    return(head);
    
def sylCount(string):
    try:
        count = nsyl(string);
    except:
        count = sc.sylco(df.iloc[i,]["FORM"]);
    return(count);
        
    
#Part III: Converting raw data (a database of sentences)
# to a database of clauses (which I want for my project),
# first step: Put each clause in one row and extract
# directly expressed arguments

allSentsR = allSents[0:4] #R stands for reduced and is for testing purposes

clauseTableColnames = ["ClauseID","Doc","SentID","SentForm",
                            "VForm","VLemma","VMorph","VMorphForm","VTense","VAspect","Voice",
                            "VSylCo","Aux1","Aux2","Aux3","VClass","OvertSubj",
                            "OvertObj","OvertIObj","OvertObl1","Obl2",
                            "Obl3","Obl4","Obl5","OvertSubjHead","OvertObjHead",
                            "SubjDef","SubjNum","SubjPers","SubjAnim","SubjSylCo",
                            "ObjDef","ObjNum","ObjPers","SubjAnim","ObjSylCo"
                            ];
                       
    
clauseTable = pandas.DataFrame(columns=clauseTableColnames);

#This is the clause ID WITHIN DOCUMENTS.
#We'll add an overall ID at the end. No need to sweat it.
currentClauseID = 1;
for sentence in allSentsR:
    i = 1;
    df = sentence['phraseDF'];
    preds = [];
    predTypes = [];
    while i <= df.shape[0]:
        ##(df.iloc[i-1,]["XPOS"])
        if re.match("V",df.iloc[i-1,]["XPOS"]):
            if df.iloc[i-1,]["UPOS"] != "AUX":
                preds.append(i-1);
                predTypes.append("V");
        i += 1;
    j = 0;
    for i in preds:
            #Initialise the row first.
            currentRow = ["/"]*len(clauseTableColnames);
            currentRow = pandas.DataFrame.from_records(currentRow);
            currentRow = currentRow.transpose();
            currentRow.columns=clauseTableColnames;
            #Fill in doc-level, sentence-level info and clause ID.
            currentRow["ClauseID"] = currentClauseID;
            currentRow["Doc"] = sentence['doc'];
            currentRow["SentID"] = sentence['sentID'];
            currentRow["SentForm"] = sentence['phrase'];
            #Now for clause-level stuff. First the V/A.
            featsInfo = getInfoFromFeats(df.iloc[i,]["FEATS"])
            if predTypes[j] == "V":
                
                #Verb form stuff.
                currentRow["VForm"] = df.iloc[i,]["FORM"];
                currentRow["VLemma"] = df.iloc[i,]["LEMMA"];
                currentRow["VMorph"] = df.iloc[i,]["XPOS"];
                currentRow["VMorphForm"] = featsInfo["VerbForm"];
                if "Voice" in featsInfo:
                    currentRow["Voice"] = featsInfo["Voice"];
                else:
                    currentRow["Voice"] = "Act";
                auxiliaries = getDependents(i+1,"aux",sentence);
                k = 1;
                
                #Fill up auxiliaries
                for aux in auxiliaries:
                    auxHead = getHead(i+1,aux);
                    auxInfo = getInfoFromFeats(auxHead["FEATS"]);
                    currentRow["Aux" + str(k)] = auxHead["LEMMA"];
                    if k == 3:
                        break;
                    k = k + 1;
                    
                #Syllable count. Simple!
                currentRow["VSylCo"] = sylCount(df.iloc[i,]["FORM"])
                    
                #More abstract grammatical categories
                if featsInfo["VerbForm"] == "Fin":
                    currentRow["VTense"] = featsInfo["VerbForm"];
                else:
                    for aux in auxiliaries:
                        auxHead = getHead(i+1,aux);
                        auxInfo = getInfoFromFeats(auxHead["FEATS"]);
                        if auxInfo["VerbForm"] == "Fin":
                            if "Tense" in auxInfo:
                                currentRow["VTense"] = auxInfo["Tense"];
                            else:
                                currentRow["VTense"] = "NMA!";
                            break;
                if currentRow["VTense"][0] == "/":
                    currentRow["VTense"] == "Tenseless";
                #Determine aspect from the auxiliaries
                for aux in auxiliaries:
                        auxHead = getHead(i+1,aux);
                        
                #And now the arguments
                #Subject is first
                currSubject = getDependents(i+1,"nsubj",sentence)
                if len(currSubject) == 0:
                    currSubject = getDependents(i+1, "nsubj:pass",sentence);
                    if len(currSubject) != 0:
                        currentRow["OvertSubj"] = currSubject[0]['phrase'];
                        currSubject = currSubject[0];
                    else:
                        currSubject = "NOSUBJ";
                else:
                    currentRow["OvertSubj"] = currSubject[0]["phrase"];
                    currSubject = currSubject[0];
                    
                if currSubject != "NOSUBJ":
                    subjectHead = getHead(i+1,currSubject)
                    subjectFeats = getInfoFromFeats(subjectHead['FEATS'])
                    currentRow["SubjSylCo"] = sylCount(currentRow["OvertSubj"])
                    
                        
                
            currentClauseID += 1;
            j += 1;
            ##print(currentRow)
            ##print(clauseTable)
            clauseTable = clauseTable.append(currentRow,ignore_index=True
                                             ,sort=False)
        
clauseTable

#NMA! = Needs Manual Attention!
#We deal with NMA stuff semi-automatically now.