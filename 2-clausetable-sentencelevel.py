# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

#Run this whenever restarting.
import os;
#os.chdir("G:\我的雲端硬碟\corpus priming\eng")
os.chdir("G:\My Drive\corpus priming\eng")
import pandas;
import re;
import shelve;
import numpy as np;
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
import nltk;
from nltk.corpus import cmudict
nltk.download('cmudict');
d = cmudict.dict()
#https://stackoverflow.com/a/4103234
def nsyl(word):
  return [len(list(y for y in x if y[-1].isdigit())) for x in d[word.lower()]] 
import sylcount as sc;
from nltk import word_tokenize, pos_tag, ne_chunk, tree
nltk.download('verbnet');
from nltk.corpus import verbnet as vn
import csv
from nltk.corpus import wordnet as wn
s = shelve.open("rawdata.dat");
allSents = s["allSents"];
s.close();
pandas.set_option("max_columns",50)

#End runthis

sentence1 = "Mark and John are working in the United States, while Mary found a pig in Europe.";
print (ne_chunk(pos_tag(word_tokenize(allSents[2]['phrase']))));

sentence2 = "The 20th century is a good time for ABC Corporation.";
print (ne_chunk(pos_tag(word_tokenize(sentence2))));

sentence2 = "Mr Chan is aboard the Titanic.";
print (ne_chunk(pos_tag(word_tokenize(sentence2))));

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
print(unifyPhrases(allSents[0],allSents[0]));
        
#Given the head, get the whole phrase. Recursive.
def getPhrase(headID, phrase, skip = []):
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
            #print(depString)
            matchFound = False;
            for pair in depString:
                #print(pair)
                if re.match(str(headID)+":",pair):
                    matchFound = True;
            if matchFound & (i not in skip):
                #print(i)
                #print(skip)
               ## print(df.iloc[i-1,:]);
               newPhrase = unifyPhrases(newPhrase, getPhrase(i,phrase,[headID]+skip));
               ## print(newPhrase)
                #newPhrase['phraseDF'] = newPhrase['phraseDF'].append(df.iloc[i-1,:]);
                
        i += 1;
    newPhrase['phraseDF'] = newPhrase['phraseDF'].sort_values(by=['ID'])
    newPhrase['phrase'] = ' '.join(newPhrase['phraseDF']['FORM'].values.tolist());
    
    return newPhrase;
    
        
#Test the function
print(getPhrase(19,allSents[0]));

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
        print(searchString)
        for pair in searchString:
            print(pair)
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
            #print("i: " + str(i));
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

print(getParents(2,allSents[0]));

def getInfoFromFeats(feats):
    info = {};
    if feats != "_":
        feats = feats.split("|");
        for feat in feats:
            name = feat.split("=")[0];
            value = feat.split("=")[1];
            info[name] = value;
    return(info);

def getHead(parentID, phrase):
    i = 0;
    df = phrase['phraseDF'];
    head = 0;
    while i < df.shape[0]:
        if re.search(str(parentID),df.iloc[i,]["DEPS"]):
            head = df.iloc[i,];
        i += 1;
    return(head);
        
def doNothing():
    a = 1;


def getValueFromInfo(info, key):
    if key in info:
        value = info[key];
    else:
        value = "/";
    return(value);
    
def sylCount(string):
    try:
        words = string.split(" ");
        count = 0;
        for word in words:
            count = count + np.mean(nsyl(word));
    except:
        count = sc.sylco(string);
    return(count);

def getPosTaggedPhraseFromTable(phrase):
    postags = phrase["phraseDF"]["XPOS"];
    words = phrase["phraseDF"]["FORM"];
    result = list();
    for i in np.arange(0,len(words),1):
        result.append((postags.iloc[i],words.iloc[i]));
    return(result);
    
def isHyponym(hyponym, synset_hypernym, proper=False):
    if isinstance(hyponym, str):
        synsets_hyponym = wn.synsets(hyponym);
    elif isinstance(hyponym, list):
        synsets_hyponym = hyponym;
    else:
        synsets_hyponym = [hyponym];
    answer = False;
    for synset_hyponym in synsets_hyponym:
        #print(synsets_hyponym);
        if synset_hypernym in synset_hyponym.hypernyms():
            answer = True;
            break;
        elif isHyponym(synset_hyponym.hypernyms(),synset_hypernym):
            answer = True;
            break;
    if proper == False:
        for synset_hyponym in synsets_hyponym:
            if synset_hyponym == synset_hypernym:
                answer = True;
    return answer;
    
synset_human = wn.synsets('human')[0];
synset_person = wn.synsets('person')[0];
synset_animal = wn.synsets('animal')[0];
synset_org = wn.synsets('organization')[0];
synset_vehicle = wn.synsets('vehicle')[0];
synset_machine = wn.synsets('machine')[0];
synset_conc = wn.synsets('physical_entity')[0];
synset_nonconc = wn.synsets('abstract_entity')[0];
synset_loc = wn.synsets('location')[0];
synset_time = wn.synsets('time')[4];

synset_period = wn.synsets('period')[4];
synset_virus = wn.synsets('virus')[0];
synset_bacterium = wn.synsets('bacterium')[0];
synset_building = wn.synsets('building')[0];

#Part III: Converting raw data (a database of sentences)
# to a database of clauses (which I want for my project),
# first step: Put each clause in one row and extract
# directly expressed arguments


nomSemExceptions = ["member"]

allSentsR = allSents[0:10] #R stands for reduced and is for testing purposes

clauseTableColnames = ["ClauseID","Doc","SentID","SentForm",
                            "VForm","VLemma","VMorph","VMorphForm","VTense","VAspect","Voice",
                            "VSylCo","Aux3","Aux2","Aux1","PassAux","VClass","OvertSubj",
                            "OvertSubjHead","SubjDef","SubjNum","SubjPers","SubjAnim","SubjSynType",
                            "SubjSylCo","SubjMorph","OvertObj","OvertIObj","OvertObjHead",
                            "ObjDef","ObjNum","ObjPers","ObjAnim","ObjSylCo","ObjMorph",
                            "ObjSynType","OvertObl1","Obl2","Obl3","Obl4","Obl5"];
                       
    
clauseTable = pandas.DataFrame(columns=clauseTableColnames);

#This is the clause ID WITHIN DOCUMENTS.
#We'll add an overall ID at the end. No need to sweat it.
currentClauseID = 1;
for sentence in allSentsR:
    print("Doing sentence: ",sentence['sentID'])
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
            currentRow = dict()
            for col in clauseTableColnames:
                currentRow[col] = "/"
            
            #Fill in doc-level, sentence-level info and clause ID.
            currentRow["ClauseID"] = currentClauseID;
            currentRow["Doc"] = sentence['doc'];
            currentRow["SentID"] = sentence['sentID'];
            currentRow["SentForm"] = sentence['phrase'];
            #Now for clause-level stuff. First the V/A.
            featsInfo = getInfoFromFeats(df.iloc[i,]["FEATS"])
            if predTypes[j] == "V":
                #print("hihihihi1")
                
                #Verb form stuff.
                currentRow["VForm"] = df.iloc[i,]["FORM"];
                currentRow["VLemma"] = df.iloc[i,]["LEMMA"];
                currentRow["VMorph"] = df.iloc[i,]["XPOS"];
                currentRow["VMorphForm"] = featsInfo["VerbForm"];
                auxiliaries = getDependents(i+1,"aux",sentence);
                auxLemmas = [None] * len(auxiliaries)
                
                k = len(auxiliaries);
                #Fill up auxiliaries
                for aux in auxiliaries:
                    auxHead = getHead(i+1,aux);
                    auxLemmas[k-1] = auxHead["LEMMA"];
                    auxInfo = getInfoFromFeats(auxHead["FEATS"]);
                    currentRow["Aux" + str(k)] = auxHead["FORM"];
                    if k == 1:
                        break;
                    k = k - 1;
                if "Voice" in featsInfo:
                    currentRow["Voice"] = featsInfo["Voice"];
                    currentRow["PassAux"] = auxLemmas[0]
                else:
                    currentRow["Voice"] = "Act";
                    
                #Syllable count. Simple!
                currentRow["VSylCo"] = sylCount(df.iloc[i,]["FORM"]);
                    
                #More abstract grammatical categories
                if featsInfo["VerbForm"] == "Fin":
                    currentRow["VTense"] = getValueFromInfo(featsInfo,"Tense");
                else:
                    for aux in auxiliaries:
                        auxHead = getHead(i+1,aux);
                        auxInfo = getInfoFromFeats(auxHead["FEATS"]);
                        if auxInfo["VerbForm"] == "Fin":
                            if "Tense" in auxInfo:
                                currentRow["VTense"] = auxInfo["Tense"];
                                break;
                            elif auxHead["FORM"] == "will":
                                currentRow["VTense"] = "Fut";
                                break;
                if currentRow["VTense"] == "/":
                    currentRow["VTense"] = "Tenseless";
                    
                #Determine aspect from the auxiliaries
                try:
                    if (currentRow["Aux1"] == "being") | (currentRow["Aux1"] == "getting"):
                        currentRow["VAspect"] = "Prog";
                    elif featsInfo["VerbForm"] == "Part" & featsInfo["Tense"] == "Pres":
                        currentRow["VAspect"] = "Prog";
                except:
                    doNothing();
                    
                for aux in auxLemmas:
                    if aux == "have":
                        if currentRow["VAspect"] == "Prog":
                            currentRow["VAspect"] = "PerfProg";
                        else:
                            currentRow["VAspect"] = "Perf";
                    break;
                
                if currentRow["VAspect"] == "/":
                    currentRow["VAspect"] = "Simple";
                
                #Arguments
                #Subject first.
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
                    subjectHead = getHead(i+1,currSubject);
                    subjectFeats = getInfoFromFeats(subjectHead['FEATS']);
                    currentRow["SubjSylCo"] = sylCount(currentRow["OvertSubj"]);
                    currentRow["SubjMorph"] = subjectHead["XPOS"];
                    
                    currentRow["OvertSubjHead"] = subjectHead["FORM"];
                    
                    if currentRow["SubjMorph"] in ["NN","NNS"]:
                        currentRow["SubjSynType"] = "full";
                    elif currentRow["SubjMorph"] in ["NNP","NNPS"]:
                        currentRow["SubjSynType"] = "proper";
                    elif currentRow["SubjMorph"] in ["DT"]:
                        if getValueFromInfo(subjectFeats, "Prontype") == "Dem":
                            currentRow["SubjSynType"] = "dem";
                        else:
                            currentRow["SubjSynType"] = "det";
                        currentRow["SubjSynType"] = "det";
                    elif currentRow["SubjMorph"] in ["EX"]:
                        currentRow["SubjSynType"] = "there";
                    elif currentRow["SubjMorph"] in ["WP","WDT"]:
                        currentRow["SubjSynType"] = "wh";
                    elif currentRow["SubjMorph"] in ["PRP"]:
                        if currentRow["OvertSubj"] in ["mine","yours","his",
                                     "hers","its","ours","theirs"]:
                            currentRow["SubjSynType"] = "poss";                            
                        else:
                            currentRow["SubjSynType"] = "pron";
                    elif currentRow["SubjMorph"] == "CD":
                        currentRow["SubjSynType"] = "num";
                    elif currentRow["SubjMorph"] == "/":
                        currentRow["SubjSynType"] = "/";
                    else:
                        currentRow["SubjSynType"] = "NMN!";
                        
                    
                    #Number
                    currentRow["SubjNum"] = getValueFromInfo(subjectFeats, "Number");
                    if currentRow["SubjNum"] =="/":
                        if subjectHead["XPOS"] == "CD":
                            if subjectHead["LEMMA"] == "one":
                                currentRow["SubjNum"] = "Sing";
                            else:
                                currentRow["SubjNum"] = "Plur";
                                
                    #Person   
                    currentRow["SubjPers"] = getValueFromInfo(subjectFeats, "Person");
                    if (currentRow["SubjPers"] == "/") & (currentRow["OvertSubj"] != "/"):
                        currentRow["SubjPers"] = "3"
                        
                    #Definiteness
                    currDeterminers = getDependents(subjectHead["ID"],"det",sentence)
                    print(currDeterminers)
                    for determiner in currDeterminers:
                        detHead = getHead(subjectHead["ID"],determiner)
                        #print('detHead')
                        #print(detHead)
                        defInfo = getInfoFromFeats(detHead["FEATS"])
                        print("PRONTYPE",getValueFromInfo(defInfo,"PronType"))
                        if (getValueFromInfo(defInfo,"Definite") == "Def") | (getValueFromInfo(defInfo,"PronType") == "Dem"):
                            currentRow["SubjDef"] = "def";
                        elif getValueFromInfo(defInfo,"Definite") == "Ind":
                            currentRow["SubjDef"] = "indef";
                        
                    if currentRow["SubjDef"] == "/":                        
                        if currentRow["SubjMorph"] in ["PRP","NNP"]:
                            currentRow["SubjDef"] = "def";
                        else:
                            currentRow["SubjDef"] = "indef";                            
                    #Doubt: every and all???
                
                    #Animacy          
                    #We will be working with the animacy tags from 
                    #Zaenen et al. (2004)
                    #human, org, animal, place, time,
                    #concrete, nonconc, mac, veh
                    if currentRow["SubjMorph"] == ["PRP"]:
                        if currentRow["OvertSubj"] in ["I","me","he","she","him","her"]:
                            currentRow["SubjAnim"] = "human";
                    elif currentRow["SubjMorph"] in ["NNP","NNPS"]:
                        nertree = ne_chunk(pos_tag(word_tokenize(sentence["phrase"])));                      
                        first = True;
                        for subtree in nertree.subtrees():                                
                            if first == False:
                                containsHead = False;
                                for word in subtree:
                                    if word[1] == subjectHead["FORM"]:
                                        containsHead = True;
                                if containsHead:                                        
                                    if subtree.label() == "PERSON":
                                        currentRow["SubjAnim"] = "human";
                                    elif subtree.label() == "GPE":
                                        currentRow["SubjAnim"] = "loc";
                                    elif subtree.label() == "ORGANIZATION":
                                        currentRow["SubjAnim"] = "org";
                                    elif subtree.label() == "PERSON":
                                        currentRow["SubjAnim"] = "org";
                            first = False;
                    elif (subjectHead["LEMMA"] in ["I","you","we","he","she","it","me","him","her","us"]):
                        currentRow["SubjAnim"] = "human";
                    elif isHyponym(subjectHead["LEMMA"],synset_machine):
                        currentRow["SubjAnim"] = "machine";
                    elif isHyponym(subjectHead["LEMMA"],synset_vehicle):
                        currentRow["SubjAnim"] = "vehicle";
                    elif isHyponym(subjectHead["LEMMA"],synset_org):
                        currentRow["SubjAnim"] = "org";
                    elif isHyponym(subjectHead["LEMMA"],synset_human) | isHyponym(subjectHead["LEMMA"],synset_person):
                        currentRow["SubjAnim"] = "human";
                    elif isHyponym(subjectHead["LEMMA"],synset_animal)| isHyponym(subjectHead["LEMMA"],synset_bacterium) | isHyponym(subjectHead["LEMMA"],synset_virus):
                        currentRow["SubjAnim"] = "animal";
                    elif isHyponym(subjectHead["LEMMA"],synset_time) | isHyponym(subjectHead["LEMMA"],synset_period):
                        currentRow["SubjAnim"] = "time";
                    elif isHyponym(subjectHead["LEMMA"],synset_loc):
                        currentRow["SubjAnim"] = "loc";
                    elif isHyponym(subjectHead["LEMMA"],synset_conc):
                        currentRow["SubjAnim"] = "conc";
                    elif isHyponym(subjectHead["LEMMA"],synset_nonconc):
                        currentRow["SubjAnim"] = "nonconc";
                        
                    
                currObject = getDependents(i+1,"iobj",sentence)
                if len(currObject) == 0:
                    currObject = getDependents(i+1,"obj",sentence)
                    currentRow["OvertIObj"] = 1;
                    
                    
                if len(currObject) != 0:
                    currentRow["OvertObj"] = currObject[0]['phrase'];
                    currObject = currObject[0];
                    if currentRow["OvertIObj"] != 1:
                        currentRow["OvertIObj"] = 0;
                else:
                    currObject = "NOOBJ";

                if currObject != "NOOBJ":
                    print(currentRow["OvertObj"])
                    objectHead = getHead(i+1,currObject);
                    objectFeats = getInfoFromFeats(objectHead['FEATS']);
                    currentRow["SubjSylCo"] = sylCount(currentRow["OvertObj"]);
                    currentRow["ObjMorph"] = objectHead["XPOS"];
                    
                    currentRow["OvertObjHead"] = objectHead["FORM"];
                    
                    if currentRow["ObjMorph"] in ["NN","NNS"]:
                        currentRow["ObjSynType"] = "full";
                    elif currentRow["ObjMorph"] in ["NNP","NNPS"]:
                        currentRow["ObjSynType"] = "proper";
                    elif currentRow["ObjMorph"] in ["DT"]:
                        if getValueFromInfo(objectFeats, "Prontype") == "Dem":
                            currentRow["ObjSynType"] = "dem";
                        else:
                            currentRow["ObjSynType"] = "det";
                        currentRow["ObjSynType"] = "det";
                    elif currentRow["ObjMorph"] in ["EX"]:
                        currentRow["ObjSynType"] = "there";
                    elif currentRow["ObjMorph"] in ["WP","WDT"]:
                        currentRow["ObjSynType"] = "wh";
                    elif currentRow["ObjMorph"] in ["PRP"]:
                        if currentRow["OvertObj"] in ["mine","yours","his",
                                     "hers","its","ours","theirs"]:
                            currentRow["ObjSynType"] = "poss";                            
                        else:
                            currentRow["ObjSynType"] = "pron";
                    elif currentRow["ObjMorph"] == "CD":
                        currentRow["ObjSynType"] = "num";
                    elif currentRow["ObjMorph"] == "/":
                        currentRow["ObjSynType"] = "/";
                        print("Yay!")
                    else:
                        currentRow["ObjSynType"] = "NMN!";
                        
                    print(currentRow["ObjSynType"])

                    #Number
                    currentRow["ObjNum"] = getValueFromInfo(objectFeats, "Number");
                    if currentRow["ObjNum"] =="/":
                        if objectHead["XPOS"] == "CD":
                            if objectHead["LEMMA"] == "one":
                                currentRow["ObjNum"] = "Sing";
                            else:
                                currentRow["ObjNum"] = "Plur";
                                
                    #Person   
                    currentRow["ObjPers"] = getValueFromInfo(objectFeats, "Person");
                    if (currentRow["ObjPers"] == "/") & (currentRow["OvertObj"] != "/"):
                        currentRow["ObjPers"] = "3"
                        
                    #Definiteness
                    currDeterminers = getDependents(objectHead["ID"],"det",sentence)
                    print(currDeterminers)
                    for determiner in currDeterminers:
                        detHead = getHead(objectHead["ID"],determiner)
                        defInfo = getInfoFromFeats(detHead["FEATS"])
                        print("PRONTYPE",getValueFromInfo(defInfo,"PronType"))
                        if (getValueFromInfo(defInfo,"Definite") == "Def") | (getValueFromInfo(defInfo,"PronType") == "Dem"):
                            currentRow["ObjDef"] = "def";
                        elif getValueFromInfo(defInfo,"Definite") == "Ind":
                            currentRow["ObjDef"] = "indef";
                        
                    if currentRow["ObjDef"] == "/":                        
                        if currentRow["ObjMorph"] in ["PRP","NNP"]:
                            currentRow["ObjDef"] = "def";
                        else:
                            currentRow["ObjDef"] = "indef";                            
                    #Doubt: every and all???
                
                    #Animacy          
                    #We will be working with the animacy tags from 
                    #Zaenen et al. (2004)
                    #human, org, animal, place, time,
                    #concrete, nonconc, mac, veh
                    if currentRow["ObjMorph"] == ["PRP"]:
                        if currentRow["OvertObj"] in ["I","me","he","she","him","her"]:
                            currentRow["ObjAnim"] = "human";
                    elif currentRow["ObjMorph"] in ["NNP","NNPS"]:
                        nertree = ne_chunk(pos_tag(word_tokenize(sentence["phrase"])));                      
                        first = True;
                        for subtree in nertree.subtrees():                                
                            if first == False:
                                containsHead = False;
                                for word in subtree:
                                    if word[1] == objectHead["FORM"]:
                                        containsHead = True;
                                if containsHead:                                        
                                    if subtree.label() == "PERSON":
                                        currentRow["ObjAnim"] = "human";
                                    elif subtree.label() == "GPE":
                                        currentRow["ObjAnim"] = "loc";
                                    elif subtree.label() == "ORGANIZATION":
                                        currentRow["ObjAnim"] = "org";
                                    elif subtree.label() == "PERSON":
                                        currentRow["ObjAnim"] = "org";
                            first = False;
                    elif (objectHead["LEMMA"] in ["I","you","we","he","she","it","me","him","her","us"]):
                        currentRow["ObjAnim"] = "human";
                    elif isHyponym(objectHead["LEMMA"],synset_machine):
                        currentRow["ObjAnim"] = "machine";
                    elif isHyponym(objectHead["LEMMA"],synset_vehicle):
                        currentRow["ObjAnim"] = "vehicle";
                    elif isHyponym(objectHead["LEMMA"],synset_org):
                        currentRow["ObjAnim"] = "org";
                    elif isHyponym(objectHead["LEMMA"],synset_human) | isHyponym(objectHead["LEMMA"],synset_person):
                        currentRow["ObjAnim"] = "human";
                    elif isHyponym(objectHead["LEMMA"],synset_animal)| isHyponym(objectHead["LEMMA"],synset_bacterium) | isHyponym(objectHead["LEMMA"],synset_virus):
                        currentRow["ObjAnim"] = "animal";
                    elif isHyponym(objectHead["LEMMA"],synset_time) | isHyponym(objectHead["LEMMA"],synset_period):
                        currentRow["ObjAnim"] = "time";
                    elif isHyponym(objectHead["LEMMA"],synset_loc):
                        currentRow["ObjAnim"] = "loc";
                    elif isHyponym(objectHead["LEMMA"],synset_conc):
                        currentRow["ObjAnim"] = "conc";
                    elif isHyponym(objectHead["LEMMA"],synset_nonconc):
                        currentRow["ObjAnim"] = "nonconc";
                        
                    #Exceptions
                    if objectHead["LEMMA"] in nomSemExceptions: 
                        currentRow["ObjAnim"] = "NMA!";
                        
            #And now the arguments
            currentClauseID += 1;
            j += 1;
            clauseTable = clauseTable.append(currentRow,ignore_index=True)
                                            # ,sort=False)
        
print(clauseTable)

clauseTable.to_csv(path_or_buf="jul19table.csv")


#NMA! = Needs Manual Attention!
#We deal with NMA stuff semi-automatically now.

#TODO: Check Tenseless items if they're really tenseless -
#in particular would, could, should etc