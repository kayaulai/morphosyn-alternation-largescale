# -*- coding: utf-8 -*-
#TODO
#-Obliques other than the 1st
#-Finitenes
#-More fixing with relatives
#Expletive subject -> NOT passivisable


#Run this whenever restarting.
import os;
os.chdir("G:\我的雲端硬碟\corpus priming\eng")
import warnings;
#os.chdir("G:\My Drive\corpus priming\eng")
import pandas;
import re;
import shelve;
import numpy as np;
from wordfreq import word_frequency;
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
    while i <= df.shape[0]:
        depString = df.loc[df['ID'] == i,df.columns.values=="DEPS"];
        if depString.shape[0] != 0:
            depString = depString.iloc[0,0];
            depString = depString.split("|");
            matchFound = False;
            for pair in depString:
                if re.match(str(headID)+":",pair):
                    matchFound = True;
            if matchFound & (i not in skip):
               newPhrase = unifyPhrases(newPhrase, getPhrase(i,phrase,[headID]+skip));
                
        i += 1;
    newPhrase['phraseDF'] = newPhrase['phraseDF'].sort_values(by=['ID'])
    newPhrase['phrase'] = ' '.join(newPhrase['phraseDF']['FORM'].values.tolist());
    
    return newPhrase;
    
def splitDepString(depString):
    deps = depString.split("|");
    result = [];
    for dep in deps:
        pair = dep.split(":");
        result.append((pair[0],pair[1]));    
    return result;

def searchDepStrings(depStrings, ID = 0, relation = ""):
    if ID == 0 and relation == "": return("ERROR");
    elif ID == 0 and relation != "":
        for item in depStrings:
            if item[1] == relation: result = item; break;
    elif ID != 0 and relation == "":
        for item in depStrings:
            if item[0] == ID: result = item; break;
    else:
        for item in depStrings:
            if item[1] == relation and item[0] == ID: result = item; break;
    return(item);
        
        
        
#Test the function
print(getPhrase(19,allSents[0]));

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

def getDependents(headID, relation, phrase, depOnly = True, conjs = False):
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
        if not depOnly:
            if (str(headID) == df["HEAD"].iloc[i-1]) & (relation == df["DEPREL"].iloc[i-1]): matchFound = True;
        if matchFound:
            newPhrases.append(getPhrase(i, phrase));
        i += 1;
    
    if conjs:
        mainConjID = getMainConjID(headID, phrase);
        if mainConjID != "/":
            dep = str(mainConjID) + ":" + relation;
            i = 1;
            while i <= df.shape[0]:
                searchString = df["DEPS"].iloc[i-1];
                searchString = searchString.split("|");
                matchFound = False;
                for pair in searchString:
                    if re.match(dep,pair):
                        matchFound = True;
                if not depOnly:
                    if (str(headID) == df["HEAD"].iloc[i-1]) & (relation == df["DEPREL"].iloc[i-1]): matchFound = True;
                if matchFound:
                    newPhrases.append(getPhrase(i, phrase));
                i += 1;
        return [newPhrases, mainConjID]
    else:
        return newPhrases;
    


def getMainConjID(headID, phrase):
    df = phrase["phraseDF"];
    headWord = df[df["ID"]==headID];
    depStringSplit = splitDepString(headWord["DEPS"][0]);
    mainConjID = "/";
    for item in depStringSplit:
        if item[1] == "conj":
            mainConjID = int(item[0]);
            break;
    return(mainConjID);
    

def getDependentsFromDeprel(headID, relation, phrase):
    df = phrase['phraseDF'];
    i = 1;
    newPhrases = [];
    while i <= df.shape[0]:
        if (df["HEAD"].iloc[i-1] == str(headID)) & (df["DEPREL"].iloc[i-1] == relation):
            newPhrases.append(getPhrase(i, phrase));
        i += 1;
            
    return newPhrases;

def getDependentIDsFromDeprel(headID, relation, phrase):
    df = phrase['phraseDF'];
    i = 1;
    ids = [];
    while i <= df.shape[0]:
        if (df["HEAD"].iloc[i-1] == str(headID)) & (df["DEPREL"].iloc[i-1] == relation):
            ids.append(i);
        i += 1;
            
    return ids;

#Test the function
getDependents(7,"obj",allSents[0])

#This guy returns a row rather than a full phrase object
#At least for now

def getParents(childID, phrase, relation = "any"):
    df = phrase['phraseDF'];
    depString = df["DEPS"].iloc[childID-1];
    depString = depString.split("|");
    
    parents = [None] * len(depString);
    i = 0
    for string in depString:
        string = string.split(":");
        if (relation == "any") or (string[1] == relation):
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

#parentID is the ID of the parent
#phrase is the phrase containing the head, not the full sentence
#returns rows not entire tables
#if only one head expected, use [0]

def getHeads(parentID, phrase, RC = False):
    i = 0;
    df = phrase['phraseDF'];
    heads = [];
    if not RC:
        while i < df.shape[0]:
            if re.search("\|"+str(parentID)+":",df.iloc[i,]["DEPS"]) or re.search("^"+str(parentID)+":",df.iloc[i,]["DEPS"]) or (df.iloc[i,]["HEAD"] == str(parentID)):
                heads.append(df.iloc[i,]);
            i += 1;
    
    i = 0;
    if RC or heads == []:
        print(df["ID"].tolist())
        while i < df.shape[0]:
            if int(df.iloc[i,]["HEAD"]) not in df["ID"].tolist():
                print(int(df.iloc[i,]["HEAD"]))
                heads.append(df.iloc[i,]);
            i += 1;
        if len(heads) > 1: print("RC argument no. of heads possibly exceeds 1! Please check getHeads code.");
    if heads == []:
        heads == [None];
        print("heynohead");
        #Try to find the head anyway
        
    return(heads);
    
def combineNPFeatures(fullNPfeatures, coreffeatures, prefix):
    finalBundle = dict();
    finalBundle[prefix + "Freq"] = coreffeatures[prefix + "Freq"];
    finalBundle[prefix + "Def"] = "def";
    finalBundle[prefix + "Head"] = coreffeatures[prefix + "Head"];
    finalBundle[prefix + "Morph"] = coreffeatures[prefix + "Morph"];
    finalBundle[prefix + "Pers"] = fullNPfeatures[prefix + "Pers"];
    finalBundle[prefix + "Anim"] = fullNPfeatures[prefix + "Anim"];
    finalBundle[prefix + "Num"] = fullNPfeatures[prefix + "Num"];
    finalBundle[prefix + "SynType"] = coreffeatures[prefix + "SynType"];
    return(finalBundle)
        


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
        if synset_hypernym in synset_hyponym.hypernyms() + synset_hyponym.instance_hypernyms():
            answer = True;
            break;
        elif isHyponym(synset_hyponym.hypernyms() + synset_hyponym.instance_hypernyms(),synset_hypernym):
            answer = True;
            break;
    if proper == False:
        for synset_hyponym in synsets_hyponym:
            if synset_hyponym == synset_hypernym:
                answer = True;
    return answer;

def extractRefRelationFromList(parentID, phraseList):
    ids = [];
    deps = [];
    refs = [];
    for phrase in phraseList:
        currHead = getHeads(parentID, phrase, RC = True)[0]; #Just the first conjunt should suffice
        ids.append(currHead["ID"]);
        currDeps = currHead["DEPS"].split("|");
        deps.append(currDeps);
        refs.append("/")
        for dep in currDeps:
            if re.search(":ref",dep):
                reObj = re.search(":ref",dep)
                refs[len(refs)-1] = dep[0:reObj.span(0)[0]];
    
    rels = [];
    for i in np.arange(0,len(refs),1):
        if refs[i] != "/":
            j = 0;
            for head in ids:
                if refs[i] == str(head):
                   rels.append((j,i));
                j = j + 1;

    return(rels);


def getNomSem(npHead, phrase, ner = False):
    #Word - subjectHead["FORM"], lemma - lemma, sentence - sentence["phrase"]
    #ner = currentRow["SubjMorph"] in ["NNP","NNPS"]
    
    anim = "/";
    word = npHead["FORM"];
    lemma = npHead["LEMMA"];
    headID = npHead["ID"];
    sentence = phrase['phrase']
    
    if ner:
        nertree = ne_chunk(pos_tag(word_tokenize(sentence)));                      
        first = True;
        for subtree in nertree.subtrees():   
            if first == False:
                containsHead = False;
                for word in subtree:
                    if word[1] == word:
                        containsHead = True;
                if containsHead:                                        
                    if subtree.label() == "PERSON":
                        anim = "human";
                    elif subtree.label() == "GPE":
                        anim = "loc";
                    elif subtree.label() == "ORGANIZATION":
                        anim = "org";
                    elif subtree.label() == "PERSON":
                        anim = "org";
            first = False;
    elif (lemma in ["I","you","we","he","she","me","him","her","us"]):
        anim = "human";
    elif isHyponym(lemma,synset_machine):
        anim = "machine";
    elif isHyponym(lemma,synset_vehicle):
        anim = "vehicle";
    elif isHyponym(lemma,synset_org):
        anim = "org";
    elif isHyponym(lemma,synset_human) | isHyponym(lemma,synset_person):
        anim = "human";
    elif isHyponym(lemma,synset_animal)| isHyponym(lemma,synset_bacterium) | isHyponym(lemma,synset_virus):
        anim = "animal";
    elif isHyponym(lemma,synset_time) | isHyponym(lemma,synset_period):
        anim = "time";
    elif isHyponym(lemma,synset_loc):
        anim = "loc";
    elif isHyponym(lemma,synset_country):
        anim = "org";
    elif isHyponym(lemma,synset_conc):
        anim = "conc";
    elif isHyponym(lemma,synset_nonconc):
        anim = "nonconc";
        
    
    appos = getDependents(headID, "appos", phrase)
    if (anim == "/") & (len(appos) > 0):
        appos = appos[0];
        apposHead = getHeads(npHead["ID"], phrase)[0];
        anim = getNomSem(apposHead, phrase, apposHead["XPOS"] in ["NNP","NNPS"]);        
    return anim;


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
synset_country = wn.synsets('country')[1];

synset_period = wn.synsets('period')[4];
synset_virus = wn.synsets('virus')[0];
synset_bacterium = wn.synsets('bacterium')[0];
synset_building = wn.synsets('building')[0];

# Converting raw data (a database of sentences)
# to a database of clauses (which I want for my project),
# first step: Put each clause in one row and extract
# directly expressed arguments
nomSemExceptions = ["member"]


#Accepting multiple values: SubjHead, SubjFreq, SubjDef, SubjAnim, SubjSynType, SubjMorph
#Combined: SubjSylCo, SubjNum, SubjPers

def constituentUnique(headID, constituents):
    ids = [];
    retain = [];
    newConstituentList = constituents;
    for constituent in constituents:
        currHeadID = getHeads(headID, constituent)[0]["ID"];
        if currHeadID in ids:
            retain.append(False);
        else:
            ids.append(currHeadID);
            retain.append(True);
    for i in np.arange(len(constituents)-1,-1,-1):
        if retain[i] == False:            
            newConstituentList.pop(i); 
            
    return newConstituentList;

def extractNPHeadProperties(heads, prefix = "", case = False):
     #This is divided into two sub-modules
     #The first part is extracting the features
     #The second part is combining them
     
     outputProps = dict()
     
     #Part I: Extraction
     
     headPropsSep = [];
     
     for head in heads:
        feats = getInfoFromFeats(head['FEATS']);
        currProps = dict()
        
        #Note: The first line of each property must start with an
        #unconditional assignment to ensure that key errors won't
        #occur later on
        
        #Morphology
        currProps["Morph"] = head["XPOS"];
        
        #Head
        currProps["Head"] = head["FORM"];
        
        #pos
        currProps["PosID"] = head["ID"];
        
        
        #POS
        currProps["XPOS"] = "/";
        if head["XPOS"] in ["NN","NNS"]:
            currProps["SynType"] = "full";
        elif head["XPOS"] in ["NNP","NNPS"]:
            currProps["SynType"] = "proper";
        elif head["XPOS"] in ["DT"]:
            if getValueFromInfo(feats, "Prontype") == "Dem":
                currProps["SynType"] = "dem";
            else:
                currProps["SynType"] = "det";
            currProps["SynType"] = "det";
        elif head["XPOS"] in ["EX"]:
            currProps["SynType"] = "there";
        elif head["XPOS"] in ["WP","WDT"]:
            currProps["SynType"] = "wh";
        elif head["XPOS"] in ["PRP"]:
            if currProps["Head"] in ["mine","yours","his",
                         "hers","its","ours","theirs"]:
                currProps["SynType"] = "poss";                            
            else:
                currProps["SynType"] = "pron";
        elif head["XPOS"] == "CD":
            currProps["SynType"] = "num";
        elif head["XPOS"] == "/":
            currProps["SynType"] = "/";
        else:
            currProps["SynType"] = "NMA!";
            
        #Frequency
        currProps["Freq"] = word_frequency(head["FORM"], 'en')
        
        #Number
        currProps["Num"] = getValueFromInfo(feats, "Number");
        if currProps["Num"] =="/":
            if head["XPOS"] == "CD":
                if head["LEMMA"] == "one":
                    currProps["Num"] = "Sing";
                else:
                    currProps["Num"] = "Plur";
                    
        #Person   
        currProps["Pers"] = getValueFromInfo(feats, "Person");
        if (currProps["Pers"] == "/"):
            currProps["Pers"] = "3"
            
        #Definiteness        
        currProps["Def"] = "/";
        currDeterminers = getDependents(head["ID"],"det",sentence)
        for determiner in currDeterminers:
            detHead = getHeads(head["ID"],determiner)[0]
            defInfo = getInfoFromFeats(detHead["FEATS"])
            if (getValueFromInfo(defInfo,"Definite") == "Def") | (getValueFromInfo(defInfo,"PronType") == "Dem"):
                currProps["Def"] = "def";
            elif getValueFromInfo(defInfo,"Definite") == "Ind":
                currProps["Def"] = "indef";
            
        if currProps["Def"] == "/":                        
            if head["XPOS"] in ["PRP","NNP"]:
                currProps["Def"] = "def";
            else:
                currProps["Def"] = "indef";                            
        #Doubt: every and all???
        
        #Case
        if case:
            currCases = getDependents(head["ID"],"case",sentence,conjs=True); #We need to deal with e.g. by John and Mary
            if len(currCases[0]) == 1 and currCases[1] != "/":
                currProps["Case"]  = getHeads(currCases[1],currCases[0][0])[0]["FORM"];
            elif len(currCases[0]) > 0:
                print(head["ID"])
                currProps["Case"]  = getHeads(head["ID"],currCases[0][0])[0]["FORM"];
            else:
                currProps["Case"] = "/";
                
    
        #Animacy          
        #We will be working with the animacy tags from 
        #Zaenen et al. (2004)
        #human, org, animal, place, time,
        #concrete, nonconc, mac, veh
        currProps["Anim"] = getNomSem(head,sentence,
                  head["XPOS"] in ["NNP","NNPS"]);
            
        #Exceptions
        if head["LEMMA"] in nomSemExceptions: 
            currProps["Anim"] = "NMA!";
            
        headPropsSep.append(currProps)
    
     #Part II: Combination
     multValProps = ["Head", "Freq", "Def", "Anim",
                        "SynType", "Morph","PosID"];
     if case: multValProps = multValProps + ["Case"];
     for prop in multValProps:
         currString = "";
         for headProps in headPropsSep:
             currString = currString + str(headProps[prop]) + ";;";
         currString = currString[0:(len(currString)-2)];
         outputProps[prefix + prop] = currString;
        
     #SubjNum
     if len(heads) > 1:
         outputProps[prefix + "Num"] = "Plur";
     else:
         outputProps[prefix + "Num"] = headPropsSep[0]["Num"];
         
     #SubjPers
     persons = []
     for headProps in headPropsSep:
         persons.append(headPropsSep[0]["Pers"]);
     if "1" in persons:
         outputProps[prefix + "Pers"] = "1";
     elif "2" in persons:
         outputProps[prefix + "Pers"] = "2";
     else:
         outputProps[prefix + "Pers"] = "3";
         
    
         
     return(outputProps)

def extractPredProperties(predID, phrase, prefix):
    props = dict();
    df = phrase["phraseDF"];
    props[prefix + "Form"] = df.iloc[predID,]["FORM"];
    props[prefix + "Lemma"] = df.iloc[predID,]["LEMMA"];
    props[prefix + "Morph"] = df.iloc[predID,]["XPOS"];
    
    auxiliaries = getDependents(predID+1,"aux",sentence);
    auxLemmas = [None] * len(auxiliaries);
    
    particles = getDependents(predID+1,"compound:prt",sentence);
    for part in particles:
        props[prefix + "Lemma"] = props[prefix + "Lemma"] + " " + getHeads(predID+1,part)[0]["FORM"];
        props[prefix + "Form"] = props[prefix + "Form"] + " " + getHeads(predID+1,part)[0]["FORM"];
    
    #Fill up auxiliaries
    k = len(auxiliaries);
    for aux in auxiliaries:
        auxHead = getHeads(predID+1,aux)[0];
        auxLemmas[k-1] = auxHead["LEMMA"];
        auxInfo = getInfoFromFeats(auxHead["FEATS"]);
        props["Aux" + str(k)] = auxHead["FORM"];
        if k == 1:
            break;
        k = k - 1;
        
    featsInfo = getInfoFromFeats(df.iloc[predID,]["FEATS"])
    if "Voice" in featsInfo:
        props["Voice"] = featsInfo["Voice"];
        props["PassAux"] = auxLemmas[0];
    elif len(auxiliaries) == 0 and getValueFromInfo(featsInfo,"VerbForm") == "Part" and getValueFromInfo(featsInfo,"Tense") == "Past":
        #Things like 'the teacher killed by the doctor...' not considered RCs by UD for some reason
        props["Voice"] = "Pass";        
    else:
        props["Voice"] = "Act";
    
    props[prefix + "Fin"] = "Nonfin";
    if getValueFromInfo(featsInfo,"VerbForm") == "Fin":
        props[prefix + "Fin"] = "Fin";
    elif props["Voice"] == "Pass":
        if len(auxiliaries) > 0:
            passAuxInfo = getInfoFromFeats(getHeads(predID+1,auxiliaries[0])[0]["FEATS"]);
            if getValueFromInfo(passAuxInfo,"VerbForm") == "Fin":
                props[prefix + "Fin"] = "Fin";
        elif len(auxiliaries) == 0:
            props[prefix + "Fin"] = "Fin";             
    elif len(auxiliaries) != 0:
        print("hi");
        auxHeadZero = getHeads(predID+1,auxiliaries[0])[0]
        if getValueFromInfo(getInfoFromFeats(auxHeadZero["FEATS"]),"VerbForm") == "Fin":
            props[prefix + "Fin"] = "Fin";
                
            
        
        
    #Syllable count. Simple!
    props[prefix + "SylCo"] = sylCount(df.iloc[predID,]["FORM"]);
    
    #Verb freq
    props[prefix + "Freq"] = word_frequency(props[prefix + "Form"], 'en')

        
    #More abstract grammatical categories
    props[prefix + "Tense"] = "/"
    if getValueFromInfo(featsInfo,"VerbForm") == "Fin":
        props[prefix + "Tense"] = getValueFromInfo(featsInfo,"Tense");
    else:
        for aux in auxiliaries:
            auxHead = getHeads(predID+1,aux)[0];
            auxInfo = getInfoFromFeats(auxHead["FEATS"]);
            if "VerbForm" in auxInfo:
                if auxInfo["VerbForm"] == "Fin":
                    if "Tense" in auxInfo:
                        props[prefix + "Tense"] = auxInfo["Tense"];
                        break;
                    elif auxHead["FORM"] == "will":
                        props[prefix + "Tense"] = "Fut";
                        break;
            elif auxHead["FORM"] == "better":
                props[prefix + "Tense"] = "Past"; 
    if props[prefix + "Tense"] == "/":
        props[prefix + "Tense"] = "Tenseless";
    #Determine aspect from the auxiliaries
    props[prefix + "Aspect"] = "/"
    print(getValueFromInfo(featsInfo,"VerbForm"))
    if len(auxiliaries) > 0:
        if (props["Aux1"] == "being") | (props["Aux1"] == "getting"):
            props[prefix + "Aspect"] = "Prog";
    if (getValueFromInfo(featsInfo,"VerbForm") == "Part" and getValueFromInfo(featsInfo,"VerbForm") == "Pres") or getValueFromInfo(featsInfo,"VerbForm") == "Ger":
        props[prefix + "Aspect"] = "Prog";
        
    for aux in auxLemmas:
        if aux == "have":
            if props[prefix + "Aspect"] == "Prog":
                props[prefix + "Aspect"] = "PerfProg";
            else:
                props[prefix + "Aspect"] = "Perf";
        break;
    
    if props[prefix + "Aspect"] == "/":
        props[prefix + "Aspect"] = "Simple";
        
    return(props);


nomSemExceptions = ["member"]



#allSentsR = allSents[(860+579+7940+307+229+12+3929):] #R stands for reduced and is for testing purposes
allSentsR = allSents[1:2]
#allSentsR = allSents[0:1350]

#TODO: Embedding depth, idiomaticity, 
clauseTableColnames = ["ClauseID","Doc","SentID","SentForm","ClauseForm","PredType",
                            "PredForm","PredLemma","PredMorph","VMorphForm","PredTense",
                            "PredAspect","Voice","PredSylCo","PredFreq","PredFin","Aux3","Aux2","Aux1",
                            "PassAux","OvertSubj","CovertSubj","SubjRef",
                            "SubjHead","SubjFreq","SubjDef","SubjNum",
                            "SubjPers","SubjAnim","SubjSynType",
                            "SubjSylCo","SubjMorph","SubjPosID","OvertObj","OvertIObj","CovertObj","ObjFreq",
                            "ObjHead","ObjDef","ObjNum","ObjPers","ObjAnim",
                            "ObjSylCo","ObjMorph","ObjSynType","ObjPosID","OvertObl",
                            "OblCase","OblHead","OblFreq","OblDef","OblNum",
                            "OblPers","OblAnim","OblSylCo","OblMorph","OblSynType","OblPosID",
                            "Obl2","Obl3","Obl2PosID","Obl3PosID","NonAlternable"];
                       

print(getParents(24, allSents[14], "conj"));

clauseTable = pandas.DataFrame(columns=clauseTableColnames);

#Accepting multiple values: SubjHead, SubjFreq, SubjDef, SubjAnim, SubjSynType, SubjMorph
#Combined: SubjSylCo, SubjNum, SubjPers


#This is the clause ID WITHIN DOCUMENTS.
#We'll add an overall ID at the end. No need to sweat it.
currentClauseID = 1;
currSentID = 0;
for sentence in allSentsR:
    print("Doing sentence: ",currSentID);
    i = 1;
    df = sentence['phraseDF'];
    preds = [];
    predTypes = [];
    
    
    #'Manual' changes to what I believe are errors in the original annotations
    if sentence['doc'] == "weblog-blogspot.com_dakbangla_20050311135387_ENG_20050311_135387" and sentence['sentID'] == 89:
        sentence['phraseDF'].iloc[2,8] = '17:nsubj';
    elif sentence['doc'] == "answers-20111108110012AAK8Azy_ans" and sentence['sentID'] == 31:
        sentence['phraseDF'].iloc[9,3] = 'NOUN';
    elif sentence['doc'] == "answers-20111108110012AAK8Azy_ans" and sentence['sentID'] == 32:
        sentence['phraseDF'].iloc[13,3] = 'NOUN';
    elif sentence['doc'] == "reviews-348247" and sentence['sentID'] == 6:
        sentence['phraseDF'].iloc[10,3] = 'NOUN';
    elif sentence['doc'] == "reviews-159485" and sentence['sentID'] == 0:
        sentence['phraseDF'].iloc[39,3] = 'NOUN';
    elif sentence['doc'] == "answers-20111108084036AAh8Ws9_ans" and sentence['sentID'] == 0:
        sentence['phraseDF'].iloc[2,3] = 'NOUN';
    
    
    while i <= df.shape[0]:
        ##(df.iloc[i-1,]["XPOS"])
        if df.iloc[i-1,]["UPOS"] == "VERB":
            preds.append(i-1);
            predTypes.append("V");
                
        if df.iloc[i-1,]["UPOS"] == "ADJ":
            #print("ADJ",len(getDependents(i, "aux", sentence)))
            if len(getDependents(i, "cop", sentence)) > 0:
                preds.append(i-1);
                predTypes.append("A");
            
        if df.iloc[i-1,]["UPOS"] in ["NOUN", "NUM", "PRON", "PROPN"]:
            if len(getDependents(i, "cop", sentence)) > 0:
                preds.append(i-1);
                predTypes.append("N");
            
        i += 1;
    j = 0;
    for i in preds:
            #Initialise the row first.
            currentRow = dict()
            for col in clauseTableColnames:
                currentRow[col] = "/"
                
            currRowPhrase = getPhrase(i+1,sentence);
            currentRow["ClauseForm"] = currRowPhrase['phrase'];
            
            #Fill in doc-level, sentence-level info and clause ID.
            currentRow["ClauseID"] = currentClauseID;
            currentRow["Doc"] = sentence['doc'];
            currentRow["SentID"] = sentence['sentID'];
            currentRow["SentForm"] = sentence['phrase'];
            #Now for clause-level stuff. First the V/A.
            featsInfo = getInfoFromFeats(df.iloc[i,]["FEATS"])
            
            currentRow["PredType"] = predTypes[j];
            predProps = extractPredProperties(i,sentence,"Pred")
            for prop in predProps:
                currentRow[prop] = predProps[prop];                

            if predTypes[j] == "V":        
                currentRow["VMorphForm"] = featsInfo["VerbForm"];
            if getValueFromInfo(featsInfo,"Mood") == "Imp":
                currentRow["NonAlternable"] = True;
                
            #Arguments
            #Subject first.
            currSubject = "NOSUBJ";
            
            currSubjectCands = getDependents(i+1,"nsubj",sentence);
            currSubjectCands = getDependents(i+1,"csubj",sentence);
            currSubjectCands = currSubjectCands + getDependents(i+1, "nsubj:pass",sentence);
            currSubjectCands = currSubjectCands + getDependentsFromDeprel(i+1,"nsubj",sentence);
            currSubjectCands = currSubjectCands + getDependentsFromDeprel(i+1,"nsubj:pass",sentence);
            
            currCovertSubject = None;
            RC = False;
            RCconj = False;
            if re.search("acl:relcl",df.iloc[i,]["DEPREL"]): #For relative clauses only
                relations = extractRefRelationFromList(i+1,currSubjectCands)
                if  len(relations) == 0:
                    if len(currSubjectCands)>0:
                        currSubject = currSubjectCands[0];
                else:
                    if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                    relations = relations[0];
                    currSubject = currSubjectCands[relations[1]];
                    currCovertSubject = currSubjectCands[relations[0]];
                    RC = True;
            elif re.search("acl:relcl",df.iloc[i,]["DEPS"]): #RCs that aren't the first conjunct
                primConjunct = getParents(i+1, sentence, "conj")[0];
                primConjunctID = primConjunct["ID"];
                currSubjectCands = currSubjectCands + getDependentsFromDeprel(primConjunctID, "nsubj",sentence);
                currSubjectCands = currSubjectCands + getDependentsFromDeprel(primConjunctID, "nsubj:pass",sentence);
                relations = extractRefRelationFromList(primConjunct["ID"],currSubjectCands)
                if  len(relations) == 0:
                    if len(currSubjectCands)>0:
                        currSubject = currSubjectCands[0];
                else:
                    if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                    relations = relations[0];
                    currSubject = currSubjectCands[relations[1]];
                    currCovertSubject = currSubjectCands[relations[0]]; 
                    RCconj = True;
            elif len(currSubjectCands)>0:
                currSubject = currSubjectCands[0];
            elif getValueFromInfo(featsInfo,"VerbForm") == "Ger" and re.search("acl",df.iloc[i,]["DEPS"]):
                print("GP",getParents(i+1, sentence, relation = "acl"));
                headNoun = getParents(i+1, sentence, relation = "acl")[0];
                
                skips = [i + 1];
                if re.search("acl",df.iloc[i,]["DEPREL"]):
                    skips = skips + getDependentIDsFromDeprel(i+1,"conj",sentence);
                else:
                    skips = skips + [getMainConjID(i+1, sentence)];
                skips = skips 
                currSubjectCands = currSubjectCands + [getPhrase(headNoun["ID"],sentence, skips)];
                currSubject = currSubjectCands[0]
            else:
                currSubject = "NOSUBJ";
                
            
            if currSubject != "NOSUBJ":
                #Features that don't take heads into account
                currentRow["OvertSubj"] = currSubject['phrase'];
                if currCovertSubject:
                    currentRow["CovertSubj"] = currCovertSubject['phrase'];                        
                currentRow["SubjSylCo"] = sylCount(currentRow["OvertSubj"]);
                
                #Features that do take head into account
                if RCconj:
                    subjectHeads = getHeads(primConjunctID,currSubject);
                else:
                    subjectHeads = getHeads(i+1,currSubject);
                subjHeadProps = extractNPHeadProperties(subjectHeads,"Subj");
                if currCovertSubject:
                    covertSubjectHeads = getHeads(i+1,currCovertSubject,RC=True);
                    covertSubjHeadProps = extractNPHeadProperties(covertSubjectHeads,"Subj");
                    subjHeadProps = combineNPFeatures(covertSubjHeadProps,subjHeadProps,"Subj")
                
                for prop in subjHeadProps:
                    currentRow[prop] = subjHeadProps[prop];                
            
            if predTypes[j] == "V":
                #Objects and obliques are for verbs only.
                RC = False;
                RCconj = False;
                currCovertObject = None;
                
                currObject = "NOOBJ";
                currObjectCands = getDependents(i+1,"iobj",sentence,False);
                currObjectCands = currObjectCands + getDependentsFromDeprel(i+1,"iobj",sentence);
                if len(currObjectCands) == 0:
                    currObjectCands = getDependents(i+1,"obj",sentence);
                    currObjectCands = currObjectCands + getDependentsFromDeprel(i+1,"obj",sentence);
                    currentRow["OvertIObj"] = 0;
                else:
                    currentRow["OvertIObj"] = 1;
             
                if re.search("acl:relcl",df.iloc[i,]["DEPS"]): #For relative clauses only
                    deps = splitDepString(df.iloc[i,]["DEPS"]);
                    headnounID = searchDepStrings(deps, relation = "acl:relcl")[0];
                    headnounDeprels = splitDepString(df.iloc[int(headnounID)-1,]["DEPS"]); #Potential bugs here
                    headnounDeprelsRelation = searchDepStrings(headnounDeprels, ID = i+1)[1];
                    if headnounDeprelsRelation in ["obj", "iobj"]: #Only object relative clauses, not subject ou quelque chose du genre
                        if re.search("acl:relcl",df.iloc[i,]["DEPREL"]):
                            relations = extractRefRelationFromList(i+1,currObjectCands)
                            if  len(relations) == 0:
                                if len(currObjectCands)>0:
                                    currObject = currObjectCands[0];
                            else:
                                if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                                relations = relations[0];
                                currObject = currObjectCands[relations[1]];
                                currCovertObject = currObjectCands[relations[0]];
                                RC = True;
                        else: #RCs that aren't the first conjunct
                            primConjunct = getParents(i+1, sentence, "conj")[0];
                            primConjunctID = primConjunct["ID"];
                            currObjectCands = currObjectCands + getDependentsFromDeprel(primConjunctID, "nobj",sentence);
                            currObjectCands = currObjectCands + getDependentsFromDeprel(primConjunctID, "nobj:pass",sentence);
                            print("ATTENTION",primConjunct["ID"],currObjectCands);
                            relations = extractRefRelationFromList(primConjunct["ID"],currObjectCands);
                            if  len(relations) == 0:
                                if len(currObjectCands)>0:
                                    currObject = currObjectCands[0];
                            else:
                                if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                                relations = relations[0];
                                currObject = currObjectCands[relations[1]];
                                currCovertObject = currObjectCands[relations[0]]; 
                                RCconj = True;
                
                if len(currObjectCands)>0:
                    currObject = currObjectCands[0];
                    
                #print("Larger?",len(currObjectCands))
                #print("currObjectC!!",currObjectCands)
                #print(currObject)
                if currObject != "NOOBJ":
                    #Features that don't take heads into account
                    currentRow["OvertObj"] = currObject['phrase'];
                    currentRow["ObjSylCo"] = sylCount(currentRow["OvertObj"]);
                    
                    #Features that do take head into account
                    if RCconj:
                        objectHeads = getHeads(primConjunctID,currObject);
                    else:
                        objectHeads = getHeads(i+1,currObject);
                    objHeadProps = extractNPHeadProperties(objectHeads,"Obj");
                    
                    if currCovertObject:
                        covertObjectHeads = getHeads(i+1,currCovertObject,RC=True);
                        covertObjHeadProps = extractNPHeadProperties(covertObjectHeads,"Obj");
                        objHeadProps = combineNPFeatures(covertObjHeadProps,objHeadProps,"Obj");
                        
                    for prop in objHeadProps:
                        currentRow[prop] = objHeadProps[prop];                
                
                #OBLIQUE!  
                currObliqueCands = getDependents(i+1,"obl",sentence,False);
                currObliqueCands = currObliqueCands + getDependentsFromDeprel(i+1,"obl",sentence);                
                
                
                currOblique = "NOOBL";
                currCovertOblique = None;
                
                currObliqueCands = constituentUnique(i+1, currObliqueCands);
                if (currentRow["Voice"] == "Pass") and (len(currObliqueCands) > 1):
                    currObliqueCands0HeadID = getHeads(i+1, currObliqueCands[0])[0]["ID"];
                    if len(getDependents(currObliqueCands0HeadID,"case",sentence)) > 0:
                        zeroCase = getDependents(currObliqueCands0HeadID,"case",sentence)[0];
                        zeroHead = getHeads(currObliqueCands0HeadID,zeroCase)[0];
                        if zeroHead["FORM"] != "by":
                            i = 1;
                            for cand in currObliqueCands[1:len(currObliqueCands)]:
                                currObliqueHeadID = getHeads(i+1, cand)[0]["ID"];
                                if len(getDependents(currObliqueHeadID,"case",sentence)) > 0:
                                    currCase = getDependents(currObliqueHeadID,"case",sentence)[0];
                                    currHead = getHeads(currObliqueHeadID,currCase)[0];
                                    if currHead["FORM"] == "by":
                                        currObliqueCands[0], currObliqueCands[i] = currObliqueCands[i], currObliqueCands[0];
                                        break;
                                i = i + 1;
                            
                    
                
                
                if re.search("acl:relcl",df.iloc[i,]["DEPREL"]): #For relative clauses only
                    relations = extractRefRelationFromList(i+1,currObliqueCands)
                    if  len(relations) == 0:
                        if len(currObliqueCands)>0:
                            currOblique = currObliqueCands[0];
                    else:
                        if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                        relations = relations[0];
                        currOblique = currObliqueCands[relations[1]];
                        currCovertOblique = currObliqueCands[relations[0]];
                        RC = True;
                elif re.search("acl:relcl",df.iloc[i,]["DEPS"]): #RCs that aren't the first conjunct
                    primConjunct = getParents(i+1, sentence, "conj")[0];
                    primConjunctID = primConjunct["ID"];
                    currObliqueCands = currObliqueCands + getDependentsFromDeprel(primConjunctID, "nobl",sentence);
                    currObliqueCands = currObliqueCands + getDependentsFromDeprel(primConjunctID, "nobl:pass",sentence);
                    relations = extractRefRelationFromList(primConjunct["ID"],currObliqueCands)
                    if  len(relations) == 0:
                        if len(currObliqueCands)>0:
                            currOblique = currObliqueCands[0];
                    else:
                        if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                        relations = relations[0];
                        currOblique = currObliqueCands[relations[1]];
                        currCovertOblique = currObliqueCands[relations[0]]; 
                        RCconj = True;
                elif len(currObliqueCands)>0:
                    currOblique = currObliqueCands[0];
                else:
                    currOblique = "NOOBL";
                    

                if currOblique != "NOOBL":
                    #Features that don't take heads into account
                    currentRow["OvertObl"] = currOblique['phrase'];
                    currentRow["OblSylCo"] = sylCount(currentRow["OvertObl"]);
                    
                    #Features that do take head into account
                    if RCconj:
                        obliqueHeads = getHeads(primConjunctID,currOblique);
                    else:
                        obliqueHeads = getHeads(i+1,currOblique);
                    oblHeadProps = extractNPHeadProperties(obliqueHeads,"Obl",case=True);
                    
                    if currCovertOblique:
                        covertObliqueHeads = getHeads(i+1,currCovertOblique,RC=True);
                        covertOblHeadProps = extractNPHeadProperties(covertObliqueHeads,"Obl");
                        oblHeadProps = combineNPFeatures(covertOblHeadProps,oblHeadProps,"Obl");
                        
                    for prop in oblHeadProps:
                        currentRow[prop] = oblHeadProps[prop];
                
                currObliqueCands = constituentUnique(i+1, currObliqueCands);
                if len(currObliqueCands) > 1:
                    currentRow["Obl2"] = currObliqueCands[1]['phrase'];
                    if len(getHeads(i+1,currObliqueCands[1])) > 0:
                        currentRow["Obl2PosID"] = getHeads(i+1,currObliqueCands[1])[0]["ID"];
                if len(currObliqueCands) > 2:
                    currentRow["Obl3"] = currObliqueCands[2]['phrase'];
                    if len(getHeads(i+1,currObliqueCands[2])) > 0:
                        currentRow["Obl3PosID"] = getHeads(i+1,currObliqueCands[2])[0]["ID"];
                

            """
            if predTypes[j] == "N":
                #Consider nominal predicates as 'objects' for convenience
                
                #Features that don't take heads into account
                currentRow["OvertObj"] = currObject['phrase'];
                currentRow["ObjSylCo"] = sylCount(currentRow["OvertObj"]);
                    
                #Features that do take head into account
                if RCconj:
                    objectHeads = getHeads(primConjunctID,currObject);
                #else:
                    objectHeads = getHeads(i+1,currObject);
                objHeadProps = extractNPHeadProperties(objectHeads,"Obj");
                    
                for prop in objHeadProps:
                    currentRow[prop] = objHeadProps[prop];                
"""
            #And now the arguments
            currentClauseID += 1;
            j += 1;
            clauseTable = clauseTable.append(currentRow,ignore_index=True)
                                            # ,sort=False)
            print("CURRENTCLAUSEID::::::::::::::::::",currentClauseID)
    currSentID += 1;
            

            
        
print(clauseTable)
clauseTable.to_csv(path_or_buf="dec26table-first3000-v3.csv")

i = 0;
j = 0;
k = 0;
docTextTable  =  pd.DataFrame(columns=["Doc","Text"]);
while i + j < len(allSents):
    i = i + j;
    sentence = allSents[i];
    j = 1;
    found = False;
    currDoc = [sentence];
    docText = str(j) + ". " + sentence["phrase"];
    
    
    while found == False:
        if (i + j) == len(allSents): found = True; break;
        currSent = allSents[i+j];
        if currSent['doc'] != sentence['doc']:
            found = True;
        else:
            currDoc.append(allSents[i+j]);
            docText =  docText +" " +  str(j) + ". " + " ".join(currSent["phraseDF"]["FORM"]);
            j = j + 1;
    
    currentRow = dict();
    currentRow["Doc"] = sentence['doc'];
    currentRow["Text"] = docText;
    docTextTable = docTextTable.append(currentRow,ignore_index=True)
    print(j);
    print(i);
    
docTextTable.to_csv(path_or_buf="docs.csv")


#NMA! = Needs Manual Attention!
#We deal with NMA stuff semi-automatically now.

#TODO: Check Tenseless items if they're really tenseless -
#in particular would, could, should etc