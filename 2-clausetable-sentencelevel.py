# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

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

def getDependents(headID, relation, phrase, depOnly = True):
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
            
    return newPhrases;

def getDependentsFromDeprel(headID, relation, phrase):
    df = phrase['phraseDF'];
    i = 1;
    newPhrases = [];
    while i <= df.shape[0]:
        if (df["HEAD"].iloc[i-1] == str(headID)) & (df["DEPREL"].iloc[i-1] == relation):
            newPhrases.append(getPhrase(i, phrase));
        i += 1;
            
    return newPhrases;


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
    elif RC:
        print(str(df["ID"]))
        while i < df.shape[0]:
            print(df.iloc[i,]["HEAD"])
            if int(df.iloc[i,]["HEAD"]) not in df["ID"].tolist():
                heads.append(df.iloc[i,]);
            i += 1;
        if len(heads) > 1: print("RC argument no. of heads exceeds 1! Please check getHeads code.");
    if heads == []: heads == [None]; print("heynohead");
    return(heads);
    
def combineNPFeatures(fullNPfeatures, coreffeatures, prefix):
    finalBundle = dict();
    finalBundle[prefix + "Freq"] = coreffeatures[prefix + "Freq"];
    finalBundle[prefix + "Def"] = "def";
    finalBundle[prefix + "Head"] = coreffeatures[prefix + "Head"];
    finalBundle[prefix + "Morph"] = coreffeatures[prefix + "Morph"];
    finalBundle[prefix + "Pers"] = fullNPfeatures[prefix + "Pers"];
    finalBundle[prefix + "Anim"] = fullNPfeatures[prefix + "Anim"]
    finalBundle[prefix + "Num"] = fullNPfeatures[prefix + "Num"];
    finalBundle[prefix + "SynType"] = coreffeatures[prefix + "SynType"];
    return(finalBundle)
        
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

def extractRefRelationFromList(parentID, phraseList):
    ids = [];
    deps = [];
    refs = [];
    for phrase in phraseList:
        currHead = getHeads(parentID, phrase)[0]; #Just the first conjunt should suffice
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



def extractNPHeadProperties(heads, prefix = ""):
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
            currProps["SynType"] = "NMN!";
            
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
    
        #Animacy          
        #We will be working with the animacy tags from 
        #Zaenen et al. (2004)
        #human, org, animal, place, time,
        #concrete, nonconc, mac, veh
        currProps["Anim"] = getNomSem(head,sentence,
                  head["XPOS"] in ["NNP","NNPS"])
            
        #Exceptions
        if head["LEMMA"] in nomSemExceptions: 
            currProps["Anim"] = "NMA!";
            
        headPropsSep.append(currProps)
    
     #Part II: Combination
     multValProps = ["Head", "Freq", "Def", "Anim",
                        "SynType", "Morph"];
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



nomSemExceptions = ["member"]



allSentsR = allSents[7125:7126] #R stands for reduced and is for testing purposes

#TODO: Embedding depth, idiomaticity, 

clauseTableColnames = ["ClauseID","Doc","SentID","SentForm",
                            "VForm","VLemma","VMorph","VMorphForm","VTense",
                            "VAspect","Voice","VSylCo","VFreq","Aux3","Aux2","Aux1",
                            "PassAux","VClass","OvertSubj","CovertSubj","SubjRef",
                            "SubjHead","SubjFreq","SubjDef","SubjNum",
                            "SubjPers","SubjAnim","SubjSynType",
                            "SubjSylCo","SubjMorph","OvertObj","OvertIObj","CovertObj","ObjFreq",
                            "ObjHead","ObjDef","ObjNum","ObjPers","ObjAnim",
                            "ObjSylCo","ObjMorph","ObjSynType","OvertObl1",
                            "Obl2","Obl3","Obl4","Obl5"];
                       

print(getParents(24, allSents[14], "conj"));

clauseTable = pandas.DataFrame(columns=clauseTableColnames);

#Accepting multiple values: SubjHead, SubjFreq, SubjDef, SubjAnim, SubjSynType, SubjMorph
#Combined: SubjSylCo, SubjNum, SubjPers


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
                
                #Verb form stuff.
                currentRow["VForm"] = df.iloc[i,]["FORM"];
                currentRow["VLemma"] = df.iloc[i,]["LEMMA"];
                currentRow["VMorph"] = df.iloc[i,]["XPOS"];
                currentRow["VMorphForm"] = featsInfo["VerbForm"];
                auxiliaries = getDependents(i+1,"aux",sentence);
                auxLemmas = [None] * len(auxiliaries)
                
                #Fill up auxiliaries
                k = len(auxiliaries);
                for aux in auxiliaries:
                    auxHead = getHeads(i+1,aux)[0];
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
                
                #Verb freq
                currentRow["VFreq"] = word_frequency(currentRow["VForm"], 'en')

                    
                #More abstract grammatical categories
                if featsInfo["VerbForm"] == "Fin":
                    currentRow["VTense"] = getValueFromInfo(featsInfo,"Tense");
                else:
                    for aux in auxiliaries:
                        auxHead = getHeads(i+1,aux)[0];
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
                currSubjectCands = getDependents(i+1,"nsubj",sentence);
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

                print("objStart");
                currObjectCands = getDependents(i+1,"iobj",sentence,False);
                currObjectCands = currObjectCands + getDependentsFromDeprel(i+1,"iobj",sentence);
                if len(currObjectCands) == 0:
                    currObjectCands = getDependents(i+1,"obj",sentence);
                    currObjectCands = currObjectCands + getDependentsFromDeprel(i+1,"obj",sentence);
                    currentRow["OvertIObj"] = 0;
                else:
                    currentRow["OvertIObj"] = 1;
                    
                print(currObjectCands)
                
                if re.search("acl:relcl",df.iloc[i,]["DEPREL"]): #For relative clauses only
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
                elif re.search("acl:relcl",df.iloc[i,]["DEPS"]): #RCs that aren't the first conjunct
                    primConjunct = getParents(i+1, sentence, "conj")[0];
                    print("primConjunct",primConjunct)
                    primConjunctID = primConjunct["ID"];
                    currObjectCands = currObjectCands + getDependentsFromDeprel(primConjunctID, "nobj",sentence);
                    currObjectCands = currObjectCands + getDependentsFromDeprel(primConjunctID, "nobj:pass",sentence);
                    relations = extractRefRelationFromList(primConjunct["ID"],currObjectCands)
                    if  len(relations) == 0:
                        if len(currObjectCands)>0:
                            currObject = currObjectCands[0];
                    else:
                        if len(relations) > 1: warnings.warn("ATTENTION: >1 ref relation!");
                        relations = relations[0];
                        currObject = currObjectCands[relations[1]];
                        currCovertObject = currObjectCands[relations[0]]; 
                        RCconj = True;
                elif len(currObjectCands)>0:
                    currObject = currObjectCands[0];
                else:
                    currObject = "NOOBJ";

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

                        
            #And now the arguments
            currentClauseID += 1;
            j += 1;
            clauseTable = clauseTable.append(currentRow,ignore_index=True)
                                            # ,sort=False)
        
print(clauseTable)
clauseTable.to_csv(path_or_buf="sept26table.csv")


#NMA! = Needs Manual Attention!
#We deal with NMA stuff semi-automatically now.

#TODO: Check Tenseless items if they're really tenseless -
#in particular would, could, should etc