# -*- coding: utf-8 -*-
"""
Created on Mon Oct  8 09:30:17 2018

@author: User
"""

# -*- coding: utf-8 -*-
"""
Created on Fri Sep 14 06:59:55 2018

@author: User
"""


#Run this whenever restarting.
import os;
os.chdir("G:\我的雲端硬碟\corpus priming\eng")
import warnings;
#os.chdir("G:\My Drive\corpus priming\eng")
import pandas as pd;
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
s = shelve.open("G:\\我的雲端硬碟\\corpus priming\\eng\\rawdata.dat");
allSents = s["allSents"];
s.close();
pd.set_option("max_columns",50)
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
clauseTable = pd.read_csv("G:\\我的雲端硬碟\\corpus priming\\eng\\oct5table.csv");

import spacy;
nlp = spacy.load('en_core_web_sm')

s2 = shelve.open("G:\\我的雲端硬碟\\corpus priming\\eng\\mentions.dat");
mentionsList = s2["mentionsList"];
s2.close();

def addChain(orig, new):
    if orig != "/":
        origSplit = orig.split(",");
        if str(new) not in origSplit:
            new = orig + "," + new;
    return(new);

#Actually using the coreference data
i = 0;
j = 0;
k = 0;
newClauseTable = pd.DataFrame();
while i + j < len(allSents[0:40]):
    i = i + j;
    sentence = allSents[i];
    j = 1;
    found = False;
    currSent = allSents[i+j];
    currDoc = currSent["doc"];
    currSents = [allSents[i+j]];
    
    currClauses =  clauseTable.loc[clauseTable["Doc"] == currDoc,:];
    #currCorefTable = pd.DataFrame(columns=["SubjMention-","SubjMention-1","SubjMention-2","SubjMention-3","SubjMention-4","SubjMention-5","SubjMention-6","SubjMention-7","SubjMention-8","SubjMention-9","SubjMention-10","ObjMention1","ObjMention2","ObjMention3","ObjMention4","ObjMention5","ObjMention6","ObjMention7","ObjMention8","ObjMention9","ObjMention10"]);
    #currCorefTable = pd.DataFrame(["SubjChain","ObjChain","OblChain"])
    currClauses["SubjChain"] = np.full(currClauses.shape[0],"/");
    currClauses["ObjChain"] = np.full(currClauses.shape[0],"/");
    currClauses["OblChain"] = np.full(currClauses.shape[0],"/");
    
    while found == False:
        if (i + j) == len(allSents): found = True; break;
        currSent = allSents[i+j];
        if currSent['doc'] != sentence['doc']:
            found = True;
        else:
            currSents.append(allSents[i+j]);
            j = j + 1;
            
            
    m = 0;
    currClausesNew = pd.DataFrame();
    for mention in mentionsList[k]:
        chainID = str(m);
        for item in mention[1]:
              for i in np.arange(0,currClauses.shape[0]-1,1):
                  if (currClauses.iloc[i,]["SentID"]+1)==item['sentNum']:
                      print(item);
                      if currClauses.iloc[i,]["SubjPosID"] != "/":
                          if item['headIndex'] == int(currClauses.iloc[i,]["SubjPosID"]):
                              currClauses["SubjChain"][i] = addChain(currClauses["SubjChain"][i],chainID);
                              print(currClauses["SubjChain"][i])
                              print(addChain(currClauses["SubjChain"][i],chainID))
                         
                      if currClauses.iloc[i,]["ObjPosID"] != "/":
                          if item['headIndex'] == int(currClauses.iloc[i,]["ObjPosID"]):
                              currClauses["ObjChain"][i] = addChain(currClauses["ObjChain"][i],chainID);
                          
                      if currClauses.iloc[i,]["OblPosID"] != "/":
                          if item['headIndex'] == int(currClauses.iloc[i,]["OblPosID"]):
                              currClauses["OblChain"][i] = addChain(currClauses["OblChain"][i],chainID);
        
        m = m + 1;
    
    newClauseTable = newClauseTable.append(currClauses);
                
            
    k = k + 1;
    print(j);
    print(i);



#Previous k clauses
k = 10;
pctCols = [];
for i in np.arange(1,k+1,1): pctCols.append("PrevVoice"+str(i));

prevClausesTable = pd.DataFrame(columns=pctCols);
for clause in clauseTable.iterrows():
    currID = clause[1]["ClauseID"];
    currRow = dict()
    for i in np.arange(max(1,currID-k),currID,1):
        currRow["PrevVoice"+str(currID-i)] = clauseTable.iloc[i-1,]["Voice"]; #This line must be changed (i-1 is wrong)
    print(currRow)
    prevClausesTable = prevClausesTable.append(currRow,ignore_index=True)
    
clauseTable = pd.concat([clauseTable,prevClausesTable], axis=1)
