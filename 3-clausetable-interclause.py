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
import csvkit
from nltk.corpus import wordnet as wn
s = shelve.open("G:\\我的雲端硬碟\\corpus priming\\eng\\rawdata.dat");
allSents = s["allSents"];
s.close();
pd.set_option("max_columns",50)
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
clauseTable = pd.read_csv("G:\\我的雲端硬碟\\corpus priming\\eng\\dec26table-first3000-v3.csv", engine='python');

#https://stackoverflow.com/a/40093482/9505928
which = lambda lst:list(np.where(lst)[0])

import spacy;
nlp = spacy.load('en_core_web_sm')

s2 = shelve.open("G:\\我的雲端硬碟\\corpus priming\\eng\\mentions.dat");
mentionsList = s2["mentionsList"];
s2.close();

def splitConjString(string):
    components = string.split(";;");
    return(components);

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
sentCorefTable = pd.DataFrame(dict(SentID=np.arange(0,len(allSents),1),Chains=np.full(len(allSents),"/")));
while i + j < len(allSents[0:1350]):
    i = i + j;
    sentence = allSents[i];
    j = 1;
    found = False;
    currSent = allSents[i+j];
    currDoc = currSent["doc"];
    currSents = [allSents[i+j]];
    
    currClauses =  clauseTable.loc[clauseTable["Doc"] == currDoc,:];
    firstIndex = currClauses.index[0]
    lastIndex = currClauses.index[len(currClauses.index)-1]
    #currCorefTable = pd.DataFrame(columns=["SubjMention-","SubjMention-1","SubjMention-2","SubjMention-3","SubjMention-4","SubjMention-5","SubjMention-6","SubjMention-7","SubjMention-8","SubjMention-9","SubjMention-10","ObjMention1","ObjMention2","ObjMention3","ObjMention4","ObjMention5","ObjMention6","ObjMention7","ObjMention8","ObjMention9","ObjMention10"]);
    #currCorefTable = pd.DataFrame(["SubjChain","ObjChain","OblChain"])
    currClauses["SubjChain"] = np.full(currClauses.shape[0],"/");
    currClauses["ObjChain"] = np.full(currClauses.shape[0],"/");
    currClauses["OblChain"] = np.full(currClauses.shape[0],"/");
    currClauses["Obl2Chain"] = np.full(currClauses.shape[0],"/");
    currClauses["Obl3Chain"] = np.full(currClauses.shape[0],"/");
    
    #subjChainIndex = which([x == 'SubjChain' for x in list(currClauses)])[0]
    #objChainIndex = which([x == 'ObjChain' for x in list(currClauses)])[0]
    #oblChainIndex = which([x == 'OblChain' for x in list(currClauses)])[0]
    #obl2ChainIndex = which([x == 'Obl2Chain' for x in list(currClauses)])[0]
    #obl3ChainIndex = which([x == 'Obl3Chain' for x in list(currClauses)])[0]
    #sentNumIndex = which([x == 'SentID' for x in list(currClauses)])[0];
    
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
             sentCorefTable.iloc[i + item['sentNum'],1] = addChain(sentCorefTable.iloc[i + item['sentNum'],1],chainID)
             for p in np.arange(firstIndex,lastIndex,1):
                  if (currClauses.loc[p,"SentID"]+1)==item['sentNum']:
                      if (currClauses.loc[p,"SubjPosID"] != "/"):
                          subjIDs = [int(n) for n in splitConjString(currClauses.loc[p,"SubjPosID"])];
                          print("REACHED");
                          if item['headIndex'] in subjIDs:
                              currClauses.loc[p,"SubjChain"] = addChain(currClauses.loc[p,"SubjChain"],chainID);
                             
                      if (currClauses.loc[p,"ObjPosID"] != "/"):
                          objIDs = [int(n) for n in splitConjString(currClauses.loc[p,"ObjPosID"])];
                          if item['headIndex'] in objIDs:
                              currClauses.loc[p,"ObjChain"] = addChain(currClauses.loc[p,"ObjChain"],chainID);
                          
                      if (currClauses.loc[p,"OblPosID"] != "/"):
                          oblIDs = [int(n) for n in splitConjString(currClauses.loc[p,"OblPosID"])];
                          if item['headIndex'] in oblIDs:
                              currClauses.loc[p,"OblChain"] = addChain(currClauses.loc[p,"OblChain"],chainID);
                      
                      if (currClauses.loc[p,"Obl2PosID"] != "/"):
                          obl2IDs = [int(n) for n in splitConjString(currClauses.loc[p,"Obl2PosID"])];
                          if item['headIndex'] in obl2IDs:
                              currClauses.loc[p,"Obl2Chain"] = addChain(currClauses.loc[p,"Obl2Chain"],chainID);
                        
                      if (currClauses.loc[p,"Obl3PosID"] != "/"):
                          obl3IDs = [int(n) for n in splitConjString(currClauses.loc[p,"Obl3PosID"])];
                          if item['headIndex'] in obl3IDs:
                              currClauses.loc[p,"Obl3Chain"] = addChain(currClauses.loc[p,"Obl3Chain"],chainID);
        
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
for clause in newClauseTable.iterrows():
    currID = clause[1]["ClauseID"];
    currRow = dict()
    for i in np.arange(max(1,currID-k),currID,1):
        currRow["PrevVoice"+str(currID-i)] = newClauseTable.iloc[i-1,]["Voice"]; #This line must be changed (i-1 is wrong)
    print(currRow)
    prevClausesTable = prevClausesTable.append(currRow,ignore_index=True)
    
newClauseTable = pd.concat([newClauseTable,prevClausesTable], axis=1)
newClauseTable.to_csv(path_or_buf="dec26-table-withintersentence.csv")
