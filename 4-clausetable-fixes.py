# -*- coding: utf-8 -*-
"""
Created on Tue Dec 25 06:23:23 2018

@author: User
"""

#This is for non-systematic fixes to the clause tables
#that nevertheless need code
#Or fixes that need 3 to be run first
#For systematic fixes, 2/3 are modified
#For manual, nonsystematic fixes, they're just fixed maually
#Using extra columns


#Run this whenever restarting.
#Run this whenever restarting.
import os;
os.chdir("G:\我的雲端硬碟\corpus priming\eng")
#os.chdir("G:\My Drive\corpus priming\eng")
import pandas as pd;
import shelve;
import numpy as np;
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
import nltk;
#https://stackoverflow.com/a/4103234
nltk.download('verbnet');
s = shelve.open("G:\\我的雲端硬碟\\corpus priming\\eng\\rawdata.dat");
allSents = s["allSents"];
s.close();
pd.set_option("max_columns",50)
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
clauseTable = pd.read_csv("G:\\我的雲端硬碟\\corpus priming\\eng\\dec26-table-withintersentence.csv", engine='python');

#https://stackoverflow.com/a/40093482/9505928
which = lambda lst:list(np.where(lst)[0])


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
sentCorefTable = pd.DataFrame(dict(OverallSentID=np.arange(0,len(allSents),1), Doc=np.full(len(allSents),"/"),SentID=np.full(len(allSents),"0"),Chains=np.full(len(allSents),"/")));
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
            
            
    sentCorefTable.iloc[i:(i+j),1] = currDoc;
    sentCorefTable.iloc[i:(i+j),2] = np.arange(0,j,1);
    
    m = 0;
    currClausesNew = pd.DataFrame();
    for mention in mentionsList[k]:
        chainID = str(m);
        for item in mention[1]:
             sentCorefTable.iloc[i + item['sentNum'],3] = addChain(sentCorefTable.iloc[i + item['sentNum'] - 1,3],chainID)
             for p in np.arange(firstIndex,lastIndex+1,1):
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


sentCorefTable.to_csv(path_or_buf="dec27-sentence-coref-table.csv")