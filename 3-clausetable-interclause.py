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
s = shelve.open("rawdata.dat");
allSents = s["allSents"];
s.close();
pd.set_option("max_columns",50)
from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()
clauseTable = pd.read_csv("sept26table.csv");

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
