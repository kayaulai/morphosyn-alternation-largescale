# -*- coding: utf-8 -*-
"""
Created on Wed Nov  7 21:31:36 2018

@author: User
"""

import json
import io
io = io.StringIO('["streaming API"]')
json.loads(ann)

import json
from stanfordcorenlp import StanfordCoreNLP

nlp = StanfordCoreNLP('http://localhost', port=9000)
props = {'annotators': 'coref', 'pipelineLanguage': 'en'}

text = 'Barack Obama was born in Hawaii .  He is the president . Obama was elected in 2008 .'
result = json.loads(nlp.annotate(text, properties=props))

num, mentions = list(result['corefs'].items())[0]
for mention in mentions:
    print(mention)

props = {'annotators': 'coref', 'pipelineLanguage': 'en'}
nlp = StanfordCoreNLP('http://localhost', port=9000)

i = 0;
j = 0;
mentionsList = [];
while i + j < len(allSents):
    i = i + j;
    sentence = allSents[i];
    j = 1;
    found = False;
    currDoc = [sentence];
    docText = sentence["phrase"];
    
    
    while found == False:
        if (i + j) == len(allSents): found = True; break;
        currSent = allSents[i+j];
        if currSent['doc'] != sentence['doc']:
            found = True;
        else:
            currDoc.append(allSents[i+j]);
            docText = docText + " " + " ".join(currSent["phraseDF"]["FORM"]);
            j = j + 1;
    print(j);
    print(i);
    
    currResult = json.loads(nlp.annotate(docText, properties=props))
    if len(list(currResult['corefs'].items())) > 0:
        mentions = list(currResult['corefs'].items())
    else:
        mentions = [];
    mentionsList.append(mentions)
    
 
s2 = shelve.open("C:\\Users\\HKUII\\Downloads\\mentions.dat");
s2["mentionsList"] = mentionsList;
s2.close();

shelf = shelve.open("C:\\Users\\HKUII\\Downloads\\mentions.dat");
print(shelf["mentionsList"][0]);
shelf.close();

mentionsListString = str(mentionsList)
textFile = open("C:\\Users\\HKUII\\Downloads\\mentionsString.dat", "w")
textFile.write(mentionsListString)
textFile.close();