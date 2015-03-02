# ***************************************************************************************
# ***************************************************************************************
# ************************ Analyzing Yelp, Inc. Business Reviews ************************ 
# *********** Written by David W. Vinson for the purpose of demonstration *************** 
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# *************************************************************************************** 

# This script builds the unigram and bigram distrubitions needed to determine the 'information' of reviews

import nltk
import numpy as np
import os, re
import json

txt = []
with open('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_review.json') as f:
    for line in f:
        a = json.loads(line)
        txt.append(a['text'].lower())

unis = nltk.FreqDist()
bigs = nltk.FreqDist()
for line in txt:
    words = nltk.word_tokenize(line)
    big = nltk.bigrams(words)
    
    for word in words:
        unis[word] += 1
        
    for bigrams in big: 
        bigs[bigrams] += 1

with open("/Users/Dave/Desktop/unis.csv", "w") as w:
    for item in unis.items():
        w.write('{},{}\n'.format(item[0].encode('utf-8'), item[1]))

with open("/Users/Dave/Desktop/bigs.csv", "w") as w:
    for item in bigs.items():
        w.write('{},{},{}\n'.format(item[0][0].encode('utf-8'), item[0][1].encode('utf-8'), item[1]))