#!/usr/bin/env python
import numpy as np
import pandas as pd
import os
import re
import sys
from pathlib import Path
import matplotlib.pyplot as plt

from gensim.models import Word2Vec
import nltk
from nltk.tokenize import word_tokenize

this_file_path = os.path.abspath(__file__)
project_root = os.path.split(this_file_path)[0]

sys.path.append(project_root)

import txt_preprocess as tp

data_path = os.path.split(os.path.split(this_file_path)[0])[0] + '/data/'

# data
child_df =  pd.read_excel(os.path.join(data_path,'ChildObservationLemmaDataNoNumNoP_0622.xlsx'), index_col=0)

child_df.columns
child_df.shape

child_df['Lemmatize'].dtypes

text = child_df['Lemmatize'].astype(str)

text = text.apply(tp.clean_text)

tp.get_top_n_words(text, n=100)

# Maybe some cleaning is needed lets tokenize
tokenized_text = [word_tokenize(i) for i in text]

# word embedding model
model_baseline = Word2Vec(tokenized_text, min_count=1) 

len(list(model_baseline.wv.vocab))

# 100 most occuring 
model_baseline.wv.index2entity[:100] # same as top_n words

# Build a function
keywords = [
    'laugh',
    'play',
    'hit',
    'hold'
]

model_baseline.most_similar(positive='laugh', topn=10)
model_baseline.most_similar(positive='play', topn=10)
model_baseline.most_similar(positive='hit', topn=10)
model_baseline.most_similar(positive='angry', topn=10)
# create function
