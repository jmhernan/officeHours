# Functions specific to preprocess raw extract data from GoogleSheets

import re
import json
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

# regex conditions for text cleanup
REPLACE_BY_SPACE_RE = re.compile(r'[/(){}\[\]\|@,;]')
REM_USC = re.compile(r'(_)')
SEP_CAPS = re.compile(r'(?<=[a-z])(?=[A-Z])')
BAD_SYMBOLS_RE = re.compile(r'[\W]')
REPLACE_NUM_RMN = re.compile(r"([0-9]+)|(i[xv]|v?i{0,3})$")

# handle json
def get_metadata_dict(metadata_file):
    metadata_handle = open(metadata_file)
    metadata = json.loads(metadata_handle.read())
    return metadata

# clean text 
def clean_text(text):    
    text = REM_USC.sub(' ', text)
    text = SEP_CAPS.sub(' ', text)
    text = str.lower(text)
    text = REPLACE_NUM_RMN.sub('', text)
    text = REPLACE_BY_SPACE_RE.sub(' ', text)
    text = BAD_SYMBOLS_RE.sub(' ', text)
    text = ' '.join(word for word in text.split() if len(word)>1)
    return text

def get_top_n_words(corpus, n=None):
    """
    List the top n words in a vocabulary according to occurrence in a text corpus.
    """
    vec = CountVectorizer().fit(corpus)
    bag_of_words = vec.transform(corpus)
    sum_words = bag_of_words.sum(axis=0) 
    words_freq = [(word, sum_words[0, idx]) for word, idx in vec.vocabulary_.items()]
    words_freq = sorted(words_freq, key = lambda x: x[1], reverse=True)
    return words_freq[:n]