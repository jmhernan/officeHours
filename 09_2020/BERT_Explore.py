import pandas as pd
import numpy as np
from sklearn import metrics, manifold## for processing
import re
import nltk## for plotting
import matplotlib.pyplot as plt
import seaborn as sns## for w2v
import gensim
import gensim.downloader as gensim_api## for bert
import transformers

import os

df = pd. read_excel("ChildObservationCleanedPreprocessed.xlsx")

df_clean = df["Lemmatize"]

gl_embed = gensim_api.load("glove-wiki-gigaword-300")