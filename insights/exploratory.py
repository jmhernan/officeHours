#!/usr/bin/env python
import numpy as np
import pandas as pd
import os
import re
import sys
from pathlib import Path
import matplotlib.pyplot as plt

this_file_path = os.path.abspath(__file__)
project_root = os.path.split(os.path.split(this_file_path)[0])[0]

import txt_preprocess as tp

path_data = os.path.join(project_root, "data") + '/'

# need to test the following
updated_data = sorted(list(filter(lambda x: '.csv' in x, os.listdir(path_data))))[-1]
oh_data =  pd.read_csv(os.path.join(path_data,updated_data), delimiter = ',')

# dept_abb = tp.get_metadata_dict(os.path.join(path_to_metadata, 'dept_abb.json'))
oh_data.columns
oh_data.shape

# look at unique values 
oh_data["Department"].value_counts()
oh_data["When"].value_counts()

# look at date months 
date_stm = oh_data["When"].astype("datetime64")

date_stm.groupby(date_stm.dt.month).count().plot(kind="bar")

# look at the reason for the visit
text =  oh_data['What'].dropna()

# clean things up for topic modeling
text = text.apply(tp.clean_text)

# Look at top words
tp.get_top_n_words(text)

# Who

oh_data.Who.value_counts()
