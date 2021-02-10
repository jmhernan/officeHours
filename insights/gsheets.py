#!/usr/bin/env python

# dowload google drive dataset
# function to work with the google api 
# you will need to enable your api and generate a key
# `GoogleAuth` will look for a "client_secrets.json" in the base directory
# You need to get the google drive file directory ID see analysis script for examples

import gspread
from oauth2client.service_account import ServiceAccountCredentials
import pandas as pd
import os
from pathlib import Path

this_file_path = os.path.abspath(__file__)
project_root = os.path.split(os.path.split(this_file_path)[0])[0]

path_data = os.path.join(project_root, "data") + '/'

import datetime

def _getToday():
	return datetime.date.today().strftime("%Y%m%d")

# use creds to create a client to interact with the Google Drive API
scope = ['https://spreadsheets.google.com/feeds','https://www.googleapis.com/auth/drive']
creds = ServiceAccountCredentials.from_json_keyfile_name(os.path.join(project_root, 'insights/credentials.json'), scope)
client = gspread.authorize(creds)

db_name = 'DSS office hours'
data = client.open(db_name)
data.worksheets()
data_sheet = 'DSS Office Hours 2020'

updated_data = data.worksheet(data_sheet)
list_of_hashes = updated_data.get_all_records()
headers = list_of_hashes.pop(0)
df = pd.DataFrame(list_of_hashes, columns=headers)
print(df.tail())

filename = "%s_%s.%s" % ("office_hours_data_2020", _getToday() ,"csv")

df.to_csv(os.path.join(path_data, filename), encoding='utf-8', index=False)

db_name = 'DSS office hours'
data = client.open(db_name)
data_sheet = 'DSS Office Hours 2019'

updated_data = data.worksheet(data_sheet)
list_of_hashes = updated_data.get_all_records()
headers = list_of_hashes.pop(0)
df = pd.DataFrame(list_of_hashes, columns=headers)
print(df.tail())

filename = "%s_%s.%s" % ("office_hours_data_2019", _getToday() ,"csv")

df.to_csv(os.path.join(path_data, filename), encoding='utf-8', index=False)