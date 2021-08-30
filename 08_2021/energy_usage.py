import os
import glob
import pandas as pd

# get data file names from data/
# you will have a unique path
path = '../data/daily_dataset'
filenames = glob.glob(path + "/*.csv")

# read files and explore 
df = pd.read_csv(filenames[0])

# Once you have the in mem you can begin to summarise
hh_energy_count_mean = df[['LCLid','energy_count']].groupby("LCLid").mean()

# you will need to extract the block information from the file 
# and create a column to identify 

# you will save to a list and move on to the next until finished 
