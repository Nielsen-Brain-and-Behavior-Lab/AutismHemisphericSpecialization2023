#!/usr/bin/env python
# Purpose: Find avg. DVARS value for each session and for each participant overall.
#
# Input: Single-column txt file DVARS for each bold image. These were generated using the CBIG2016 preproc pipeline.
# Output: Avg. DVARS value for each participant's session an deach participant overall. 
#
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License (2021)

#load packages
from pathlib import Path
import time
import os
from os.path import dirname, join as pjoin
import sys
import scipy.io #loads .mat files
import csv
import numpy as np
import pandas as pd
import glob

#for each subject, load the FDRMS files for each session
directory = '/fslgroup/grp_proc/compute/Utah_analysis/CBIG2016_preproc_FS6/' #set the directory to your preproc output
sub_count = 0
for sub in os.scandir(directory): #loop through each subject in the preproc dir
	sub_count = sub_count + 1 
	sub_name = str(sub.name) + "/" + str(sub.name) + "/bold/mc"
	subdir = pjoin(directory, sub_name)
	counter = 0
	for sess in glob.iglob(f'{subdir}/*_rest_skip4_motion_outliers_DVARS'): #loop through each bold session
		counter = counter + 1
		sess_df = pd.read_table(sess, header=0, sep='\s+') #seeting header=0 removes the first line where FD=0

		avg = sess_df.mean(axis='rows') #take average of column

		#append sess avg to running_list
		if counter==1:
			running_list = []
			running_list.append(avg)
		else:
			running_list.append(avg)
	
	sub_avg = np.mean(np.array(running_list)) #find the subject's avg DVARS
	
	#append sub's avg to list
	if sub_count==1:
		sub_list = []
		sub_list.append(str(sub.name))
		DVARS_list = []
		DVARS_list.append(str(sub_avg))
	else:
		sub_list.append(str(sub.name))
		DVARS_list.append(str(sub_avg))


#Save the sub_avg list as .csv
r_name = "DVARS_avg_UT_ALL_230224.csv" #file name

df = pd.DataFrame(list(zip(sub_list, DVARS_list)), columns = ['ID', 'DVARS_avg']) #create two-column dataframe with subject name and avg FD

df.to_csv(r_name, index=False, index_label=None) #save dataframe to csv

