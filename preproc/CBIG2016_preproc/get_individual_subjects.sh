#!/bin/bash

# Set your paths here
HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/Utah_analysis/CBIG2016_preproc_ALL
data_DIR=/fslgroup/grp_proc/compute/Utah_analysis/Utah_BIDS

# Create that special CBIG subjids text file for each subject (data must be in BIDS format)
# CBIG example format: 001 /fslhome/NETID/Downloads/CBIG_Data/Sub0001/func/Sub0001_Ses1.nii
# Ideal for running subjects parallel
# Change the path of the first for loop to be your code_DIR

counter=0
#for subj in `cat $code_DIR/subjids/ids.txt`; do 	
for subj in `cat $code_DIR/subjids/alt_subjids.txt`; do
	for run in {1..2}; do
		path=${data_DIR}/${subj}/func
		file1=${path}/${subj}_ses-1_task-rest_run-${run}_echo-1_bold.nii.gz
		if [ -f "$file1" ]; then
			counter=$((counter+1))
			file2=${path}/${subj}_ses-1_task-rest_run-${run}_echo-2_bold.nii.gz
			file3=${path}/${subj}_ses-1_task-rest_run-${run}_echo-3_bold.nii.gz
			(echo 00$counter ${file1} ${file2} ${file3}) >> ${data_DIR}/${subj}/${subj}_fmrinii.txt
		else
			echo ${file1}
		fi	
	done
counter=0
done
