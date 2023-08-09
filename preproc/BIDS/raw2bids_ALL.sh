#!/bin/bash

#Purpose: Take raw nifti files and conform naming conventions to BIDS
#Inputs: Utah dataset nifti files (time 5 multiecho)
#Outputs: Utah dataset in BIDS format
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/Utah_analysis/data_org
RAW_DIR=/fslgroup/grp_proc/compute/Utah_analysis/Time5_221207
BIDS_DIR=/fslgroup/grp_proc/compute/Utah_analysis/Utah_BIDS

#LOOP
for sub in `cat $CODE_DIR/ALL_subjids.txt`; do
	#Make BIDS filestructure
	mkdir -p $BIDS_DIR/sub-$sub/func $BIDS_DIR/sub-$sub/anat $BIDS_DIR/sub-$sub/fmap	

	#Convert structural scans to BIDS
	cp $RAW_DIR/${sub}-c/Structural/mp2rage.nii.gz $BIDS_DIR/sub-${sub}/anat/sub-${sub}_ses-1_MP2RAGE.nii.gz

	#Convert resting scans to BIDS: LR
	count=0
		for echo in `ls $RAW_DIR/${sub}-c/Resting/DICOM_Multiecho_BOLD_LR*.nii.gz`; do
			count=$((count+1))
			ECHO=`basename $echo`
			cp $RAW_DIR/${sub}-c/Resting/$ECHO $BIDS_DIR/sub-$sub/func/sub-${sub}_ses-1_task-rest_run-1_echo-${count}_bold.nii.gz
		done

	#Convert resting scans to BIDS: RL
	count=0
		for echo in `ls $RAW_DIR/${sub}-c/Resting/DICOM_Multiecho_BOLD_RL*.nii.gz`; do
			count=$((count+1))
			ECHO=`basename $echo`
			cp $RAW_DIR/${sub}-c/Resting/$ECHO $BIDS_DIR/sub-$sub/func/sub-${sub}_ses-1_task-rest_run-2_echo-${count}_bold.nii.gz
		done

	#convert fieldmaps to BIDS
				
	


	#convert physio logs to BIDS
		#LR
			cp $RAW_DIR/${sub}-c/Resting/LR_Physio*_PULS.log $BIDS_DIR/sub-$sub/sub-${sub}_ses-1_task-rest_run-1_physio-PULS.log		
			cp $RAW_DIR/${sub}-c/Resting/LR_Physio*_RESP.log $BIDS_DIR/sub-$sub/sub-${sub}_ses-1_task-rest_run-1_physio-RESP.log
		#RH
			cp $RAW_DIR/${sub}-c/Resting/RL_Physio*_PULS.log $BIDS_DIR/sub-$sub/sub-${sub}_ses-1_task-rest_run-2_physio-PULS.log
			cp $RAW_DIR/${sub}-c/Resting/RL_Physio*_RESP.log $BIDS_DIR/sub-$sub/sub-${sub}_ses-1_task-rest_run-2_physio-RESP.log

done

