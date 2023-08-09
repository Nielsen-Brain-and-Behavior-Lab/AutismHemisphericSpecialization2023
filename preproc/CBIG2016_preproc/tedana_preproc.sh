#!/bin/bash

#Purpose: Run CBIG2016 preprocessing for multi-echo data. Restarts the Tedana command and remainder of the preproc.
#Written by M. Peterson, Nielsen Brain and Behavior Lab

#PATHS
HOME=/fslgroup/fslg_spec_networks/compute
CODE_DIR=${HOME}/code/Utah_analysis/CBIG2016_preproc_ALL
OUT_DIR=/fslgroup/grp_proc/compute/Utah_analysis/CBIG2016_preproc_FS6
mkdir -p ${CODE_DIR}/subject_scripts/${subj}

#STEP 1: Tedana Processing
	source ${CODE_DIR}/CBIG_preproc_tested_config_funconn.sh
	#grab tedana command from CBIG preproc log file
	sed -n '/CBIG_preproc_multiecho_denoise]/p' ${OUT_DIR}/${subj}/${subj}/logs/CBIG_preproc_fMRI_preprocess.log >> ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt

	#remove first three lines in order to isolate the command
	sed -i '1d' ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt
	sed -i '1d' ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt
	sed -i '1d' ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt

	#remove the last line of the file
	sed -i '2d' ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt

	#remove the first handful of characters that preceed the command	
	sed -r 's/.{34}//' ${CODE_DIR}/subject_scripts/${subj}/tedanacommand.txt > ${CODE_DIR}/subject_scripts/${subj}/tedanacommand2.txt

	#run the command
	sh ${CODE_DIR}/subject_scripts/${subj}/tedanacommand2.txt


#STEP 2: Restart the Preproc

	#Submit the job script for the subject (as if this script is a wrapper)
	    mkdir -p ${CODE_DIR}/logfiles
	    sbatch \
	    -o ${CODE_DIR}/logfiles/output_${subj}.txt \
	    -e ${CODE_DIR}/logfiles/error_${subj}.txt \
	    ${CODE_DIR}/preproc_job.sh \
	    ${subj}
	    sleep 5


