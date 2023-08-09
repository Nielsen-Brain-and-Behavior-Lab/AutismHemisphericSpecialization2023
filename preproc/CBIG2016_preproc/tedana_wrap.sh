#!/bin/bash

#Purpose: Submit tedana restart script for each subject
#Written by M. Peterson, Nielsen Brain and Behavior Lab

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/Utah_analysis/CBIG2016_preproc_ALL

for subj in `cat ${CODE_DIR}/subjids/preproc_failed_ids2.txt`; do
	mkdir -p ${CODE_DIR}/subject_scripts/${subj}
	cp ${CODE_DIR}/tedana_preproc.sh ${CODE_DIR}/subject_scripts/${subj}
	
	#replace subject with subject name
	sed -i 's|${subj}|'"${subj}"'|g' ${CODE_DIR}/subject_scripts/${subj}/tedana_preproc.sh
	
	source ${CODE_DIR}/subject_scripts/${subj}/tedana_preproc.sh
done
