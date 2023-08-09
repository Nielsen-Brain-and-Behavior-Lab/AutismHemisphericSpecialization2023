#!/bin/bash

HOME=/fslgroup/fslg_spec_networks/compute
code_DIR=${HOME}/code/Utah_analysis/CBIG2016_preproc_ALL
output_DIR=/fslgroup/grp_proc/compute/Utah_analysis/CBIG2016_preproc_FS6

##Change this first line to your code_DIR pointing to the subjids file
for subj in `cat $code_DIR/subjids/ids.txt`;do
    mkdir -p ${code_DIR}/logfiles
    sbatch \
    -o ${code_DIR}/logfiles/output_${subj}.txt \
    -e ${code_DIR}/logfiles/error_${subj}.txt \
    ${code_DIR}/preproc_job.sh \
    ${subj}
    sleep 5
done
