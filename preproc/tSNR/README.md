# Temporal SNR

## Contents
1. ids.txt Text file containing anonymized subject IDs.
2. step1_fslmaths.sh Calculates tSNR for each run using mean BOLD signal divided by the standard deviation (requires CBIG2016 preproc output)
3. step2_surfproject.sh Projects tSNR maps from step 1 to the native surface
4. step3_loadgifti.m Generates a subject-averaged tSNR .mat file and .csv file containing the network-averaged tSNR for each subject (requires individual parcellations)
