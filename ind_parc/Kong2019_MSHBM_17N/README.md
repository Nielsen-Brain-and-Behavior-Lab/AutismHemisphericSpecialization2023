# Kong2019 Individual Parcellations (MS-HBM)

## Contents
1. CBIG_preproc_tested_config_funconn.sh Configuration file for setting up CBIG environment.
2. subjids.txt Text file containing anonymized subject IDs.
3. Generate_data_step_0_UT.sh Script for generating text files prior to step 1.
4. parc_step_1_UT_ALL.m MATLAB script for generating session profiles and group parcellation.
5. Params_Final.mat Group priors from 37 GSP subjects in fsaverage6 space.
6. Generate data_step_2.5_UT.sh Script for generating text files prior to step 3.
7. Generate_data_step_2.5_part2_UT Script for determining which subjects have which number of runs available and create lists accordingly prior to step 3.
8. parc_step_3_UT_ALL.m MATLAB script for generating individual parcellations.
9. HungarianMatching.m MATLAB script for implementing a Hungarian matching algorithm to match labels with an fsaverage6 17-network group parcellation (Yeo 2011 parcellation)
10. 17Network_Reference_FS6_Labels_220808.mat Reference file for Hungarian matching fsaverage6 17N labels.
11. Data_vis folder containing scripts for visualizing the parcellations.

## Note
These scripts are used to implement the actual CBIG repo individual parcellation and Hungarian matching scripts which can be found at: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Kong2019_MSHBM
   
## Tutorial Guide
A tutorial guide can be found on NeuroDocs: https://neurodocs.readthedocs.io/en/latest/kong2019/parc_ov.html
