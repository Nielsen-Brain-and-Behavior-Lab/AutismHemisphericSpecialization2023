# CBIG2016 Preprocessing

## Contents
The following scripts were used to implement the multi-echo preprocessing pipeline (Note: Actual pipeline code is available at the CBIG GitHub repo: https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/preprocessing/CBIG_fMRI_Preproc2016).

1. ids.txt List of anonymized subject IDs.
2. example_config.txt Sets the preprocessing steps and desired order.
3. get_individual_subjects.sh Necessary to generate text file with paths for each subjects' mutli-echo runs
4. preproc_wrap.sh Wrapper script to run the preprocessing pipeline. Tied to preproc_job.sh and submits one job per subject.
5. preproc_job.sh Job script to run the preprocessing pipeline.
6. tedana_wrap.sh Wrapper script to restart the tedana step (sometimes fails for unknown reasons)
7. tedana_preproc.sh. Job script to restart the tedana step.

## Tutorial Guide
A tutorial guide to using this preprocessing pipeline is available on NeuroDocs (https://neurodocs.readthedocs.io/en/latest/) and with Tedana Integration (https://neurodocs.readthedocs.io/en/latest/cprep/cprep_4.html)
