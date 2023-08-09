# Autism Hemispheric Specialization 2023

## Purpose
The aim of this study was to estimate hemispheric speiclaization in autism using an individual-level approach and then explore potential relationships between language specialization and behavioral phenotypes including verbal ability, language delay, and autism symptom severity. Using multi-echo resting-state fMRI scans, we generated individual network parcellations and estimated network specialization using a surface area-based approach. 

## Getting Started
Scripts are organized in the following folders: preproc, ind_parc, network_sa, and stats.

The _preproc_ folder contains scripts for data organization, FreeSurfer, multi-echo preprocessing (the CBIG2016 pipeline with tedana integrated), and tSNR calculation.
The _ind_parc_ folder contains scripts for running the Kong2019 MS-HBM pipeline and visualization.
The _network_sa_ folder contains scripts for calculating network surface area using workbench_command.
The _stats_ folder contains scripts for manuscript figure generation and statistical analyses.
README files can be found in each folder, so please see these for additional details.

## Tutorial Guide
Step-by-step walkthroughs are avaialble for the multi-echo preprocessing and MS-HBM parcellation steps on NeuroDocs (https://neurodocs.readthedocs.io/en/latest/).

## Contact
For questions concerning script usage, please contact us through our lab webpage: https://brain.byu.edu/contact. Any questions regarding the usage of software/pipelines developed by other labs (e.g., fMRIprep) should be directed to their respective forums. Best of luck!
