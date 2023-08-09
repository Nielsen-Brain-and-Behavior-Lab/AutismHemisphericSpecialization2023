#!/bin/bash

#SBATCH --time=25:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=51250M   # memory per CPU core
#SBATCH -J "UT_preproc"   # job name

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

#Load git and matlab modules
#export LD_PRELOAD=/usr/lib/fastx/latest/virtualgl/usr/lib64/libdlfaker.so:/usr/lib/fastx/latest/virtualgl/usr/lib64/libvglfaker.so

ml python/3.6
conda init bash
conda activate /fslgroup/fslg_autism_asym/software/anaconda3/envs/CBIG_py3

ml git/2.14
ml matlab/r2018b
LD_PRELOAD= matlab &
unset DISPLAY



#Set paths! ADD YOUR NETID: 
HOME=/fslgroup/fslg_spec_networks/compute
code_Dir=${HOME}/code/Utah_analysis/CBIG2016_preproc_ALL
CBIG_Dir=/fslgroup/fslg_rdoc/compute/CBIG
out_Dir=/fslgroup/grp_proc/compute/Utah_analysis/CBIG2016_preproc_FS6
subj=$1
subj_Dir=/fslgroup/grp_proc/compute/Utah_analysis/Utah_BIDS/${subj}

mkdir -p ${out_Dir}

# 'source' -> YOUR <- config script (with software paths)
source ${code_Dir}/CBIG_preproc_tested_config_funconn.sh

# Run the preprocessing pipeline
${CBIG_Dir}/stable_projects/preprocessing/CBIG_fMRI_Preproc2016/CBIG_preproc_fMRI_preprocess.csh \
	-s ${subj} \
	-output_d ${out_Dir}/${subj} \
	-anat_s ${subj} \
	-anat_d ${subj_Dir}/anat \
	-fmrinii ${subj_Dir}/${subj}_fmrinii.txt \
	-config ${code_Dir}/example_config.txt 

