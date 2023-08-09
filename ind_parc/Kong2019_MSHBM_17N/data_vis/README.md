# Parcellation Visualization

## Description
These scripts are used to convert the .mat parcellation files into FreeSurfer annotation files and then Workbench-compatible gifti files. Gifti surface underlays are also available here.

## Contents
1. parc2annot.m Calls on the CBIG function CBIG_SaveParcellationToFreesurferAnnotation.
2. convert_annot2gii_fs6_individual.sh Projects annotation files to the fsaverage6 surface for visualization.
3. lh.pial_infl2.surf.gii LH fsaverage6 surface for visualizing individual parcellations.
4. rh.pial_infl2.surf.gii RH fsaverage6 surface for visualizing individual parcellations.
