#!/bin/bash

#SBATCH --job-name=predCC-traj  # Job name
#SBATCH --array=1-10     # Number of jobs
#SBATCH --partition=long    # Queue. Use long for run time >8h and standard otherwise
#SBATCH --mem=1G # Memory limit, e.g. reserve 1 GB memory 
#SBATCH --output=output/analysis-predictionCC-traj/slurm-%x-%a-%J.out # Direct output to subdirectory
#SBATCH --error=output/analysis-predictionCC-traj/slurm-%x-%a-%J.out # Same file for output and error

R CMD BATCH --vanilla code-data-analysis/analysis-predictionCC-traj.R output/analysis-predictionCC-traj/R-$SLURM_JOB_NAME-$SLURM_ARRAY_TASK_ID-$SLURM_JOB_ID.Rout

## go to directory    ## cd h:/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/
                      ## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/
## clean outputs      ## rm -r ./output/analysis-predictionCC-traj/*
## clean results      ## rm -r ./results/analysis-predictionCC-traj/*
## submission command ## sbatch SUBM_analysis-predictionCC-traj.slurm

## submission output  ## Submitted batch job 1268537 (time using C-c t: torsdag 22-04-21 at 16:34) 

## init               ## module module load gcc/11.2.0 R/4.1.2
                      ## cat .bash_profile (in the root directory on the server to visualize the current configuration)
                      ## vi .bash_profile (in the root directory on the server to modify the current configuration)

## commands           ## squeue           : view current jobs 
                      ## squeue -u id     : view current jobs for user id
                      ## scancel          : delete the job with job id 1034 from the queue type
                      ## sinfo            : view state of the queues
                      ## sshare           : check own usage of the server
                        
## documentation      ## from SLURM: https://slurm.schedmd.com/pdfs/summary.pdf: key functions
                      ##           : https://slurm.schedmd.com/sbatch.html (filename pattern)
                      ## from KU   : https://hpc.ku.dk/documentation/slurm.html
                      ##           : https://github.com/aejensen/computing-biostat-ucph/blob/main/README.md
