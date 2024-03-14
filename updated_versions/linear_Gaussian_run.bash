#!/bin/bash -l
#SBATCH --job-name linear_Gaussian_run
#SBATCH --output linear_Gaussian_run_%A_%a.txt
#SBATCH --error linear_Gaussian_run_%A_%a.txt
#SBATCH --account=jkarvane
#SBATCH --partition large
#SBATCH --time 3:00:00
#SBATCH --ntasks 2
#SBATCH --mem-per-cpu=16000
#SBATCH --array=1-100

module load r-env

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/jkarvane/tmp" >> ~/.Renviron

# Run the R script
srun apptainer_wrapper exec Rscript --no-save linear_Gaussian_run.R $SLURM_ARRAY_TASK_ID



