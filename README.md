# simulating_counterfactuals

R code for the paper J. Karvanen, S. Tikka, M. Vihola (2024) Simulating counterfactuals. Journal of Artificial Intelligence Research, 80, 835-857.
[https://doi.org/10.1613/jair.1.15579]

Files:
- linear_Gaussian_setup.R, Setting up the simulation with linear Gaussian models (Section 4 in the paper)
- linear_Gaussian_run.R, Running the simulation with linear Gaussian models (Section 4 in the paper)
- linear_Gaussian_run.bash, the array job instructions (SLURM) for linear_Gaussian_run.R. (Section 4 in the paper)
- linear_Gaussian_reporting.R, Reporting the results of the simulation with linear Gaussian models (Section 4 in the paper)
- fairness_example.R, Example of fairness evaluation in credit scoring (Section 5 in the paper)

The files work with R6causal 0.8.3 (or later) and reproduce the results in the paper (arXiv v3).
