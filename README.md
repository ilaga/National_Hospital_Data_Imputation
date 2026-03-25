# National Hospital Data Imputation

Manuscript code for "From Reporting Gaps to Hospital Cost Drivers to Enhance Digital Health Decision Making: A Machine Learning-Assisted Analysis of National Hospital Data"

## Project Structure

The code is organized as follows:

```
project-root/
|-- *.R (analysis scripts)
|-- Data/
|-- Figures/
|-- Results/
```

## File Descriptions

### Core Analysis Scripts

- **aha_analysis.R**  
  Primary analysis script. Performs data cleaning, imputation, sensitivity analysis, and model fitting (both imputed and complete case analyses).

- **geospatial_analysis.R**  
  Secondary analysis script. Evaluates spatial correlation of fitted models and outcome variables; produces spatial plots.

- **imputation_comparison.R**  
  Cluster-optimized script. Evaluates performance of each imputation method for each variable using cross-validation. (Note: computationally intensive)

- **imputation_results.R**  
  Post-processing script. Summarizes and processes results from `imputation_comparison.R`.

## Note

The data used for the manuscript cannot be shared publicly.
