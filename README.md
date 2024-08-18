# Galaxiids
Data Repository for "Social and metabolic mediation of growth performance in a temperate estuarine fish".

Files:

  1 - Respirometry Data.xlsx contains raw slopes from respirometry experiments for all fish across both stages (6 experimental days). Also shows how those slopes were used to calculate SMR, RMR, and MMR for each fish.

  2 - Mass Standardization.xlsx shows calculations to mass-standardize SMR, RMR, and MMR calculations for all fish. 

  3 - Growth and MO2 Rates.xlsx contains both SGR and MO2 data for this study. Formatted for use with the R Script (Script_08182024) to produce mass-standardized SGR values and generate figures 1 and 3 in the main text, as well as Supp. Fig. 2.

  4 - Behavior Data.xlsx contains behavior data for this study. Used in the R Script (Script_08182024) to statistically test differences between tank group behaviors.

  Script_08182024.R (replaces 03252024.R) is the main R script used for data analysis and figure generation in this study. Requires loading in data from 3 - Growth and MO2 Rates.xlsx, as well as 4 - Behavior Data. xlsx.

  Supplementary Materials.docx contains supplementary figures and tables.

  Outlier-analysis.pdf shows annotated statistical analyses used to determine which data were outliers for modeling purposes.
