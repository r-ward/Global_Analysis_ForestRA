# Global_Analysis_ForestRA

This repository holds the scripts and data used for the global analysis of forest reproductive allocation described in the manuscript Ward et al. (in press).
Data will be released upon publication.


GlobalForestRA_analysis.r
  - this script calls source scripts from the /Functions folder to run analyses and generate all tables and figures in the main text

GlobalForestRA_SI.r
  - note that the main analysis must be run first to create the data objects used in the SI script
  - this script calls source scripts from the /Functions folder to generate all tables and figures in the SI

/Data
 - the file 'GlobalForestRA_data.csv' contains all litterfall data needed to run the analysis and SI scripts.
 - the file 'GlobalForestRA_metadata.pdf' contains descriptions of all variables in 'GlobalForestRA_data.csv'.
 
 The following data files are used to create Figure 1 and can be found at: https://data.ess-dive.lbl.gov/view/ess-dive-f7b4f8a695ba8e3-20250721T185348259
 - the file 'nph18131-sup-0003-tables2_TableS2_RAFluxdata.csv' contains data from Table S2 of the Supporting Information for Hanbury-Brown et al. (2022) needed to create Figure 1a (the original .xlsx file can be downloaded from https://nph.onlinelibrary.wiley.com/doi/full/10.1111/nph.18131). 
 - the file 'land_cover_classification_5000.tif' contains the MODIS land cover classification raster (Friedl & Sulla-Menashe 2019) needed to create Figure 1c. See 'Functions/create_forest_extent_map.r' for additional detail.
 
 
/Functions
  - functions are called in the main analysis and SI scripts to format data, fit models, and create plots and tables

/Output
  - tables and figures generated in the above scripts are saved here
  

Ward, R.E., Zhang-Zheng, H. Aernethy, K., Adu-Bredu, S., Arroyo, L., Bailey, A. et al. (in press). Forest age rivals climate to explain reproductive allocation patterns in forest ecosystems globally. Ecology Letters. 

Hanbury-Brown, A.R., Ward, R.E. & Kueppers, L.M. (2022). Forest regeneration within Earth system models: current process representations and ways forward. New Phytol., 235, 20–40.

Friedl, M., & Sulla-Menashe, D. (2019). MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V051 [Data set]. NASA EOSDIS Land Processes DAAC. 2019. Accessed January 21, 2024.




