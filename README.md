# Lancet Haematology commission on anaemia targets
This repository houses the code for the anaemia modelling study currently under review at Lancet Haematology.

The code contains all data and code required to run the main analysis. It can be run from the Setting_targets.Rmd file.
Required data are all contained in the Data folder, and turned into R objects using the 0_Create_dataset.R file.

File index:
- 0_Create_dataset.R turns csv files into R objects and transforms them into formats required for the analysis.
- 1_Run_model.R is the file that is run each time an iteration of the simulation is required. It is called through a source() function in the markdown file.
- 99_Functions.R produces the functions required for data transformation and analysis (some are helper functions)
- No_coverage.R is a sensitivity analysis to determine whether setting current coverage of interventions to 0 has an impact on the results. Outputs from this R file are stored in the Sensivitity_analysis folder.
- Setting_targets.Rmd is the file containing the code chunks required for the simulation as well as a brief explanation of the methods. A more comprehensive methods explanation is available in the supplementary information of the main paper. Results from the analysis in this file are stored in the Results folder.
- The 'targets' files (.csv) are just the median [95% uncertainty intervals] of the results from all simulations. They are made available so that country-level results can quickly be queried by anyone interested in the final results.

The simulations are somewhat computationally expensive for home laptops. It may take around 2 hours to run on a regular professional laptop, and around 15 minutes on high performance machines.
CAUTION: it is recommended to briefly read up on parallel processing if you are unfamiliar with the process. The parallel computing step in the Rmarkdown document may slow down your computer.

For queries about the methods or this GitHub repository, reach out to me at r.blythe@duke-nus.edu.sg or robin.blythe@qut.edu.au.
