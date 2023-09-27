# Lancet_anaemia
Anaemia modelling work

The data for Armenia and Malawi are included in the Git repository to be loaded immediately

There are three R files currently in use:
0_Functions loads functions required to summarise GBD data by year. Can be run without changing anything
1_Load_and_wrangle imports, prepares, and summarises the data for viewing. To run on a local machine, set the "import" object filepath to your local machine, and set the read.csv function call to the filenames.
2_Estimands runs scripts 0 and 1 and will be used to estimate effectiveness.
