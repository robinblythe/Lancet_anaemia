# Lancet_anaemia
Anaemia modelling work

GBD data, pregnancy data, and stillbirth data are all included in the repository.

Files to note:
0_Functions loads functions required to summarise GBD data by year. Can be run without changing anything
1_Load_and_wrangle imports, prepares, and summarises the data for viewing. To run on a local machine, set the "import" object filepath to your local machine, and set the read.csv function call to the filenames.
2_Estimands runs scripts 0 and 1 and will be used to estimate effectiveness.
000_app is the app file. Currently what I'm working on.
99_Update_appdata is required if any changes are made to the data to keep the app up to date.
Constants is a dump file for a lot of the details, mostly to declutter the app file.

Data files:
countries is a list of the 196 countries we have data for (some exclusions like Andorra)
data is the data file that the app loads. Reading in csv files every time is bothersome so this is much faster
