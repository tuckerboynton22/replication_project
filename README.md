# Gov 52 Final Replication Project
## Spring 2021
### Tucker Boynton
Replicating *An Economic Evaluation of the Moneyball Hypothesis* by Jahn K. Hakes and Raymond D. Sauer (2006)

## Files
Within this repository are the files necessary to generate the tables and figures from Hakes and Sauer's 2006 paper as well as a PDF copy of my own replication.

Under the "/data" directory, you will find "frontier201.dta," which contains salary index and win percentage for various teams from 1986 to 2006, "teamaggs.xlsx," which contains statistics aggregated at the team level for the years 1999-2003, "playerseasons.xlsx," which contains individual player salary data from 2000-2004 and lagged performance variables from 1999-2003, and "lewis205b.dta," which contains more detailed player data than the previous file from 1986-2005.

Under the "/scripts" directory, you will find "figures and tables.R," which has the script to generate all of the necessary figures and tables, and "replication.Rmd," which is the file that contains all written text as well as the aforementioned script to generate figures and tables.

Within the "/scripts" directory is the "/bibliography" directory, where you will find the bibliography for the replication project under "bibliography.bib"

Finally, "replication.pdf" is in the parent directory and is a PDF of my version of the knitted R Markdown file that resides in the "/scripts" directory.

## How to Run
In order to run this code on your local device, download the full repository, set the working directory to wherever you choose to store the repo, open the "replication.Rmd"" file, and click "Knit." Once all dependencies are installed, the file should run without a hitch, and you will get an output file titled "replication.pdf" in your "/scripts" directory with the compiled text, figures, and tables.

If you wish to run only the code to create the graphics, follow the same first steps, but open and run the "figures and tables.R" file instead. Note that although running this file will display the figures, it will not save them to your device, and all tables will be outputted in LaTeX.

If you would like to alter the figures and tables in the .pdf file produced by "replication.Rmd," you can edit the code chunks within the file.

## Computational Environment
R version 4.0.5 (2021-03-31)  
Platform: x86_64-apple-darwin17.0 (64-bit)  
Running under: macOS Catalina 10.15.7  

## Packages
rmarkdown_2.7  
tidyverse_1.3.0  
ggplot2_3.3.3  
ggtext_0.1.1  
readxl_1.3.1  
ggrepel_0.9.1  
knitr_1.31  
stargazer_5.2.2  
gt_0.2.2  
foreign_0.8.81  