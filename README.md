# death-penalty-favourability

# Gauging national suppoprt of the death penalty
In this paper I use multilevel regression with post-stratification methods to track the national opinion on support for the death penalty from 2015 and 2018

## File structure
'data' folder is where the raw data files should go. There are four data files that were used in this study: (i) 2018 and 2015 post-stratification data from the Census Bureau's American Community Survey (ACS); (ii) 2018 and 2015 Pew Research Centre Monthly Political survey. I am not allowed to redistribute these data files. Users are expected to obtain the data files with persmission from the official websites. Clean data will also be pushed here. 

'scripts' folder contains the R scripts that we used to clean the datasets. Also used to create plots.

'visuals' is where any pdfs (minus the official report) or images made are put. This includes plots in the paper. Again, we are not allowed to share the data files.

## Reproducibility
1. Download the raw data files from the ACS and Pew Research Centre website. Place them in the 'data' folder.
2. Run the data cleaning scripts that we have provided in the 'scripts' folder. In order for the scripts to run correctly, ensure that the filepath and filenames in the code match with the ones on your system.
3. Ensure that the cleaned data files from step 2 are placed in the 'data' folder and plots are in the 'visuals' folder.
4. In the repository, you will find the .Rmd file to generate the report. This should match the corresponding pdf here. 


