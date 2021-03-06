# A random bayesian analysis tool

## Overview of the files in the repo

- **www** - The pipedrive logo lives in this folder because in R-shiny custom pictures used from a local machine have to be in a folder named "www"
- **abayesian_project.Rproj** is the project file for the tool. It is not necessary to open this in R for the tool to work but it is advised.
- **back_end.Rmd** is an Rmarkdown document with the back-end code for the tool as it was first developed and conceptualized (you could view this as a very rough 0 version of the tool). The code includes a little more in depth comments about the inner workings of the tool and also detailed descriptions for the output tables and graphs. This can be knit to html in Rmarkdown, but for that you need a .csv data file and manually refer to it in this code (in the section where raw data is first  explored; search: "andmed <- read_csv(download (5).csv") and also enter conversion numbers manually into the code (in the conversion rate analysis part of the code where n = visitors, conversion = conversions). **NB! It should be noted that this is a very rough draft of the end result and there is some code and descriptions in this file that were removed or changed in the front-end development process and hence do not apply to the final product"**. 
- **bayesian_analysis_applet.R** This is the "executable file" that should be opened and "Run" in r-studio to make the tool work. In essence it is the back-end code wrapped in the R-shiny syntax to create the user interface for the tool. There are big parts of the back-end version that were changed for different reasons during the front-end development of this code (from changing colours to removing some visualisations and outputs alltogether). This also includes comments and descriptions of what the code does.

### Enjoy!
