# A random bayesian analysis tool

## Overview of the files in the repo

- abayesian_project.Rproj is the project file for the tool. It is not necessary to open this in R for the tool to work but for further development it would be wise to open this in R.
- back_end.Rmd is an Rmarkdown document with the backend code for tool as it was first developed and conceptualized (you could view this as a very rough 0.0 version of the tool). The file includes somewhat more in depth comments about the inner workings of the tool and also detailed descriptions about the output tables and graphs. For a clearer reading this should be knit in Rmarkdown. **NB! It should be noted that this is a very rough draft of the end result and there is some code and descriptions in this file that were removed or changed and is just unnecessary and does not apply to the final product"**. 
- bayesian_analysis_applet.R This is the file that should be opened and "Run" in r-studio to make the tool work. In essence it is the back-end code in the R-shiny wrapped in the R-shiny front-end development syntax to create the user interface for the tool. There is no sepparate CSS file because it was used on very few occasion inside this file and is not necessary. For futher front-end customization a sepparate CSS file should be created.
