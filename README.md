FormatAnalysis
==============

# Analysis Workflow 
This repository contains an analysis workflow written in R which calculates format technology 
lifecycles. As an input it accepts format profiles with accompanying time stamps. 
One good example is UK Web Archive Format Profile dataset (DOI:10.5259/ukwa.ds.2/fmt/1). 

## Input Data
The dataset representing a format profile should follow the following format. 
Set of characterization outputs coming from the same source (e.g. coming from the same tool) 
should be separated by TAB. There isn't a limit on how many sources could be used. 
Different values from the same source (representing different properties such as format version) are 
separated by ;
Last two columns of the dataset should contain a timestamp and the amount of files. 
Good dataset example is UK Web Archive Format Profile dataset (DOI:10.5259/ukwa.ds.2/fmt/1).
 
## Execution
To execute the workflow main.R needs to be executed. 
In the config.R configuration parameters can be set. One of the parameters is the path to the 
file whhich contains data for analysis. note that the UK Web Archive Format Profile dataset is not included with
this workflow but needs to be retrieved separately. 

The workflow expects "output data" (without quotes) folder in the Analysis folder.  

# Dependencies
To execute this workflow R environment is required. 
Additional package required is minpack.lm (http://cran.r-project.org/web/packages/minpack.lm/index.html) 
     
