# DCE-automation-tool-
R, Python and RMarkdown code for the partial automation of creating and conducting a Discrete Choice Experiment 


This repository holds the following codes:

1. DCE_automation_step1_orthogonal_array_210202023  is an R-code file that contains the R-code to create an orthogonal array from Qualtrics input coming from the Collectieve Kracht DKE tool intake survey (https://erasmusuniversity.eu.qualtrics.com/jfe/form/SV_3UxuyC9IwywzA8e ). This code will produce a csv file called 'array' that can be used in the python files. An example of 'array.csv' is also added to the repository. This is what the array file coming from the Rcode should look like. It may be necessary to remove the apostrophies from the csv file before you can use it for the Python files. 

2. 'fill_in_template'  and 'settings' are Python files that should be run together to transform 'array.csv' into a Qualtrics survey qsf file. 

3. 'DCE_automation_step3_RMarkdown_results_generation_21022023' is RMarkdown code that transforms the results of the Qualtrics survey (the survey that was created in previous steps) into a research report. 
