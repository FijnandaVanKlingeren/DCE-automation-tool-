# DCE-automation-tool-
R, Python and RMarkdown code for the partial automation of creating and conducting a Discrete Choice Experiment 

Note: all files fall under an MIT License as listed in the file "LICENSE" 

This repository holds the following files:

1. DCE_automation_step1_orthogonal_array_210202023  is an R-code file that contains the R-code to create an orthogonal array from Qualtrics input coming from the Collectieve Kracht DKE tool intake survey (https://erasmusuniversity.eu.qualtrics.com/jfe/form/SV_3UxuyC9IwywzA8e ). 
This code will produce two files: one csv file called 'survey_questions' and a csv file called 'dce_questions' that will be used in the python code. Examples of 'dce_questions.csv' and 'survey_questions.csv' are also added to the repository. This is what the orthogonal array file and the survey questions file coming from the Rcode should look like. 

2. 'fill_in_template'  and 'settings' are Python files that should be run together to transform 'dce_questions.csv' and 'survey_questions.csv' into a Qualtrics survey qsf file. An example qsf file is "DCE-Voorbeeld". "DCE-Voorbeeld" is also the name of the survey output from the Python code. You can load DCE-Voorbeeld.qsf into Qualtrics and rename it for use. Make sure to add a page of participant information and consent as the first page of the Qualtrics survey.
Make sure to have all python related files in the same project folder when working in PyCharm, including the 'ck_logo' image. The template file 'template_DCE_modest_border.qsf' dictates how the DCE survey will look like. Currently it is styled according to the Collectieve Kracht theme. If anything needs to be changed regarding the looks of the DCE survey, this can be done in that file. 

3. 'DCE_automation_step3_RMarkdown_results_generation_21022023' is RMarkdown code that transforms the results of the Qualtrics survey (the survey that was created in previous steps) into a research report. Make sure to fill in all the paths to the right files (orthogonal array, intake data, and collectieve kracht logo)

4. 'Researcher_Manual_DCE' is a pdf file serving as a sort of manual for the researcher who wants to use the DCE tool, including information on the DCE method itself as well as the process of using the R and Python files to create and analyse the DCE for citizen collectives. 

5. 'Wat is een Discrete Keuze Experiment' is a pdf file with explanation (in Dutch) and examples of a Discrete Choice Experiment, which may be shared with prospective citizen collectives who consider a DCE. 
