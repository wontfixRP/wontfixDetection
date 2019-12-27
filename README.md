# Replication Package for the paper entitled “Won’t We Fix this Issue?” Qualitative Characterization and Automated Identification of Wontfix Issues on GitHub

Description of the content:

1) "1_Scripts" contains all scripts used to generate the data and rawdata 
   (available in the "2_Data & Rawdata" folder) used to 
    answer the research questions of the research study. 
    
    Under "the sub-folder "1_Scripts/1_Data_Collection":
    
    a) "1_issue-data-extraction.R" is the R script we used to collect the initial information
       abou the C, Java, Javacript and Ruby projects
       
    b) "2_issue-fine-grained-data-extraction.R" and "3_issue-data-extraction-of-further issue.R"
       are the R scripts we used to extract more fine-grained data of wontfix issues,
       (e.g., issue title, issue comments, date of opening/closing the issue, etc.).
       
    Under "the sub-folder "2_Prediction-of-Wontfix-Issues":
    
    c) "4_(RQ3)-automatic-classification-of-wantfix-issues.R" is the R script we used to 
       automatically create training and test set matrices,
       and classify classify the issues for the considered C, Java, Javacript and Ruby projects.
       
       

2) "2_Data & Rawdata" folder contains all the data and rawdata used to answer the research questions
   of the research study. 
   	
   	a) "2_Data & Rawdata/1_Projects-list" folder contains all the data we obtained by running the script
   	    under "1_Scripts/1_Data_Collection".
   	    
   		
   	b) In "2_Rawdata - RQ1 - RQ2/RQ1_and_RQ2_raw.csv" are reported the M_opening and M_closing labels assigned 
      to each issue in the manual inspected sample, as well as the values of each collected metric.
      
    c) "3_Data & Rawdata - RQ3" folder contains the detailed information about
    
       - the training and test sets used to run the Weka tools ("Test-set" and "Training-set" folders
          under "3_Data & Rawdata - RQ3") 
          
       - the dataset used to run the Weka tools for the 10-fold validation ("10-fold cross-validation" folder
          under "3_Data & Rawdata - RQ3") 
          
       - the results of the prediction (under "3_Data & Rawdata - RQ3/Results-prediction") 
       
       - "WEKA-stable-3-8" folder contains the actual "Weka version used for this research"
           
        



