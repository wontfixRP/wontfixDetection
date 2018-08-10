
library(stringr)

path_non_wontiFixData<-"../Projects-list/projectsInfo-additional-wontfix-data-RQ3.csv"
non_wontiFixData <- read.csv(path_non_wontiFixData)

path_wontiFixData<-"../Projects-list/projectsInfo-output_with_issue_urls-full-data-LABELED.csv"
wontiFixData <- read.csv(path_wontiFixData)

# we have extracted projects of 4 program languages: C - Java - JavaScript (or TypeScript) - Ruby 
languages<- as.character(unique(non_wontiFixData$project_language))
# languages_patterns is equal to "C|Java|JavaScript#TypeScript|Ruby"
languages_patterns<- paste(languages,collapse="|") 
languages_patterns[3]<-"JavaScript#TypeScript"

non_wontiFixData$project_language<- as.character(non_wontiFixData$project_language)
wontiFixData$project_language<- as.character(wontiFixData$project_language)

N_ISSUES_1 <- sum(wontiFixData$project_language==languages[1]) + sum(non_wontiFixData$project_language==languages[1])
print(paste("Num. total issues of ",languages[1]," projects =",N_ISSUES_1))

N_ISSUES_2 <- sum(wontiFixData$project_language==languages[2]) + sum(non_wontiFixData$project_language==languages[2])
print(paste("Num. total issues of ",languages[2]," projects =",N_ISSUES_2))

languages_patterns_three<-str_replace(languages_patterns[3],"#","|")
N_ISSUES_3 <- sum(str_detect(wontiFixData$project_language,languages_patterns_three)) + sum(str_detect(non_wontiFixData$project_language,languages_patterns_three))
print(paste("Num. total issues of ",languages_patterns_three," projects =",N_ISSUES_3))

N_ISSUES_4 <- sum(wontiFixData$project_language==languages[4]) + sum(non_wontiFixData$project_language==languages[4])
print(paste("Num. total issues of ",languages[4]," projects =",N_ISSUES_4))

#Number of issues in total:
N_ISSUES_tot <- N_ISSUES_1+N_ISSUES_2+N_ISSUES_3+N_ISSUES_4
print(paste("Num. total issues =",N_ISSUES_tot))

#Wantfix issues..

N_ISSUES_1 <- sum(wontiFixData$project_language==languages[1])
print(paste("Num. total wantfix issues of ",languages[1]," projects =",N_ISSUES_1))

N_ISSUES_2 <- sum(wontiFixData$project_language==languages[2])
print(paste("Num. total wantfix issues of ",languages[2]," projects =",N_ISSUES_2))

languages_patterns_three<-str_replace(languages_patterns[3],"#","|")
N_ISSUES_3 <-  sum(str_detect(wontiFixData$project_language,languages_patterns_three)) 
print(paste("Num. total wantfix issues of ",languages[3]," projects =",N_ISSUES_3))

N_ISSUES_4 <- sum(wontiFixData$project_language==languages[4]) 
print(paste("Num. total wantfix issues of ",languages[4]," projects =",N_ISSUES_4))

#Number of wantfix issues in total:
N_ISSUES <- N_ISSUES_1+N_ISSUES_2+N_ISSUES_3+N_ISSUES_4
print(paste("Num. total wantfix issues =",N_ISSUES))

#Non-wantfix issues..

N_ISSUES_1 <- sum(non_wontiFixData$project_language==languages[1])
print(paste("Num. total non-wantfix issues of ",languages[1]," projects =",N_ISSUES_1))

N_ISSUES_2 <- sum(non_wontiFixData$project_language==languages[2])
print(paste("Num. total non-wantfix issues of ",languages[2]," projects =",N_ISSUES_2))

N_ISSUES_3 <- sum(str_detect(non_wontiFixData$project_language,languages_patterns_three))
print(paste("Num. total non-wantfix issues of ",languages[3]," projects =",N_ISSUES_3))

N_ISSUES_4 <- sum(non_wontiFixData$project_language==languages[4]) 
print(paste("Num. total non-wantfix issues of ",languages[4]," projects =",N_ISSUES_4))

#Number of wantfix issues in total:
N_ISSUES <- N_ISSUES_1+N_ISSUES_2+N_ISSUES_3+N_ISSUES_4
print(paste("Num. total non-wantfix issues =",N_ISSUES))

#Nr. projects..

N_projects_1 <- length(unique(wontiFixData$project_name[which(wontiFixData$project_language==languages[1])])) + length(unique(non_wontiFixData$project_name[which(non_wontiFixData$project_language==languages[1])])) 
print(paste("Num. total projects of ",languages[1]," language =",N_projects_1))

N_projects_2 <- length(unique(wontiFixData$project_name[which(wontiFixData$project_language==languages[2])])) + length(unique(non_wontiFixData$project_name[which(non_wontiFixData$project_language==languages[2])])) 
print(paste("Num. total projects of ",languages[2]," language =",N_projects_2))

N_projects_3 <- length(unique(wontiFixData$project_name[which(str_detect(wontiFixData$project_language,languages_patterns_three))])) + length(unique(non_wontiFixData$project_name[which(str_detect(non_wontiFixData$project_language,languages_patterns_three))])) 
print(paste("Num. total projects of ",languages[3]," language =",N_projects_3))

N_projects_4 <- length(unique(wontiFixData$project_name[which(wontiFixData$project_language==languages[4])])) + length(unique(non_wontiFixData$project_name[which(non_wontiFixData$project_language==languages[4])])) 
print(paste("Num. total projects of ",languages[4]," language =",N_projects_4))

#Number of total projects:
N_projects <- N_projects_1+N_projects_2+N_projects_3+N_projects_4
print(paste("Num. total projects =",N_projects))


#Number of total projects with wontfix issues:
N_projects <- length(unique(wontiFixData$project_name))
print(paste("Num. total projects with wontfix issues=",N_projects))

#Number of total projects without wontfix issues:
N_projects <- length(unique(non_wontiFixData$project_name))
print(paste("Num. total projects without wontfix issues=",N_projects))

#We compute the average time to close a wontfix issue:
timeToCloseIssue<- difftime(strptime(as.character(wontiFixData$issue_closing_date),format = "%d.%m.%Y %H:%M"),strptime(as.character(wontiFixData$date_first_comment),format = "%d.%m.%Y %H:%M"),units = "days")
print(paste("average fixing time of wontfix issues=",mean(na.omit(as.numeric(timeToCloseIssue)))))




