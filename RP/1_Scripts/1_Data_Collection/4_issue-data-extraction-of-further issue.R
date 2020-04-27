#AUTHOR @Sebastiano Panichella

library(stringr)

#path output
path_projectsInfo_output<-"../Projects-list/projectsInfo-additional-non-wontfix-data-RQ3-without-duplicates2.csv"

wontfix_pattern<-"wontfix|(won%27t%20fix)|(resolved%3Awontfix)|(closed%3Awontfix)|(wont-fix)|(Won%27t%20Fix)|(not-fixing)|(Status-WontFix)|WontFix|(status%3A%20will%20not%20fix)|(Cannot%20fix)"

#path input

# path dataset containing the projects with manually labeled wontfix issues
path_projectsInfo<-"../Projects-list/projectsInfo-output_with_issue_urls-full-data-LABELED_categories_merged-without-duplicates.csv"

# path dataset containing the information about all labels of all projects 
path_projectsInfo2<-"../Projects-list/projectsInfo-14-06-2018.csv"

# dataset containing the projects with manually labeled wontfix issues
projectsInfo<- read.csv(path_projectsInfo)

# we select the projects with manually labeled wontfix issues
pos_projects_to_consider<- which(projectsInfo$Label2!="")
projectsInfo<-projectsInfo[pos_projects_to_consider,]

# dataset containing the information about all labels of all projects 
projectsInfo2<- read.csv(path_projectsInfo2)

projectsInfo[1,]

projectsInfo2[1,]

projects_in_projectsInfo<- unique(projectsInfo$project_name)
projects_in_projectsInfo2<- unique(projectsInfo2$project_name)

projects_to_consider <- intersect(projects_in_projectsInfo,projects_in_projectsInfo2)
#projects_to_consider <- projects_in_projectsInfo2

projectsInfo2$all_issue_labels<- as.character(projectsInfo2$all_issue_labels)

projectsInfo2$project_name <- as.character(projectsInfo2$project_name)
projectsInfo2$project_url <- as.character(projectsInfo2$project_url)
projectsInfo2$project_language <- as.character(projectsInfo2$project_language)
projectsInfo2$all_issue_labels <- as.character(projectsInfo2$all_issue_labels)

#number of non-wantfix issue to select for "each project"
number_issues_per_project<- 200

cols<- colnames(projectsInfo2)[1:2]
cols<- c(cols,"issue_url","TitleIssue","DescriptionIssue")
projectsInfo3<-vector(mode <- "list",length= length(cols))
names(projectsInfo3) <- cols 

names(projectsInfo3)


#TASK 1: with this for cycle we collect the link to the issue for each project..

i<-2

#for(i in 6:10 )#length(projects_to_consider))
for(i in 1:length(projects_to_consider)) 
 {
  
  project_to_consider<-projects_to_consider[i]
  
  pos_project_to_consider <- which( projectsInfo2$project_name==project_to_consider)
  
  all_issue_labels<-strsplit(projectsInfo2$all_issue_labels[pos_project_to_consider],split="####")[[1]]
  # we select only non wantfix labels
  non_wontfix_issue_labels<-all_issue_labels[which(!str_detect(all_issue_labels,wontfix_pattern))]
 
  if(length(non_wontfix_issue_labels)>0){
  url_non_wontfix_issue<- paste("https://github.com/",non_wontfix_issue_labels,sep="")
  url_non_wontfix_issue<- str_replace(url_non_wontfix_issue, "/labels/","/issues?q=label%3A")
  url_non_wontfix_issue<- paste(url_non_wontfix_issue,"+is%3Aclosed",sep="")
  # we read the page to see whether there are "non wontfix issues" until we collect 
  #"number_issues_per_project" issues
  
    if(length(url_non_wontfix_issue)>1){
      u<- 1
      #while until we have navigate all non-wantfix issues or we reached 
      # the "number_issues_per_project" number of issues...
      while( u <= length(url_non_wontfix_issue) && sum(projectsInfo3$project_name==project_to_consider)< number_issues_per_project ){
      con <- file(url_non_wontfix_issue[u], "r")
      page<- readLines(con) # empty
      close(con)
        if(sum(str_detect(page,"O Closed"))==0){
          links_non_wontfix_issues_discussions<-"" #initialization
          p<-1
          end<-0
          while(p< number_issues_per_project && end==0){
            url<- str_replace(url_non_wontfix_issue[u],"page=1&",paste("page=",p,"&",sep=""))
            con <- file(url, "r")
            page<- readLines(con) # empty
            close(con)
            Sys.sleep(0.5)# 0.5 second
            if(sum(str_detect(page,"No results matched your search"))>0)
            {
              end<-1
            }
            #CASE 2: we do not have an empty page...
            if(sum(str_detect(page,"No results matched your search"))==0)
            {
              #we extract the "links to the wontfix_issues_discussions"
              pattern<- paste("href=\"/",project_to_consider,"/issues/[0-9]+\"",sep="")
              pattern2<- paste("issue_[0-9]+_link",sep="")
              #links_non_wontfix_issues_discussions1 <- page[which(str_detect(page,pattern))]
              #links_non_wontfix_issues_discussions1 <- unique(str_extract(links_non_wontfix_issues_discussions1,pattern))
              #to obtain the full links pattern:
              #links_non_wontfix_issues_discussions1 <- str_replace(links_non_wontfix_issues_discussions1,"href=\"","https://github.com")
              #links_non_wontfix_issues_discussions1 <- str_replace(links_non_wontfix_issues_discussions1,"\"","")
              links_non_wontfix_issues_discussions2 <- page[which(str_detect(page,pattern2))]
              links_non_wontfix_issues_discussions2 <- unique(str_extract(links_non_wontfix_issues_discussions2,pattern2))
              links_non_wontfix_issues_discussions2 <- str_replace(links_non_wontfix_issues_discussions2,"issue_","")
              links_non_wontfix_issues_discussions2 <- str_replace(links_non_wontfix_issues_discussions2,"_link","")
              links_non_wontfix_issues_discussions <-paste("https://github.com/",project_to_consider,"/issues/",links_non_wontfix_issues_discussions2,sep="")
              
              if(length(links_non_wontfix_issues_discussions)>0)
              {
                d<-1
                while(d <= length(links_non_wontfix_issues_discussions) && (sum(projectsInfo3$project_name==project_to_consider)< number_issues_per_project) ){
                  if(sum(projectsInfo3$issue_url==links_non_wontfix_issues_discussions[d])==0)
                    { pos<- length(projectsInfo3$project_name)+1
                      projectsInfo3$project_name[pos] <- project_to_consider
                      projectsInfo3$project_url[pos] <-  projectsInfo2$project_url[pos_project_to_consider]
                      projectsInfo3$project_language[pos] <-  projectsInfo2$project_language[pos_project_to_consider]
                      projectsInfo3$issue_url[pos] <-  links_non_wontfix_issues_discussions[d]
                     }  
                  d <- d+1
                }# while d
              }#if links_non_wontfix_issues_discussions
            }
            Sys.sleep(0.45)# 0.25 second
            if(p%%15 ==0){
              Sys.sleep(65)# 0.5 second
            }
            print(paste(i,"out of", length(projects_to_consider)," - issue link ", u,"out of",length(url_non_wontfix_issue)," - issue page ", p,"out of (the maximum)",500))
            p<- p+1
          } #while p
          Sys.sleep(0.35)# 0.25 second
          u<- u + 1
         }
      Sys.sleep(0.35)# 0.25 second
       }#while
    }
  }
} 

#run from here...
projectsInfo3$project_name <- as.character(projectsInfo3$project_name)
projectsInfo3$project_url <- as.character(projectsInfo3$project_url)
projectsInfo3$issue_url <- as.character(projectsInfo3$issue_url)
projectsInfo3$TitleIssue <- as.character(projectsInfo3$TitleIssue)
projectsInfo3$DescriptionIssue <- as.character(projectsInfo3$DescriptionIssue)

#TASK 2: with this for cycle we collect the TITLE AND DESCRIPTION for each project..
i<-1
for(i in 1:length(projectsInfo3$project_name)){
  # we read the page of the issue to extract detailed information..
  con <- file(projectsInfo3$issue_url[i], "r")
  page<- readLines(con) # empty
  close(con)
  #WE EXTRACT   TitleIssue, DescriptionIssue, AllCommentsIssue and meanCommentSize
  #1) TitleIssue
  TitleIssue<- page[which(str_detect(page,"<title>"))]
  TitleIssue<- str_extract(TitleIssue,"<title>(.)+ Issue #[0-9]+ ")
  TitleIssue<- str_replace(TitleIssue,"<title>","")
  TitleIssue<- str_replace_all(TitleIssue,"&quot;","\"")
  TitleIssue<- str_replace_all(TitleIssue,"<U+00B7>|Issue #[0-9]+ ","\"")
  projectsInfo3$TitleIssue[i] <-  TitleIssue
  #2) AllCommentsIssue and DescriptionIssue
    pattern_issue_messages<-"td class=\"d-block comment-body markdown-body  js-comment-body\""
    positions_AllCommentsIssue<- which(str_detect(page,pattern_issue_messages))
    length_page<- length(page)
    #we try to extract all the comments (starting from the issue description). 
    AllCommentsIssue<-""
    c<-1
    for(c in 1:length(positions_AllCommentsIssue)){
      # we extract the sub page starting from the current issue message
       sub_page<-page[ positions_AllCommentsIssue[c] : length_page]
       pos_start_current_message<-3
       pos_end_current_message<- which(str_detect(sub_page, "</td>"))[1] -1
       currentComment<- paste(sub_page[pos_start_current_message:pos_end_current_message], collapse="")
       currentComment<- str_replace_all(currentComment,"<p>|</p>"," ")
       AllCommentsIssue[c]<-currentComment
    }
  projectsInfo3$DescriptionIssue[i] <- AllCommentsIssue[1]
  #projectsInfo3$AllCommentsIssue[i] <- paste(AllCommentsIssue, collapse="####")
  #we compute the mean comment size
  
  print(paste(i,"out of", length(projectsInfo3$project_name)))
  
  Sys.sleep(0.35)# 0.25 second
  Sys.sleep(0.45)# 0.25 second
  if(p%%45 ==0){
    Sys.sleep(55)# 0.5 second
  }
}

write.csv(projectsInfo3,path_projectsInfo_output,row.names=FALSE)

projectsInfo4<-read.csv(path_projectsInfo_output)

positions<- which(is.na(projectsInfo4$DescriptionIssue) && is.na(projectsInfo4$TitleIssue))

if(length(positions) >0)
{
  projectsInfo4 <- projectsInfo4[-c(positions),]
}

projectsInfo4$project_language<-"C#"
projectsInfo4 <- unique(projectsInfo4)

write.csv(projectsInfo4,path_projectsInfo_output,row.names=FALSE)



