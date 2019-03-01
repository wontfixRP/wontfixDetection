library(stringr)


#path output
path_projectsInfo_output<-"../Projects-list/projectsInfo-output.csv"

path_projectsInfo_output_with_issue_urls<-"../Projects-list/projectsInfo-output_with_issue_urls.csv"

path_projectsInfo_output_with_issue_urls_additional_data<-"../Projects-list/projectsInfo-output_with_issue_urls-additional-data.csv"

path_projectsInfo_output_with_issue_urls_full_data<-"../Projects-list/projectsInfo-output_with_issue_urls-full-data.csv"

wontfix_pattern<-"wontfix|(won%27t%20fix)|(resolved%3Awontfix)|(closed%3Awontfix)|(wont-fix)|(Won%27t%20Fix)|(not-fixing)|(Status-WontFix)|WontFix|(status%3A%20will%20not%20fix)|(Cannot%20fix)"


# TASK1: we extract all the fine-grained data from the "closed issues"

#path input
path_projectsInfo<-"../Projects-list/projectsInfo-14-06-2018.csv"


projectsInfo<- read.csv(path_projectsInfo)

projectsInfo[1,]

projectsInfo$hasWontFix<-""
#nCommentsT = number of comments the issue has received
#nActorsT = number of unique people that partecipated to the issue discussion

projectsInfo$all_issue_labels<- as.character(projectsInfo$all_issue_labels)
counter<-0
i<-1
for(i in 1:length(projectsInfo$project_name)){
  all_issue_labels<-strsplit(projectsInfo$all_issue_labels[i],split="####")[[1]]
  wontfix_issue_labels<-all_issue_labels[which(str_detect(all_issue_labels,wontfix_pattern))]
  #CODE TO FIND WONTFIX PATTERS..
#   for(p in 2001:3000){
#     labels<-strsplit(projectsInfo$all_issue_labels[p] ,split="####")[[1]]
#     if(!is.na(labels))
#     if(sum(str_detect(labels,wontfix_pattern))==0)
#       {print(paste("p",p,labels))}
#   }
  #END CODE TO FIND WONTFIX PATTERS..
  #we have to obtain somethinglike: https://github.com/MattRix/Futile/issues?q=label%3Awontfix+is%3Aclosed
  # or https://github.com/lime-company/powerauth-push-server/issues?q=is%3Aissue+is%3Aclosed+label%3Abug
  if(length(wontfix_issue_labels)>0){
  url_wontfix_issue<- paste("https://github.com/",wontfix_issue_labels,sep="")
  url_wontfix_issue<- str_replace(url_wontfix_issue, "/labels/","/issues?q=label%3A")
  url_wontfix_issue<- paste(url_wontfix_issue,"+is%3Aclosed",sep="")
  # we read the page to see whether there are "wontfix issues"
  con <- file(url_wontfix_issue[1], "r")
  page<- readLines(con) # empty
  close(con)
  page2<-""
  if(length(url_wontfix_issue)>1){
    con <- file(url_wontfix_issue[2], "r")
    page2<- readLines(con) # empty
    close(con)
    if(sum(str_detect(page2,"O Closed"))==0){
      print(paste("FOUND",i,"out of", length(projectsInfo$project_name)))
      #break;
      projectsInfo$hasWontFix[i]<- url_wontfix_issue[2]
     }
    }
  
  print(paste(i,"out of", length(projectsInfo$project_name)))
  if(sum(str_detect(page,"O Closed"))==0){
    counter<-counter+1
    print(paste("FOUND",i,"out of", length(projectsInfo$project_name)," - number of project with wontfix issues",counter))
    projectsInfo$hasWontFix[i]<- paste(projectsInfo$hasWontFix[i],"##",url_wontfix_issue[1],sep="")
    }
  }
} 

projectsInfo$hasWontFix<-as.character(projectsInfo$hasWontFix)
projectsInfo<-as.data.frame(projectsInfo)
projectsInfo<-projectsInfo[-which(projectsInfo$hasWontFix==""),]
write.csv(projectsInfo,path_projectsInfo_output,row.names=FALSE)



## TASK2: WE LOAD THE DATASET WE STORED and we collect fine grained-information (in projectsInfo2).
projectsInfo<- read.csv(path_projectsInfo_output)

projectsInfo$project_name <- as.character(projectsInfo$project_name)
projectsInfo$project_url <- as.character(projectsInfo$project_url)
projectsInfo$project_language <- as.character(projectsInfo$project_language)
cols<- colnames(projectsInfo)[-4]
cols<- c(cols,"issue_url","issue_labels", "nCommentsT","nActorsT")
projectsInfo2<-vector(mode <- "list",length= length(cols))
names(projectsInfo2) <- cols

projectsInfo[1,]
names(projectsInfo2)
projectsInfo$hasWontFix <- as.character(projectsInfo$hasWontFix)
i<-1

for(i in 1317: length(projectsInfo$project_name)){
  wontFixUrls<- strsplit(projectsInfo$hasWontFix[i],split="##")[[1]]
  
  wontFixUrls<- str_replace(wontFixUrls,"/issues[?]","/issues?page=1&")
    if(sum(wontFixUrls=="")>0){
      wontFixUrls<-wontFixUrls[-1]
      }
  l<-1
  for(l in 1:length(wontFixUrls)){
    con <- file(wontFixUrls[l], "r")
    page<- readLines(con) # empty
    close(con)
    #if there are some closed wontfix issues 
    if(sum(str_detect(page,"0 Closed"))==0){
        links_wontfix_issues_discussions<-"" #initialization
        p<-1
        end<-0
        while(p<50 && end==0){
          url<- str_replace(wontFixUrls[l],"page=1&",paste("page=",p,"&",sep=""))
          con <- file(url, "r")
          page<- readLines(con) # empty
          close(con)
          if(sum(str_detect(page,"No results matched your search"))>0)
          {
            end<-1
          }
             #CASE 2: we do not have an empty page...
             if(sum(str_detect(page,"No results matched your search"))==0)
              {
               #we extract the "links to the wontfix_issues_discussions"
               pattern<- paste("href=\"/",projectsInfo$project_name[i],"/issues/[0-9]+\"",sep="")
               links_wontfix_issues_discussions <- page[which(str_detect(page,pattern))]
               links_wontfix_issues_discussions <- unique(str_extract(links_wontfix_issues_discussions,pattern))
               #to obtain the full links:
               links_wontfix_issues_discussions <- str_replace(links_wontfix_issues_discussions,"href=\"","https://github.com")
               links_wontfix_issues_discussions <- str_replace(links_wontfix_issues_discussions,"\"","")
                 d<-1
                 if(length(links_wontfix_issues_discussions)>0)
                  {
                   for(d in 1: length(links_wontfix_issues_discussions)){
                   pos<-length(projectsInfo2$project_name)+1
                  projectsInfo2$project_name[pos] <- projectsInfo$project_name[i]
                  projectsInfo2$project_url[pos] <-  projectsInfo$project_url[i]
                  projectsInfo2$project_language[pos] <-  projectsInfo$project_language[i]
                  projectsInfo2$issue_url[pos] <-  links_wontfix_issues_discussions[d]
                  }# for d
                 }#if links_wontfix_issues_discussions
             }
          Sys.sleep(0.20)# 0.25 second
          if(p%%15 ==0){
            Sys.sleep(65)# 0.5 second
          }
          print(paste(i,"out of", length(projectsInfo$project_name)," - issue link ", l,"out of",length(wontFixUrls)," - issue page ", p,"out of (the maximum)",500))
        p<- p+1
        } #while p
    }
    
    print(paste(i,"out of", length(projectsInfo$project_name)," - issue link ", l,"out of",length(wontFixUrls)))
    Sys.sleep(0.5)# 0.25 second
  }# for l
  Sys.sleep(7)# 0.5 second
  print(paste(i,"out of", length(projectsInfo$project_name)))
  if(i%%15 ==0){
    Sys.sleep(10)# 0.5 second
  }

  write.csv(projectsInfo2[c("project_name","project_url","project_language","issue_url")],path_projectsInfo_output_with_issue_urls,row.names=FALSE)
}#for i

write.csv(projectsInfo2[c("project_name","project_url","project_language","issue_url")],path_projectsInfo_output_with_issue_urls,row.names=FALSE)



#TASK3: we extract more detailed information..
projectsInfo3<- read.csv(path_projectsInfo_output_with_issue_urls)
projectsInfo3[1,]

#we create a new list with additional attributes (to populate).
cols<- colnames(projectsInfo3)
additional_cols<-c("issue_labels", "nCommentsT","nActorsT")
cols2<- c(cols,additional_cols)
projectsInfo4<-vector(mode <- "list",length= length(cols2))
names(projectsInfo4) <- cols2

#we print some results..
projectsInfo4[cols]<-projectsInfo3[cols]
names(projectsInfo4)
projectsInfo4[additional_cols]<-""

projectsInfo4$issue_url <- as.character(projectsInfo4$issue_url)

i<-1
#for(i in 1:4){
for(i in 1:length(projectsInfo4$project_name)){
  # we read the page of the issue to extract detailed information..
  con <- file(projectsInfo4$issue_url[i], "r")
  page<- readLines(con) # empty
  close(con)
  #SUMMARY OF RELEVANT PATTERNS..
  #pattern to detect the user  (posting the message): 
  #   ''''<a class="author text-inherit css-truncate-target" data-hovercard-user-id=\"[0-9]+\"''
  #5 or 7 lines after the line of user (posting the message) there is the date in the following format:
  #  2018-06-14T14:12:22Z -> pattern -> [0-9]{4}-[0-9]{2}-[0-9]{2}[A-z][0-9]{2}:[0-9]{2}:[0-9]{2}[A-z]
  #pattern related message:
  #   ''''lass="d-block comment-body markdown-body  js-comment-body"''''
  #pattern to detect the label of the issue:
  # ''''<a class="sidebar-labels-style box-shadow-none width-full d-block IssueLabel''''
  projectsInfo4$nCommentsT[i]<- sum(str_detect(page,"lass=\"d-block comment-body markdown-body  js-comment-body\""))
  users<- page[which(str_detect(page,"<a class=\"author text-inherit css-truncate-target\" data-hovercard-user-id=\""))]
  users <- unique(users)
  projectsInfo4$nActorsT[i]<- length(users) 
  labels<- page[which(str_detect(page,"<a class=\"sidebar-labels-style box-shadow-none width-full d-block IssueLabel"))]
  labels<- str_extract(labels,"\"max-width: 100%\">([a-z]|[A-z]|(-)|(/)|(_)|(:)|( )|[0-9])+</span></a>")
  #labels<- str_extract(labels,"title=\"([a-z]|[A-z]|(-)|(/)|(_)|(:)|( )|[0-9])+\"")
  #labels<- str_replace(labels,"title=\"","")
  #labels<- str_replace(labels,"\"","")
  #labels2<- page[which(str_detect(page,"<a class=\"sidebar-labels-style box-shadow-none width-full d-block IssueLabel"))]
  #labels<- str_extract(labels,"\"max-width: 100%\">([a-z]|[A-z]|(-)|(/)|(_)|(:)|( )|[0-9])+\"><span class=")
  labels<- str_replace(labels,"\"max-width: 100%\">","")
  labels<- str_replace(labels,"</span></a>","")
  
  projectsInfo4$issue_labels[i]<- paste(c(labels),collapse = ";")
  print(paste(i,"out of", length(projectsInfo4$project_name)))

}

write.csv(projectsInfo4,path_projectsInfo_output_with_issue_urls_additional_data,row.names=FALSE)

#TASK4: we extract all the remaining detailed information..
projectsInfo5 <-  read.csv(path_projectsInfo_output_with_issue_urls_additional_data)
projectsInfo5[1,]
projectsInfo5$issue_labels_additional_to_wontfix<- str_replace(projectsInfo5$issue_labels,wontfix_pattern,"")
projectsInfo5$issue_labels_additional_to_wontfix<- str_replace(projectsInfo5$issue_labels_additional_to_wontfix,";;",";")
projectsInfo5$issue_url<- as.character(projectsInfo5$issue_url)
projectsInfo5$issue_labels <- as.character(projectsInfo5$issue_labels)

#we create a list with additional columns and initialize them
cols<- colnames(projectsInfo5)
additional_cols<-c("date_first_comment","issue_closing_date","date_last_comment","timeToCloseIssue","timeToDiscussIssue","TitleIssue","DescriptionIssue","AllCommentsIssue","meanCommentSize")
cols2<- c(cols,additional_cols)
projectsInfo6<-vector(mode <- "list",length= length(cols2))
names(projectsInfo6) <- cols2

projectsInfo6[cols]<-projectsInfo5[cols]
names(projectsInfo6)
projectsInfo6[additional_cols]<- "" #  rep("",length(projectsInfo6$project_name)) #initialization

i<-1
#for(i in 1:4){
for(i in 1:length(projectsInfo6$project_name)){
  # we read the page of the issue to extract detailed information..
  con <- file(projectsInfo6$issue_url[i], "r")
  page<- readLines(con) # empty
  close(con)
  #SUMMARY OF RELEVANT PATTERNS..
  #pattern to detect the user  (posting the message): 
  #   ''''<a class="author text-inherit css-truncate-target" data-hovercard-user-id=\"[0-9]+\"''
  #5 or 7 lines after the line of user (posting the message) there is the date in the following format:
  #  2018-06-14T14:12:22Z -> pattern -> [0-9]{4}-[0-9]{2}-[0-9]{2}[A-z][0-9]{2}:[0-9]{2}:[0-9]{2}[A-z]
  #pattern related to issue messages:
  #   ''''<title>''''
  #pattern related to issue messages (the first message is the issue description):
  #   ''''td class="d-block comment-body markdown-body  js-comment-body"''''
  
  #WE EXTRACT THE DATE OF ISSUE OPENING, THE CLOSING DATE AND THE DATE OF THE LAST COMMENT.
  dates<- page[which(str_detect(page,"[0-9]{4}-[0-9]{2}-[0-9]{2}[A-z][0-9]{2}:[0-9]{2}:[0-9]{2}[A-z]"))]
  dates<- str_extract(dates,"[0-9]{4}-[0-9]{2}-[0-9]{2}[A-z][0-9]{2}:[0-9]{2}:[0-9]{2}[A-z]")
  dates<-unique(dates)
  date_last_comment <- strptime(dates[length(dates)], format = "%Y-%m-%dT%H:%M:%S")
  date_first_comment <- strptime(dates[1], format = "%Y-%m-%dT%H:%M:%S")
  issue_closing_date<- page[which(str_detect(page,"closed this"))+3]
  issue_closing_date<- str_extract(issue_closing_date,"[0-9]{4}-[0-9]{2}-[0-9]{2}[A-z][0-9]{2}:[0-9]{2}:[0-9]{2}[A-z]")
  issue_closing_date<- strptime(issue_closing_date, format = "%Y-%m-%dT%H:%M:%S")
  if(length(issue_closing_date)==0){
    issue_closing_date<-date_last_comment
  }
      
    projectsInfo6$date_first_comment[i] <- as.character(date_first_comment)
    projectsInfo6$issue_closing_date[i] <- as.character(issue_closing_date)
    projectsInfo6$date_last_comment[i] <- as.character(date_last_comment)
    projectsInfo6$timeToCloseIssue[i] <- difftime(issue_closing_date,date_first_comment,units = "days")
    projectsInfo6$timeToDiscussIssue[i] <-  difftime(date_last_comment,date_first_comment,units = "days")
  #WE EXTRACT   TitleIssue, DescriptionIssue, AllCommentsIssue and meanCommentSize
  #1) TitleIssue
  TitleIssue<- page[which(str_detect(page,"<title>"))]
  TitleIssue<- str_extract(TitleIssue,"<title>(.)+ Issue #[0-9]+ ")
  TitleIssue<- str_replace(TitleIssue,"<title>","")
  TitleIssue<- str_replace_all(TitleIssue,"&quot;","\"")
  TitleIssue<- str_replace_all(TitleIssue,"<U+00B7>|Issue #[0-9]+ ","\"")
  projectsInfo6$TitleIssue[i] <-  TitleIssue
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
  projectsInfo6$DescriptionIssue[i] <- AllCommentsIssue[1]
  projectsInfo6$AllCommentsIssue[i] <- paste(AllCommentsIssue, collapse="####")
  #we compute the mean comment size
  projectsInfo6$meanCommentSize[i] <-   str_length(projectsInfo6$AllCommentsIssue[i])/  projectsInfo6$nCommentsT[i] 
  
  print(paste(i,"out of", length(projectsInfo6$project_name)))
  
}

write.csv(projectsInfo6,path_projectsInfo_output_with_issue_urls_full_data,row.names=FALSE)


