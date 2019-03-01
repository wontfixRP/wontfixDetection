library(stringr)


languages<- c("C","Java","JavaScript","Ruby")
minimum_number_of_pages<-100

path_projectsInfo<-"../Projects-list/projectsInfo.csv"

projectsInfo<-list(project_name=c(),project_url=c(),project_language=c(),all_issue_labels=c())

i<-1
for(i in 1: length(languages)){

j<-1
for(j in 1:minimum_number_of_pages){
  #we access to the page containing the project fo the given program language...
  C_SHARP<- paste("https://github.com/search?l=C%23&o=desc&p=",j,"&q=language%3A",languages[i],"%23&ref=advsearch&s=stars&type=Repositories&utf8=%E2%9C%93",sep="")
  con <- file(C_SHARP, "r")
  page<- readLines(con) # empty
  close(con)
  #we focus on the pages of the 
  projects_in_the_page <- page[which(str_detect(page, "quot;https://github.com/"))]
  projects_in_the_page<- str_extract(projects_in_the_page,"quot;https://github.com/(.)+&quot;[}]")
  projects_in_the_page<- str_replace_all(projects_in_the_page,"quot;|&quot;[}]","")
  #for each project in the page we collect some initial information (e.g., project_name, project_url, project_language, all_issue_labels)
    p<-1 
    if(is.na(projects_in_the_page)!=TRUE){
    for(p in 1: length(projects_in_the_page)){
     pos<- length(projectsInfo$project_name)+1
     #we populate some initial information in the list related to the current project...
     projectsInfo$project_url[pos] <- projects_in_the_page[p]
     projectsInfo$project_language[pos] <- languages[i]
     projectsInfo$project_name[pos] <- str_replace(projects_in_the_page[p],"https://github.com/","")
     
     con <- file(paste(projectsInfo$project_url[pos],"/labels",sep=""), "r", blocking = FALSE)
     project_labels_page<- readLines(con) # empty
     close(con)
     #labels pattern...
     labels_pattern<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/",sep="")
     
     #CASE 1: WE DO NOT HAVE LABELS
     if(sum(str_detect(project_labels_page,labels_pattern))==0){
       projectsInfo$all_issue_labels[pos] <- "NA"
     }
     #CASE 2: WE HAVE LABELS
     if(sum(str_detect(project_labels_page,labels_pattern))>0){
        #until there are other labels, we colect them...
       labels_pattern<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/",sep="")
       l<-2
       for(l in 2: minimum_number_of_pages ){
          con <- file(paste(projectsInfo$project_url[pos],"/labels?page=",l,sep=""), "r", blocking = FALSE)
          temp <- readLines(con) # empty
          close(con)
          if(sum(str_detect(temp,labels_pattern))==0){
            break;
          }
          project_labels_page<- c(project_labels_page,temp)
        }
        #we use the labels pattern to detect the labels of the current project
        labels_current_project<- project_labels_page[which(str_detect(project_labels_page, labels_pattern))]
        labels_pattern2<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/(.)+\" style=",sep="")
        labels_current_project<- str_extract(labels_current_project,labels_pattern2)
        labels_current_project<- str_replace_all(labels_current_project,"href=\"|\" style=","")
        #we store the labels separating them using the "####" special sequence.
        projectsInfo$all_issue_labels[pos] <- paste(labels_current_project, collapse="####")
      }

     print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages)," page \"", j, " out of ",minimum_number_of_pages," project \"", p, " out of ",length(projects_in_the_page),sep=""))
     Sys.sleep(0.5)# 1 second
     }# for p
   }# if 
  print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages)," page \"", j, " out of ",minimum_number_of_pages,sep=""))
  write.csv(projectsInfo, path_projectsInfo,row.names=FALSE)
  Sys.sleep(40)# 40 seconds
  }# for j
print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages),sep=""))
Sys.sleep(60*7)# 10 minutes
}# for i


write.csv(projectsInfo, path_projectsInfo,row.names=FALSE)

# TASK2: We try to add additional issue using another methodology
i<-1
for(i in 1: length(languages)){
  
  j<-1
  for(j in 1:minimum_number_of_pages){
    #we access to the page containing the project fo the given program language...
    C_SHARP<- paste("https://github.com/search?o=desc&p=",j,"&q=language%3A",languages[i],"%23+wontfix&s=updated&type=Issues",sep="")
    con <- file(C_SHARP, "r")
    page<- readLines(con) # empty
    close(con)
    #we focus on the pages of the 
    projects_in_the_page <- page[which(str_detect(page, "quot;https://github.com/(.)+/issues"))]
    projects_in_the_page<- str_extract(projects_in_the_page,"quot;https://github.com/(.)+/issues/[0-9]+&quot;[}]")
    projects_in_the_page<- str_replace_all(projects_in_the_page,"quot;|/issues/[0-9]+&quot;[}]","")
    #for each project in the page we collect some initial information (e.g., project_name, project_url, project_language, all_issue_labels)
    p<-1 
    if(is.na(projects_in_the_page[pos])!=TRUE){
    for(p in 1: length(projects_in_the_page)){
      pos<- length(projectsInfo$project_name)+1
      #we populate some initial information in the list related to the current project...
      projectsInfo$project_url[pos] <- projects_in_the_page[p]
      projectsInfo$project_language[pos] <- languages[i]
      projectsInfo$project_name[pos] <- str_replace(projects_in_the_page[p],"https://github.com/","")
      
      con <- file(paste(projectsInfo$project_url[pos],"/labels",sep=""), "r", blocking = FALSE)
      project_labels_page<- readLines(con) # empty
      close(con)
      #labels pattern...
      labels_pattern<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/",sep="")
      
      #CASE 1: WE DO NOT HAVE LABELS
      if(sum(str_detect(project_labels_page,labels_pattern))==0){
        projectsInfo$all_issue_labels[pos] <- "NA"
      }
      #CASE 2: WE HAVE LABELS
      if(sum(str_detect(project_labels_page,labels_pattern))>0){
        #until there are other labels, we colect them...
        labels_pattern<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/",sep="")
        l<-2
        for(l in 2: minimum_number_of_pages ){
          con <- file(paste(projectsInfo$project_url[pos],"/labels?page=",l,sep=""))
          temp <- readLines(con) # empty
          close(con)
          if(sum(str_detect(temp,labels_pattern))==0){
            break;
          }
          project_labels_page<- c(project_labels_page,temp)
        }
        #we use the labels pattern to detect the labels of the current project
        labels_current_project<- project_labels_page[which(str_detect(project_labels_page, labels_pattern))]
        labels_pattern2<-paste("href=\"/",projectsInfo$project_name[pos],"/labels/(.)+\" style=",sep="")
        labels_current_project<- str_extract(labels_current_project,labels_pattern2)
        labels_current_project<- str_replace_all(labels_current_project,"href=\"|\" style=","")
        #we store the labels separating them using the "####" special sequence.
        projectsInfo$all_issue_labels[pos] <- paste(labels_current_project, collapse="####")
      }
      
      print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages)," page \"", j, " out of ",minimum_number_of_pages," project \"", p, " out of ",length(projects_in_the_page),sep=""))
      Sys.sleep(0.5)# 1 second
    }# for p
    }# if
    print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages)," page \"", j, " out of ",minimum_number_of_pages,sep=""))
    write.csv(projectsInfo, path_projectsInfo,row.names=FALSE)
    Sys.sleep(40)# 40 seconds
  }# for j
  print(paste("language \"", languages[i],"\", ",i, " out of ",length(languages),sep=""))
  Sys.sleep(60*7)# 10 minutes
}# for i

projectsInfo2 <- as.data.frame(projectsInfo)
projectsInfo2 <- unique(projectsInfo2)
write.csv(projectsInfo2, path_projectsInfo,row.names=FALSE)

# TASK3: We extract the data of the identified projects

projects_to_analyze <- NULL

issues_data_template<- list(project=c(),git_url=c(),label=c(),issue_description=c(),link_issue=c())

topic1<- c("status:wontfix","wontfix","Resolution-Won't Fix")

topics<- list(topic1=c(),topic2=c(),topic3=c(),topic4=c(),topic5=c())
topics[[1]]<- topic1
topics[[2]]<- topic2
topics[[3]]<- topic3
topics[[4]]<- topic4
topics[[5]]<- topic5

i<-1

for(i in 10: length(projects_to_analyze[,1]) )
  {
  issues_data <- issues_data_template #initialization
  project_to_analyze <- projects_to_analyze[i,]
  
  con <- file(paste(project_to_analyze$git_url,"/labels",sep=""), "r", blocking = FALSE)
  issue_labels_page1 <- readLines(con) # empty
  close(con)
  
  issue_labels_pages<-0 #initialization
  #total number of issues
  number_issues <- issue_labels_page1[which(str_detect(issue_labels_page1,"class=\"Counter\">"))]
  number_issues <- str_extract(number_issues,"[0-9]+([,][0-9]+)?")
  number_issues <- str_replace(number_issues[1],"[,]",".")
  
  number_issue_pages <- issue_labels_page1[which(str_detect(issue_labels_page1,"Previous"))]
  
  if(length(number_issue_pages) > 0)
  {
  number_issue_pages <- str_replace_all(str_extract_all(number_issue_pages,">[0-9]+<")[[1]] , ">|<" , "")
  number_issue_pages<- number_issue_pages[length(number_issue_pages)]
  number_issue_pages<- as.numeric(number_issue_pages)
  }
  
  if(length(number_issue_pages) == 0)
  {
    number_issue_pages <- 1
  }
  
    # we compute all_labels 
    all_labels<- list(label=c(),link=c())
    # if there are other pages reserved to the label we collect them.

      k<-1
      for( k in 1:number_issue_pages){
        con <- file(paste(project_to_analyze$git_url,"/labels?page=",k,"&sort=name-asc",sep=""), "r", blocking = FALSE)
        issue_labels_page_k <- readLines(con) # empty
        close(con)
        
        labels_page_k <- issue_labels_page_k[which(str_detect(issue_labels_page_k,"<span class=\"label-name\">"))]
        
        labels_page_k <- str_replace_all(labels_page_k, "<span class=\"label-name\">|</span>|( )+" , " ")
        labels_page_k <- trimws(labels_page_k)
        
        all_labels$label <- unique(c( all_labels$label , labels_page_k ))
        # we identify the labels in the page
        positions_labels <-  (length(all_labels$label) - length(labels_page_k) + 1 ) : length(all_labels$label)
         # we add the links to the labels...
        all_labels$link[positions_labels] <- paste(project_to_analyze$git_url,"/labels/",all_labels$label[positions_labels],sep="")
        
      }
  
  
   all_labels$label <- str_replace_all( all_labels$label, "[*]", "" )
   # we identify the valid labels and work only on them..
   valid_labels <- all_labels # initialization
   valid_labels$is_valid <- seq( from = 1, to = length(valid_labels$label) ) 
   valid_labels <- as.data.frame(valid_labels)

  valid_labels[,1] <- as.character(valid_labels[,1])
  valid_labels[,2] <- as.character(valid_labels[,2])
  
   l<-1
   for(l in 1: length(valid_labels[,1]) )
   {
     t<-1
     for(t in 1: length(topics) ) {
     
       is_valid_label<- topics[[t]] == tolower(valid_labels$label[l] )
       
       is_valid_label<- sum(is_valid_label)
       # the label is valid...
       if(is_valid_label > 0 )
       {
         valid_labels$is_valid[l]<- "TRUE"
       }
       print(paste("project",i, " running - out of ",length(projects_to_analyze[,1]) , " - label to validate",l,"out of",length(valid_labels[,1])))
     }
     
    }
  
  valid_labels$is_valid[valid_labels$is_valid!="TRUE"]="FALSE"
  
  print(paste( " we need to only extract the descriptions of the valid issue types -  for project",i,"out of",length(projects_to_analyze[,1]) )) 
  
  directory_project<-paste(path_base,project_to_analyze$project,sep="")
  if(!file.exists(directory_project))
  {
    dir.create(directory_project)
  }
  
  directory_project_issues<-paste(directory_project,"/issues/",sep="")
  if(!file.exists(directory_project_issues))
  {
    dir.create(directory_project_issues)
  }
  
  path_all_labels <-paste(directory_project,"/all_labels.csv",sep="")
  path_valid_labels<-paste(directory_project,"/valid_labels.csv",sep="")
  
  write.csv(all_labels,path_all_labels,row.names=FALSE)
  write.csv(valid_labels,path_valid_labels,row.names=FALSE)
  
  
  
  v<-1
   for(v in 1:  length(valid_labels$label))
   {
     
     if(valid_labels$is_valid[v] ==  "TRUE")
      {
       print(paste( "  for project",i,"out of",length(projects_to_analyze[,1]), "we analyze issue type:", valid_labels$label[v]  )) 
       
       # relevant information before the for loop..
       counter<-1
       con <- file(paste(paste(str_replace_all(valid_labels$link[v]," ","%20")),"?page=",counter,sep=""), "r", blocking = FALSE)
       issue_page <- readLines(con) # empty
       close(con)
       
       
       number_issues_page <- issue_page[which(str_detect(issue_page,"Previous"))]
       
       if(length(number_issues_page) > 0)
       {
         number_issues_page <- str_replace_all(str_extract_all(number_issues_page,">[0-9]+<")[[1]] , ">|<" , "")
         number_issues_page<- number_issues_page[length(number_issues_page)]
         number_issues_page<- as.numeric(number_issues_page)
       }
       
       if(length(number_issues_page) == 0)
       {
         number_issues_page <- 1
       }
       
       issues <- "" 
       counter<-1
        for (counter in 1:number_issues_page)
         {
          con <- file(paste(str_replace_all(valid_labels$link[v]," ","%20"),"?page=",counter,sep=""), "r", blocking = FALSE)
          issue_page <- readLines(con) # empty
          close(con)
          issues <-  c(issues, issue_page[which(str_detect(issue_page,"link-gray-dark no-underline h4 js-navigation-open")) +1])
         }
       
       path_issues_type_v <-paste(directory_project_issues,str_replace_all(valid_labels$label[v],"/","_"),".csv",sep="")
       
       write.csv(issues,path_issues_type_v,row.names=FALSE)
       
      } # if v
    } # for v
  
  print(paste("project",i, " done - out of ",length(projects_to_analyze[,1]) ))
  }





