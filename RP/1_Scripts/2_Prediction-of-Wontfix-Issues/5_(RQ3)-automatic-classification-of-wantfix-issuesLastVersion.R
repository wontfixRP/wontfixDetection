#AUTHOR @Sebastiano Panichella

library(stringr)
library("stringr")
# create a first textmatrix with some files
library(tm)
library(lsa)
library(stringr)
library(foreign)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_65.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava) #in case of problems install http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html, then run: "sudo R CMD javareconf" from command line
library(rpart)
library(bigmemory)
library(farff)
library(foreign)
library(rio) # install_formats()
### NEW CODE ADDED IN THE SECOND EMSE REVISION
library(textclean)
#test library
#x <- c(  "<bold>Random</bold> text with symbols: &nbsp; ;","hello, how are you?<br />")
#x<- replace_html(x)
### END NEW CODE ADDED IN THE SECOND EMSE REVISION

#library(JGR)

#library(RWeka)

deleteFiles<-function(folder){
  filesTodelete<-list.files(folder)
  i<-1
  for(i in 1:length(filesTodelete))
  {
    if(length(filesTodelete)>0)
    {
      #print(filesTodelete[i])
      file.remove(paste(folder,"/",filesTodelete[i],sep="")) 
    }#for..
  }#for..
}

### NEW CODE ADDED IN THE SECOND EMSE REVISION

#paths input
path_non_wontfix_issues_data_with_duplicates<-"../Projects-list/projectsInfo-additional-non-wontfix-data-RQ3.csv"
path_non_wontfix_issues_data_without_duplicates1<-"../Projects-list/projectsInfo-additional-non-wontfix-data-RQ3-without-duplicates.csv"
path_non_wontfix_issues_data_without_duplicates2<-"../Projects-list/projectsInfo-additional-non-wontfix-data-RQ3-without-duplicates2.csv"
#paths input2
path_RQ1_and_RQ2_raw_without_duplicates<-"../Projects-list/RQ1_and_RQ2_raw_without_duplicates.csv"

path_wontfix_issues_data<-"../Projects-list/projectsInfo-output_with_issue_urls-full-data-LABELED_categories_merged-without-duplicates.csv"
#path output: 
path_non_wontfix_issues_data_without_duplicates_final<-"../Projects-list/projectsInfo-additional-non-wontfix-data-RQ3-without-duplicates-final.csv"
path_RQ1_and_RQ2_raw_without_duplicates_final<-"../Projects-list/RQ1_and_RQ2_raw_without_duplicates_final.csv"

#first check
RQ1_and_RQ2_raw_without_duplicates<-read.csv(path_RQ1_and_RQ2_raw_without_duplicates)
length1<- length(RQ1_and_RQ2_raw_without_duplicates$issue_url)
length2<- length(unique(RQ1_and_RQ2_raw_without_duplicates$issue_url))
if(length1 == length2)
{
  print("all fine with \"RQ1_and_RQ2_raw_without_duplicates\" file")
  RQ1_and_RQ2_raw_without_duplicates_final <- RQ1_and_RQ2_raw_without_duplicates
}

if(length1 != length2)
{
  RQ1_and_RQ2_raw_without_duplicates_final <- unique(RQ1_and_RQ2_raw_without_duplicates)
  print("fixed problem with duplicates, created file \"RQ1_and_RQ2_raw_without_duplicates\" file")
}
write.csv(RQ1_and_RQ2_raw_without_duplicates_final,path_RQ1_and_RQ2_raw_without_duplicates_final,row.names=FALSE)


threshold_training_and_test_set<-50 #50%

check_non_wontfix_issues_data <- read.csv(path_non_wontfix_issues_data_with_duplicates)
wontfix_issues_data <- read.csv(path_wontfix_issues_data)


#we check unique of projects - to be sure that duplicates are removed
check_wontfix_issues_data<- unique(wontfix_issues_data[,1:3])
check_wontfix_issues_data <- check_wontfix_issues_data[order(check_wontfix_issues_data$project_name),]
check_wontfix_issues_data[1:3,]
#number of unique projects - without removing the language information
lenght1<- length(check_wontfix_issues_data$project_name)
#number of unique projects - after removing the language information
lenght2<-length(unique(check_wontfix_issues_data$project_name))
 if(lenght1 == lenght2)
 {
  print("Duplicated problem solved in wontfix issues") 
   print(paste("Unique language:", unique(check_wontfix_issues_data$project_language)))
 }
if(lenght1 != lenght2)
 {
   print(paste("Duplicated problem not solved in wontfix issues, still ", abs(lenght1-lenght2), "duplicated projects"))
}

#we make unique of projects - to be sure that duplicates are removed
check_non_wontfix_issues_data$project_language <- as.character(check_non_wontfix_issues_data$project_language)
#languages<- unique(check_non_wontfix_issues_data$project_language)
check_non_wontfix_issues_data$project_language<- str_replace(check_non_wontfix_issues_data$project_language,"C","C#")
check_non_wontfix_issues_data$project_language<- str_replace(check_non_wontfix_issues_data$project_language,"JavaScript","C#")
check_non_wontfix_issues_data$project_language<- str_replace(check_non_wontfix_issues_data$project_language,"Java","C#")
check_non_wontfix_issues_data$project_language<- str_replace(check_non_wontfix_issues_data$project_language,"Ruby","C#")
#languages<- unique(check_non_wontfix_issues_data$project_language)

check_non_wontfix_issues_data<- unique(check_non_wontfix_issues_data)
print("Duplicated problem solved in non-wontfix issues") 
print(paste("Unique language:", unique(check_non_wontfix_issues_data$project_language)))

#we combine the obtained file with "non_wontfix_issues_data_without_duplicates1" 
#and "non_wontfix_issues_data_without_duplicates2" and then we write it
non_wontfix_issues_data_without_duplicates1 <- read.csv(path_non_wontfix_issues_data_without_duplicates1)
non_wontfix_issues_data_without_duplicates2 <- read.csv(path_non_wontfix_issues_data_without_duplicates2)

non_wontfix_issues_data_without_duplicates1$project_language <- "C#"
non_wontfix_issues_data_without_duplicates2$project_language <- "C#"

#initialization
non_wontfix_issues_data<- check_non_wontfix_issues_data
non_wontfix_issues_data$issue_url<- as.character(non_wontfix_issues_data$issue_url)
non_wontfix_issues_data_without_duplicates1$issue_url<- as.character(non_wontfix_issues_data_without_duplicates1$issue_url)

i<- 1
for(i in 1:length(non_wontfix_issues_data_without_duplicates1$project_name))
{
  #if the line is not duplicated, we add it
  if(sum(non_wontfix_issues_data_without_duplicates1$issue_url[i]==non_wontfix_issues_data$issue_url) == 0)
  {
    temp<- non_wontfix_issues_data_without_duplicates1[i,]
    non_wontfix_issues_data <- rbind(non_wontfix_issues_data,temp)
  }
}

i<- 1
for(i in 1:length(non_wontfix_issues_data_without_duplicates2$project_name))
{
  #if the line is not duplicated, we add it
  if(sum(non_wontfix_issues_data_without_duplicates2$issue_url[i]==non_wontfix_issues_data$issue_url) == 0)
  {
    temp<- non_wontfix_issues_data_without_duplicates2[i,]
    non_wontfix_issues_data <- rbind(non_wontfix_issues_data,temp)
  }
}

non_wontfix_issues_data <- unique(non_wontfix_issues_data)
#length(non_wontfix_issues_data$project_name)

write.csv(non_wontfix_issues_data,path_non_wontfix_issues_data_without_duplicates_final,row.names=FALSE)

### END NEW CODE ADDED IN THE SECOND EMSE REVISION

data(stopwords_en)

path_weka<-"../WEKA-stable-3-8/weka/"

# paths output matrixs
path_data_nfr<- "../Data-set/matrix_data_nfr.csv"
path_data_nfr_arff<- "../Data-set/matrix_data_nfr.arff"

#temporarily folder...
td<-"../temp_folder_baseline"


td_trainingSet<-paste(td,"training_set", sep="/")
td_testSet<-paste(td,"test_set", sep="/")

deleteFiles(td_trainingSet)
deleteFiles(td_testSet)

#lines added/modified in the EMSE major revision
stats1<-non_wontfix_issues_data[,c(1,6)]
stats2<-wontfix_issues_data[,c(1,3)]
stats3<- rbind(stats1,stats2)
p_names<-as.character(unique(stats3$project_name))
# we compute the number of issues per project

### NEW CODE ADDED IN THE SECOND EMSE REVISION
i<-1
# issue_pp_C<-0
issue_pp_C_sharp<-0
# issue_pp_Java<-0
# issue_pp_JavaScript<-0
# issue_pp_Ruby<-0
for(i in 1: length(p_names))
{
  issue_pp_C_sharp<-c(issue_pp_C_sharp,length(which(stats3$project_name==p_names[i])))
   if(unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "C#")
   {
     issue_pp_C_sharp<-c(issue_pp_C_sharp,length(which(stats3$project_name==p_names[i])))
   }
  # issue_pp<-c(issue_pp,length(which(stats3$project_name==p_names[i])))
  # if(unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "C")
  # {
  #   issue_pp_C<-c(issue_pp_C,length(which(stats3$project_name==p_names[i])))
  # }
  # if(unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "Java")
  # {
  #   issue_pp_Java<-c(issue_pp_Java,length(which(stats3$project_name==p_names[i])))
  # }
  # if ( (unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "JavaScript") || (unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "TypeScript"))
  # {
  #   issue_pp_JavaScript<-c(issue_pp_JavaScript,length(which(stats3$project_name==p_names[i])))
  # }
  # if(unique(stats3$project_language[which(stats3$project_name==p_names[i])]) == "Ruby")
  # {
  #   issue_pp_Ruby<-c(issue_pp_Ruby,length(which(stats3$project_name==p_names[i])))
  # }
}
# issue_pp_C<- issue_pp_C[-1]
# median(issue_pp_C)
# issue_pp_Java<- issue_pp_Java[-1]
# median(issue_pp_Java)
# issue_pp_JavaScript<- issue_pp_JavaScript[-1]
# median(issue_pp_JavaScript)
# issue_pp_Ruby<- issue_pp_Ruby[-1]
# median(issue_pp_Ruby)
# issue_pp<- issue_pp[-1]
# median(issue_pp)
 issue_pp_C_sharp<- issue_pp_C_sharp[-1]
 median(issue_pp_C_sharp)

 ### END NEW CODE ADDED IN THE SECOND EMSE REVISION

non_wontfix_issues_data$TitleIssue <- as.character(non_wontfix_issues_data$TitleIssue)
non_wontfix_issues_data$DescriptionIssue <- as.character(non_wontfix_issues_data$DescriptionIssue)
wontfix_issues_data$TitleIssue <- as.character(wontfix_issues_data$TitleIssue)
wontfix_issues_data$DescriptionIssue <- as.character(wontfix_issues_data$DescriptionIssue)

non_wontfix_issues_data$TitleIssue[1]
non_wontfix_issues_data$DescriptionIssue[1]
wontfix_issues_data$TitleIssue[1]
wontfix_issues_data$DescriptionIssue[1]

###  NEW CODE ADDED IN THE SECOND EMSE REVISION
#We add preprocessing steps to remove HTML tags, to remove potential informational noise
non_wontfix_issues_data$TitleIssue <- replace_html(non_wontfix_issues_data$TitleIssue)
non_wontfix_issues_data$DescriptionIssue <- replace_html(non_wontfix_issues_data$DescriptionIssue)
wontfix_issues_data$TitleIssue <- replace_html(wontfix_issues_data$TitleIssue)
wontfix_issues_data$DescriptionIssue <- replace_html(wontfix_issues_data$DescriptionIssue)
### END NEW CODE ADDED IN THE SECOND EMSE REVISION

###  NEW CODE ADDED IN THE SECOND EMSE REVISION: 
#Further pre-processing with regex https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html

non_wontfix_issues_data$TitleIssue<- str_replace_all(non_wontfix_issues_data$TitleIssue,"[:space:]+"," ")
non_wontfix_issues_data$TitleIssue<- str_replace_all(non_wontfix_issues_data$TitleIssue,"[:punct:]+"," ")
non_wontfix_issues_data$TitleIssue<- str_replace_all(non_wontfix_issues_data$TitleIssue,"[-]+","-")
non_wontfix_issues_data$DescriptionIssue<- str_replace_all(non_wontfix_issues_data$DescriptionIssue,"[:space:]+"," ")
non_wontfix_issues_data$DescriptionIssue<- str_replace_all(non_wontfix_issues_data$DescriptionIssue,"[:punct:]+"," ")
non_wontfix_issues_data$DescriptionIssue<- str_replace_all(non_wontfix_issues_data$DescriptionIssue,"[-]+","-")


wontfix_issues_data$TitleIssue<- str_replace_all(wontfix_issues_data$TitleIssue,"[:space:]+"," ")
wontfix_issues_data$TitleIssue<- str_replace_all(wontfix_issues_data$TitleIssue,"[:punct:]+"," ")
wontfix_issues_data$TitleIssue<- str_replace_all(wontfix_issues_data$TitleIssue,"[-]+","-")
wontfix_issues_data$DescriptionIssue<- str_replace_all(wontfix_issues_data$DescriptionIssue,"[:space:]+"," ")
wontfix_issues_data$DescriptionIssue<- str_replace_all(wontfix_issues_data$DescriptionIssue,"[:punct:]+"," ")
wontfix_issues_data$DescriptionIssue<- str_replace_all(wontfix_issues_data$DescriptionIssue,"[-]+","-")

###  END NEW CODE ADDED IN THE SECOND EMSE REVISION

half_wontfix_issues <- round(length(wontfix_issues_data$TitleIssue) / 2)

half_non_wontfix_issues <- round(length(non_wontfix_issues_data$TitleIssue) / 2)

#TRAINING SET PART
i<-1
for(i in 1: half_wontfix_issues)
{
  nameFile<-paste("wontfix-issue",i,".txt",sep="")  
  
  content<- paste(wontfix_issues_data$TitleIssue[i],wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep="")
  content<- str_replace_all(content,"[:space:]+"," ")
  content<- str_replace_all(content,"[:punct:]+"," ")
  content<- str_replace_all(content,"[-]+","-")
  write(content , file=paste(td,"training_set",nameFile, sep="/") )
}
i<-1
for(i in 1: half_non_wontfix_issues )
{
  nameFile<-paste("non_wontfix-issue",i,".txt",sep="") 
  content<- paste(non_wontfix_issues_data$TitleIssue[i],non_wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep="")
  content<- str_replace_all(content,"[:space:]+"," ")
  content<- str_replace_all(content,"[:punct:]+"," ")
  content<- str_replace_all(content,"[-]+","-")
  write( content, file=paste(td,"training_set",nameFile, sep="/") )
}

#TEST SET PART
i<-1
for(i in (half_wontfix_issues+1) : length(wontfix_issues_data$TitleIssue))
{
  nameFile<-paste("wontfix-issue",i,".txt",sep="")  
  content<- paste(wontfix_issues_data$TitleIssue[i],wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep="")
  content<- str_replace_all(content,"[:space:]+"," ")
  content<- str_replace_all(content,"[:punct:]+"," ")
  content<- str_replace_all(content,"[-]+","-")
  write( content, file=paste(td,"test_set",nameFile, sep="/") )
}
i<-1
for(i in (half_non_wontfix_issues+1) : length(non_wontfix_issues_data$TitleIssue) )
{
  nameFile<-paste("non_wontfix-issue",i,".txt",sep="")  
  content<- paste(non_wontfix_issues_data$TitleIssue[i],non_wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep="")
  content<- str_replace_all(content,"[:space:]+"," ")
  content<- str_replace_all(content,"[:punct:]+"," ")
  content<- str_replace_all(content,"[-]+","-")
  write( content, file=paste(td,"test_set",nameFile, sep="/") )
}

#INDEXING PROCESS 
csource <- paste(td,sep="") # corpus source
incorpus <- c(csource)

#incorpus <- c(paste(csource,"training_set",sep="/"),paste(csource,"test_set",sep="/"))
corpus<-Corpus(DirSource(directory=incorpus,ignore.case=TRUE,recursive = TRUE))
#indexing..
tm<-TermDocumentMatrix(corpus, control = list( stemming = TRUE,stopwords=TRUE,removePunctuation=TRUE,weighting =
                                                 function(x)
                                                   weightTfIdf(x, normalize =
                                                                 FALSE)))

docs_names<-colnames(tm)

terms_names<-rownames(tm)

num_docs <- length(list.files( td_trainingSet)) + length(list.files( td_testSet) )


matrix_term_by_doc<-matrix(0,length(terms_names)+1,num_docs)
colnames(matrix_term_by_doc)<-c( list.files( td_trainingSet)  , list.files( td_testSet)) ;
rownames(matrix_term_by_doc)<-c(terms_names,"is_wontfix");

#WE REPLACE to pos_j<-tm$j[z] #document position the corresponding position in "colnames(matrix_term_by_doc)"
indexes_documents<- unique(tm$j)
z<-1
# 1) we detect the positions
positions<-1 #initialization
where_to_replace <-1 #initialization
for(z in 1:length(indexes_documents))
{
  pos_j_in_the_matrix <- which(colnames(matrix_term_by_doc) == colnames(tm)[[indexes_documents[z]]])
  positions[z] <- pos_j_in_the_matrix
  # we try to see in "tm$j" the positions that will be used to replace the indexes
  temp<-which(tm$j==indexes_documents[z]) 
  where_to_replace[z] <- paste(temp,collapse=";")
  if(z %% 100 ==0 ||  z==length(indexes_documents))
  { 
    print(paste(z,"out of",length(indexes_documents)))
  }
}
# 2) we replace the positions
for(z in 1:length(positions))
{
  positions_to_replace<- as.numeric(strsplit(where_to_replace[z], split=";")[[1]])
  tm$v[positions_to_replace]<- positions[z]
  if(z %% 100 ==0 ||  z==length(positions))
  { 
    print(paste(z,"out of",length(positions)))
  }
}

z<-1
for(z in 1:length(tm$v))
{
  # if the id of the document in "tm$j[z]" is in the interval "of selected documents" 
  #then we use that value to populate the matrix...
  
  pos_i<-tm$i[z] #term position
  pos_j<-tm$j[z] #document position
  val<-tm$v[z]
  
  #we insert the "val" in the matrix only when the value is not equal to zero
  if(val!=0)
  {
    #pos_j_in_the_matrix<- which(colnames(matrix_term_by_doc) == colnames(tm)[[tm$j[z]]])
    #pos_j_in_the_matrix<- tm$j[z]
    
    matrix_term_by_doc[pos_i,pos_j]<-val
    
    if(z %% 100 ==0 ||  z==length(tm$v))
    { 
      print(paste(z,"out of",length(tm$v)))
    }
  }
}

# we remove tokens that are "numeric"
if(length(which(str_detect(rownames(matrix_term_by_doc),"[0-9+]")))>0)
{
  matrix_term_by_doc<-matrix_term_by_doc[-which(str_detect(rownames(matrix_term_by_doc),"[0-9+]")),]
}

###  NEW CODE ADDED IN THE SECOND EMSE REVISION
rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"[:space:]+"," ")
rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"[:punct:]+"," ")
rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"[-]+","-")

rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"└─","")
rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"├─","")
rownames(matrix_term_by_doc)<- str_replace_all(rownames(matrix_term_by_doc),"[─]+","-")

# we remove also strange, long terms
if(length(which(str_length(rownames(matrix_term_by_doc))>=15)) > 0 )
{
  matrix_term_by_doc<-matrix_term_by_doc[-which(str_length(rownames(matrix_term_by_doc))>=15),]
}
# we remove also strange, too short terms
if(length(which(str_length(rownames(matrix_term_by_doc))<=3)) > 0 )
{
  matrix_term_by_doc<-matrix_term_by_doc[-which(str_length(rownames(matrix_term_by_doc))<=3),]
}


#considered this list  https://www.w3schools.com/TAGS/default.ASP to define the stopword list
manual_stop_words_list<- c("bra","brbr","brdrb","aligncenterbrbrbr","brdrb","brdrl","brdrr","brdrt","doctypegrideditor","jabbr","willrendercanvas","xmlheader","frame","bdotnet","code","svg","svgfile","new","see","will","file","http","html","window","display","name","like","also","current")

m<-1
for(m in 1:length(manual_stop_words_list))
{
  if(length(which(str_detect(rownames(matrix_term_by_doc),manual_stop_words_list[m])))>0)
  {
    matrix_term_by_doc<-matrix_term_by_doc[-which(str_detect(rownames(matrix_term_by_doc),manual_stop_words_list[m])),]
  }
}


rownames(matrix_term_by_doc)[1:20]
length(rownames(matrix_term_by_doc))

###  END NEW CODE ADDED IN THE SECOND EMSE REVISION

# we see a sub part of the matrix
matrix_term_by_doc[1:3,1:3]
#matrix_term_by_doc[3470:3473,1:10]
#row.names(matrix_term_by_doc)

#line added in the second EMSE revision (normalization step)
matrix_term_by_doc2<- (matrix_term_by_doc-min(matrix_term_by_doc))/(max(matrix_term_by_doc)-min(matrix_term_by_doc))
matrix_term_by_doc2[1:3,1:3]

matrix_term_by_doc<- matrix_term_by_doc2

positions_non_wontfix<- which(str_detect(colnames(matrix_term_by_doc),"non_wontfix-issue[0-9]+(.)txt"))
positions_wontfix<- which(!str_detect(colnames(matrix_term_by_doc),"non_wontfix-issue[0-9]+(.)txt"))

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),positions_non_wontfix]="no"

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),positions_wontfix]="yes"

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]


positions_wontfix<- which(matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]=="yes")

positions_non_wontfix<- which(matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]=="no")


###  NEW CODE ADDED IN THE SECOND EMSE REVISION
  # we try to remov ethe problem of duplicated attribute, by renaming them (using ordinary information)
lenght_vocabulary<-length(rownames(matrix_term_by_doc)) #which is identical in the test set
vocabulary_terms_ids<- as.character(1:lenght_vocabulary)
rownames(matrix_term_by_doc)<- paste(vocabulary_terms_ids,"_",rownames(matrix_term_by_doc),sep="")

###  END NEW CODE ADDED IN THE SECOND EMSE REVISION

write.csv(t(matrix_term_by_doc),path_data_nfr,quote=FALSE)

convert(path_data_nfr, path_data_nfr_arff)

#WE MANIPULATE THE ARFF FILE FOR THE PREDICTION WITH WEKA GUI
con <- file(path_data_nfr_arff, "r", blocking = FALSE)
data_set<- readLines(con) # empty
close(con)

#(from now on) CODE UPDATED FOR MAJOR AT EMSE:
#preprocessing arff file for the prediction
data_set<- str_replace(data_set," string"," NUMERIC")
data_set<- str_replace(data_set," numeric"," NUMERIC")

data_set<- str_replace_all(data_set,"'","")
pos<- which(str_detect(data_set," NUMERIC"))
pos<- pos[length(pos)]
data_set[pos] <- str_replace(data_set[pos]," NUMERIC", " {yes,no}")
# we remove the wrong line in the file:
pos<- which(str_detect(data_set,"@data"))+1
data_set<- data_set[-pos]

data_set<- str_replace(data_set,"non_wontfix-issue([0-9])+.txt,","0,")
data_set<- str_replace(data_set,"non_([0-9])+,","")

data_set<- str_replace(data_set,"wontfix-issue([0-9])+.txt,","0,")
data_set<-     str_replace(data_set,"wontfix-issue([0-9])+.txt,","0,")
data_set<- str_replace(data_set,"non_([0-9])+,","")

write(data_set,path_data_nfr_arff)
#Next step is to experiment with ML using the WEKA GUI:
 # - %split 50% using  "data_set" - located in "path_data_nfr_arff" - this for J48, Naive Baye, SMO
 # - 10-fold validation  "data_set" - located in "path_data_nfr_arff" - this for J48, Naive Baye, SMO




