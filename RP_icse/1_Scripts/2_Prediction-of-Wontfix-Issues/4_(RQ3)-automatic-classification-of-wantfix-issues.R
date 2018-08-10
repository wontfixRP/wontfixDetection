library(stringr)
library("stringr")
# create a first textmatrix with some files
library(tm)
library(lsa)
library(stringr)
library(foreign)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_65.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(JGR)
library(rpart)
library(bigmemory)
library(farff)
library(foreign)
library(rio)
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


#path input
path_non_wontfix_issues_data<-"../Projects-list/projectsInfo-additional-wontfix-data-RQ3.csv"

path_wontfix_issues_data<-"../Projects-list/projectsInfo-output_with_issue_urls-full-data-LABELED_categories_merged.csv"

threshold_training_and_test_set<-50 #50%

non_wontfix_issues_data <- read.csv(path_non_wontfix_issues_data)
wontfix_issues_data <- read.csv(path_wontfix_issues_data)

data(stopwords_en)

path_weka<-"../WEKA-stable-3-8/weka/"

# paths output matrixs

path_training_data_nfr<- "../Training-set/training-matrix_data_nfr.csv"

path_test_data_nfr<- "../Test-set/test-matrix_data_nfr.csv"

path_training_data_nfr_arff<- "../Training-set/training-matrix_data_nfr.arff"

path_test_data_nfr_arff<- "../Test-set/test-matrix_data_nfr.arff"

path_all_results_prediction<- paste("../Test-set/all_results_prediction.csv", sep="")

#teporarily folder...

td<-"../temp_folder_baseline"


td_trainingSet<-paste(td,"training_set", sep="/")
td_testSet<-paste(td,"test_set", sep="/")

deleteFiles(td_trainingSet)
deleteFiles(td_testSet)

non_wontfix_issues_data$TitleIssue <- as.character(non_wontfix_issues_data$TitleIssue)
non_wontfix_issues_data$DescriptionIssue <- as.character(non_wontfix_issues_data$DescriptionIssue)
wontfix_issues_data$TitleIssue <- as.character(wontfix_issues_data$TitleIssue)
wontfix_issues_data$DescriptionIssue <- as.character(wontfix_issues_data$DescriptionIssue)

non_wontfix_issues_data$TitleIssue[1]
non_wontfix_issues_data$DescriptionIssue[1]
wontfix_issues_data$TitleIssue[1]
wontfix_issues_data$DescriptionIssue[1]



half_wontfix_issues <- round(length(wontfix_issues_data$TitleIssue) / 2)

half_non_wontfix_issues <- round(length(non_wontfix_issues_data$TitleIssue) / 2)

#TRAINING SET 
i<-1
for(i in 1: half_wontfix_issues)
{
  nameFile<-paste("wontfix-issue",i,".txt",sep="")  
  write( paste(wontfix_issues_data$TitleIssue[i],wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep=""), file=paste(td,"training_set",nameFile, sep="/") )
}
i<-1
for(i in 1: half_non_wontfix_issues )
{
  nameFile<-paste("non_wontfix-issue",i,".txt",sep="")  
  write( paste(non_wontfix_issues_data$TitleIssue[i],non_wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep=""), file=paste(td,"training_set",nameFile, sep="/") )
}

#TEST SET 
i<-1
for(i in (half_wontfix_issues+1) : length(wontfix_issues_data$TitleIssue))
{
  nameFile<-paste("wontfix-issue",i,".txt",sep="")  
  write( paste(wontfix_issues_data$TitleIssue[i],wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep=""), file=paste(td,"test_set",nameFile, sep="/") )
}
i<-1
for(i in (half_non_wontfix_issues+1) : length(non_wontfix_issues_data$TitleIssue) )
{
  nameFile<-paste("non_wontfix-issue",i,".txt",sep="")  
  write( paste(non_wontfix_issues_data$TitleIssue[i],non_wontfix_issues_data$DescriptionIssue[i]," HiHi ",sep=""), file=paste(td,"test_set",nameFile, sep="/") )
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
# we see a sub part of the matrix
matrix_term_by_doc[1:3,1:3]
#matrix_term_by_doc[3470:3473,1:10]
#row.names(matrix_term_by_doc)

positions_non_wontfix<- which(str_detect(colnames(matrix_term_by_doc),"non_wontfix-issue[0-9]+(.)txt"))
positions_wontfix<- which(!str_detect(colnames(matrix_term_by_doc),"non_wontfix-issue[0-9]+(.)txt"))

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),positions_non_wontfix]="no"

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),positions_wontfix]="yes"

matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]


positions_wontfix<- which(matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]=="yes")

positions_non_wontfix<- which(matrix_term_by_doc[length(rownames(matrix_term_by_doc)),]=="no")

threshold_wontfix <- round(length(positions_wontfix)*threshold_training_and_test_set/100)
threshold_non_wontfix <- round(length(positions_non_wontfix)*threshold_training_and_test_set/100)

train1_positions <- positions_wontfix[1:threshold_wontfix]
train2_positions <- positions_non_wontfix[1:threshold_non_wontfix]

test1_positions <- positions_wontfix[ (threshold_wontfix+1):length(positions_wontfix)]
test2_positions <- positions_non_wontfix[(threshold_non_wontfix+1):length(positions_non_wontfix)]

trainingSet<- matrix_term_by_doc[,c(train1_positions,train2_positions)]
testSet<- matrix_term_by_doc[,c(test1_positions,test2_positions)]


write.csv(t(trainingSet),path_training_data_nfr,quote=FALSE)
write.csv(t(testSet),path_test_data_nfr,quote=FALSE)

convert(path_training_data_nfr, path_training_data_nfr_arff)
convert(path_test_data_nfr, path_test_data_nfr_arff)


#WE MANIPULATE THE ARFF FILES FOR THE PREDICTION
con <- file(path_training_data_nfr_arff, "r", blocking = FALSE)
training_set<- readLines(con) # empty
close(con)
con <- file(path_test_data_nfr_arff, "r", blocking = FALSE)
test_set<- readLines(con) # empty
close(con)
#preprocessing arff file for the prediction
training_set<- str_replace(training_set," string"," NUMERIC")
test_set<- str_replace(test_set," string"," NUMERIC")
training_set<- str_replace_all(training_set,"'","")
test_set<- str_replace_all(test_set,"'","")
#we then change the type of the "nfr" attribute
#training_set[which(str_detect(training_set,"@attribute nfr NUMERIC"))] <- "@attribute nfr {Reliability,Efficiency,Functionality,Maintainability,Usability,Portability}"
#test_set[which(str_detect(test_set,"@attribute nfr NUMERIC"))] <- "@attribute nfr {Reliability,Efficiency,Functionality,Maintainability,Usability,Portability}"

pos<- which(str_detect(training_set," NUMERIC"))
pos<- pos[length(pos)]
training_set[pos] <- str_replace(training_set[pos]," NUMERIC", " {yes,no}")
pos<- which(str_detect(test_set," NUMERIC"))
pos<- pos[length(pos)]
test_set[pos] <- str_replace(test_set[pos]," NUMERIC"," {yes,no}")

# we remove the wrong line in the file:
pos<- which(str_detect(training_set,"@data"))+1
training_set<- training_set[-pos]
pos<- which(str_detect(test_set,"@data"))+1
test_set<- test_set[-pos]


training_set<- str_replace(training_set,"non_wontfix-issue([0-9])+.txt,","0,")
test_set<-     str_replace(test_set,    "non_wontfix-issue([0-9])+.txt,","0,")
test_set<- str_replace(test_set,"non_([0-9])+,","")

training_set<- str_replace(training_set,"wontfix-issue([0-9])+.txt,","0,")
test_set<-     str_replace(training_set,"wontfix-issue([0-9])+.txt,","0,")
training_set<- str_replace(test_set,"non_([0-9])+,","")
#finally we remove the useless variable V1
#training_set<- training_set[-2]
#test_set<- test_set[-2]

#test_set<-str_replace(test_set,",0$",",?")

#in the text set we replace the row related to "nfr" with the symbol "?"

write(training_set,path_training_data_nfr_arff)
#write.arff(t(matrix_nfr_TEXT22),path_test_data_nfr_arff)
write(test_set,path_test_data_nfr_arff)


# @attribute nfr {Reliability,Efficiency,Functionality,Maintainability,Usability,Portability}
# COMMAND TO MAKE PREDICT (WITHING PROJECT)
#  java -classpath $CLASSPATH:weka.jar weka.classifiers.bayes.NaiveBayes -t diabetes.arff 
#  java -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -t diabetes.arff 
# COMMAND Convert CSV to ARFF files
# java  -classpath $CLASSPATH:weka.jar weka.core.converters.CSVLoader filename.csv > filename.arff
# (EXAMPLE 0): java  -classpath $CLASSPATH:weka.jar weka.core.converters.CSVLoader training-matrix_data_nfr.csv > training-matrix_data_csv-to-nfr.arff
#COMMAND TO BUILD A MODEL:
#java  -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -C 0.25 -M 2 -t diabetes.arff -d j48-diabetes.model
# (EXAMPLE 1): java  -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -C 0.25 -M 2 -t training-matrix_data_nfr.arff -d j48-trained.model
#COMMAND TO USE the BUILT MODEL FOR THE PREDICITON:
#java -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -l j48-diabetes.model -T diabetes-to-predict.arff
# (EXAMPLE 2): java -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -p 9 -l j48-trained.model -T matrix_data_nfr-test.arff
# (EXAMPLE 3): java -classpath $CLASSPATH:weka.jar weka.classifiers.trees.J48 -p 9 -l j48-trained.model -T test-matrix_data_nfr.arff

# we set the ML model to use for the classification
#J48
# ML_model_experimented<-"J48"
# ML_model_in_weka <- "weka.classifiers.trees.J48"
# name__of_the_saved_ML_model_in_weka <- "j48-trained.model"
#weka_command <- " -C 0.25 -M 2 -t "

#Logistic-Regression
# ML_model_experimented<-"Logistic-Regression"
# ML_model_in_weka <- "weka.classifiers.functions.Logistic"
# name__of_the_saved_ML_model_in_weka <- "Logistic-Regression-trained.model"
# weka_command <- " -t "

#SMO
ML_model_experimented<-"SMO"
ML_model_in_weka <- "weka.classifiers.functions.SMO"
name__of_the_saved_ML_model_in_weka <- "SMO-trained.model"
weka_command <- " -t "

#Naive Bayes
#  ML_model_experimented<-"NaiveBayes"
#  ML_model_in_weka <- "weka.classifiers.bayes.NaiveBayes"
#  name__of_the_saved_ML_model_in_weka <- "NaiveBayes-trained.model"
#  weka_command <- " -t "

#java -classpath $CLASSPATH:weka.jar weka.classifiers.functions.Logistic -h
command<-paste("cd ",path_weka," && java  -classpath $CLASSPATH:weka.jar ",ML_model_in_weka, weka_command,path_training_data_nfr_arff,"-d ",name__of_the_saved_ML_model_in_weka)
#command<-paste("cd ",path_weka," && ./run.sh")
outcome_execution<-as.character(system(command,intern = TRUE))

command<-paste("cd ",path_weka," && java -classpath $CLASSPATH:weka.jar ",ML_model_in_weka, " -p 9 -l ",name__of_the_saved_ML_model_in_weka, " -T ",path_test_data_nfr_arff)
#command<-paste("cd ",path_weka," && ./run.sh")
results_prediction<-as.character(system(command,intern = TRUE))

# we add the value of precision, recall and F-measure:...
TP <- results_prediction[which(str_detect(results_prediction,"([0-9]+:yes( )+[0-9]+:yes)"))]
FP <- results_prediction[which(str_detect(results_prediction,"([0-9]+:yes( )+[0-9]+:no)"))]
FN <- results_prediction[which(str_detect(results_prediction,"([0-9]+:no( )+[0-9]+:yes)"))]
TN <- results_prediction[which(str_detect(results_prediction,"([0-9]+:no( )+[0-9]+:no)"))]

precision <- length(TP) / (length(TP)+ length(FP))
recall <- length(TP) / (length(TP)+ length(FN))
Fscore <- 2 * length(TP) / ( 2 * length(TP) + length(FP)+ length(FN))
Fmeasure <- 2 * precision * recall / (precision + recall)
Accuracy <- (length(TP) + length(TN) ) / ( length(TP) + length(TN)  + length(FP)+ length(FN))

pos<-length(results_prediction)+1
results_prediction[pos]<- paste("-----------------------------------")

# we add the value of precision, recall and F-measure:...
TP_2 <- results_prediction[which(str_detect(results_prediction,"([0-9]+:no( )+[0-9]+:no)"))]
FP_2 <- results_prediction[which(str_detect(results_prediction,"([0-9]+:no( )+[0-9]+:yes)"))]
FN_2 <- results_prediction[which(str_detect(results_prediction,"([0-9]+:yes( )+[0-9]+:no)"))]
TN_2 <- results_prediction[which(str_detect(results_prediction,"([0-9]+:yes( )+[0-9]+:yes)"))]


precision_2 <- length(TP_2) / (length(TP_2)+ length(FP_2))
recall_2 <- length(TP_2) / (length(TP_2)+ length(FN_2))
Fscore_2 <- 2 * length(TP_2) / ( 2 * length(TP_2) + length(FP_2)+ length(FN_2))
Fmeasure_2 <- 2 * precision_2 * recall_2 / (precision_2 + recall_2)
Accuracy_2 <- (length(TP_2) + length(TN_2) ) / ( length(TP_2) + length(TN_2)  + length(FP_2)+ length(FN_2))


pos<-length(results_prediction)+1
results_prediction[pos]<- paste("RESULTS (Wontfix issue):")

pos<-length(results_prediction)+1
results_prediction[pos]<- paste("TP =",length(TP))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("FP =",length(FP))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("TN =",length(FN))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("TN =",length(TN))

pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Precision =",precision)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Recall =",recall)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Fscore =",Fscore)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Fmeasure =",Fmeasure)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Accuracy =",Accuracy)


pos<-length(results_prediction)+1
results_prediction[pos]<- paste("-----------------------------------")

pos<-length(results_prediction)+1
results_prediction[pos]<- paste("RESULTS (Non-wontfix issue):")


pos<-length(results_prediction)+1
results_prediction[pos]<- paste("TP =",length(TP_2))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("FP =",length(FP_2))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("FN =",length(FN_2))
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("TN =",length(TN_2))

pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Precision =",precision_2)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Recall =",recall_2)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Fscore =",Fscore_2)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Fmeasure =",Fmeasure_2)
pos<-length(results_prediction)+1
results_prediction[pos]<- paste("Accuracy =",Accuracy_2)

results_prediction

#we extract the information about the predicted classification
#results_prediction <- as.character(na.omit(str_extract(results_prediction," ([0-9])+:([A-z])+")))
#results_prediction <- str_replace(results_prediction," ([0-9])+:","")

path_to_save_results <- str_replace(path_all_results_prediction, "_prediction", paste("_prediction-",ML_model_experimented,sep=""))

write(results_prediction,path_to_save_results)









