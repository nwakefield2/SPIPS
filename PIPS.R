## Author: Nathan Wakefield Based on the work of Molly Creagar in the factorAnalysisSPIPS.R file
## Written for SEMINAL Project research
## code to accompany cleaningR.R


##This defines a function whch can remove entries with missing data.
should.remove2 <- function(row) {
  
  #checking for if students answered all questions
  if(sum(is.na(row)) >0)
  {return(TRUE)}
  else
  {return(FALSE)}
  # FALSE
}

##This function will remove a row made entirely of NA entries.
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

## github code for this from 
## https://gist.github.com/tonosan/cb7581f3459ae7c4217a @tonosan
##This function computes the comparative fit index (CFI) for an output of factorial analysis with fa() function
fa.CFI<-function(x){
  nombre<-paste(x,"CFI",sep = ".")
  nombre<-
    ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  return(nombre)
}
library("readxl")
library("lavaan")

##Originally we developed a model using four factors hence this script is aimed at testing
## doing a confirmatory factor analysis with other data sets - Fall 2019, Spring 2020
## see if model holds with these years after cleaning
## testing if model holds with another site
## x16 maybe in 2, x20 maybe in 2.. best fit is when x16, x20 included in both places

cfa.model2 <-' engagement  =~ x1 + x2 + x5 + x11 + x12 + x17 + x18 + x19 + x22
collaboration =~ x6 + x7 + x8 + x10 + x15 + x16 + x20
participation   =~ x13 + x13.1 + x16 + x21
thinking =~ x3 + x4 + x20'

## What follows is a set of scripts aimed at getting the data ready

DataCSV <- read_excel(file.choose(), sheet = 1)
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='\x89۪' ,replacement="'"))
#Convert String to Numeric
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='Very descriptive' ,replacement="5"))
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='Mostly descriptive' ,replacement="4"))
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='Somewhat descriptive' ,replacement="3"))
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='Minimally descriptive' ,replacement="2"))
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='Not at all descriptive' ,replacement="1"))
##This is completely generic up to this point regardless of the file or which columns represent which items.

##colnames<-gsub(".*\\.\\.\\.","",names(DataCSV)[c(grep("^PIPS", names(DataCSV)))])
colnames<-gsub(".*\\-","",names(DataCSV)[c(grep("^PIPS", names(DataCSV)))])
##Collect all the PIPS data together with first and last name.
PIPSData<-cbind(DataCSV[,c("Recipient Last Name","Recipient First Name")],DataCSV[c(grep("^PIPS", names(DataCSV)))])

##Count the number of PIPS Questions
numberPips<-length(unique(colnames))
firstoccurance<-3

##The PIPS allowed for one instructor to fill out the form for multiple courses.
##The result was a data file in which each instructor had filled out the PIPS questions multiple times
##We chose to keep all of this data as unque entries.
##Hence we aim to build a list of data sets so that each iteration of the pips becomes a new item in the list
##The result will then be moved into a single dataframe called master data frame.
my_data_list<-list()
colnamesforloop<-c("Last Name","First Name",colnames[1:numberPips])
for (i in c(1:(length(colnames)/numberPips))){
  my_data_list[[i]]<-DataCSV[,c("Recipient Last Name","Recipient First Name")]
  my_data_list[[i]]<-cbind(my_data_list[[i]],PIPSData[,(firstoccurance+numberPips*(i-1)):(firstoccurance+numberPips*(i)-1)])
  names(my_data_list[[i]])<-colnamesforloop
  }
masterDataframe<-my_data_list[[1]]
for (i in c(2:(length(colnames)/numberPips))){
  masterDataframe<-rbind(masterDataframe, my_data_list[[i]])
}
masterDataframe<-delete.na(masterDataframe,numberPips-1)



##We now work toward conducting the CFI.

#The following is commented out because we used it for numerical results. We need a more generic name based version.
##DFData <- DataCSV[,c(82,83,85,88,90,93,94,95,96,97,98,99,107,108,109,110,111,117,118,119,120,121,122)] #questions are in same order
##DFData <- DFData[,-9]
##DFData <- DFData[,-13]

PIPSCol=c(" I guide students through major topics as they listen",
          " I provide activities that connect course content to my students' lives and future work",
          " I provide students with immediate feedback on their work during class (e.g., student response systems; short quizzes)",
          " I ask students to respond to questions during class time",
          " In my class a variety of means (models, drawings, graphs, symbols, simulations, tables, etc.) are used to represent course topics and/or solve problems",
          " I structure class so that students talk with one another about course topics",
          " I structure class so that students constructively criticize one another's ideas",
          " I structure class so that students discuss their mathematical difficulties with other students",
          " I structure class so that students work on problems individually during class",
          " I structure class so that students work together in pairs or small groups",
          " I structure class so that more than one approach to solving a problem is discussed",
          " I provide time for students to reflect about the processes they use to solve problems",
          " A wide range of students respond to my questions in class",
          " I know most of my students by name",
          " When calling on students in class, I use randomized response strategies (e.g., picking names from a hat)",
          "peer support among students (e.g., ask peer before you ask me, having group roles, developing a group solution to share)",
          " There is a sense of community among the students in my class",
          " I explain concepts in this class in a variety of ways",
          " I adjust my teaching based upon what students currently do or do not understand",
          " I give feedback on homework, exams, quizzes, etc.",
          " I structure class so that students share their ideas (or their group's ideas) during whole class discussions",
          " A wide range of students participate in class",
          " I use strategies to encourage participation from a wide range of students")

DFData <-masterDataframe[,PIPSCol]


DFData<-apply(DFData,1,as.numeric)
DFData<-t(DFData)
t.f.remove <- apply(DFData, 1, should.remove2) # should row be removed
DFData <- as.data.frame(DFData[!t.f.remove, ])
DFDataM<-as.matrix(DFData)
DFDataM<-apply(DFDataM,1,as.numeric)
DFDataM<-t(DFDataM)
row.names(DFDataM) <- NULL
colnames(DFDataM) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8","x9", "x10", "x11", "x12", "x13","x14","x13.1", "x15", "x16", "x18", "x17", "x19", "x20", "x21", "x22")


## testing data w cfa
fit <- cfa(cfa.model2, data=DFDataM, estimator="MLM")
summary(fit, fit.measures=TRUE)


#checking discriminant validity
library("semTools")
discriminantValidity(fit, cutoff = 0.85)

