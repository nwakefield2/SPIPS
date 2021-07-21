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

cfa.model.A <-' student centered  =~ x2 + x3 + + y25 + x4 + y26 + x5 + y27 + x6 + x7 + x8 + x10 + x11 + y29 + y30 + y31
instructor centered =~ x1 + y23 + y24 + y28 + x12 + y32 + y33 + y34 + y35'
cfa.model.B <-' student student interactions  =~ y27 + x6 + x7 + x8 +x10 +y30
content delivery practices =~ x1 + y23 + y24 + y28
formative assessment   =~ x3 + y25 + y26 + y29 + y31
student content engagement =~ x2 + x4 + x5 + x11 + x12
summative assessment =~ y32 + y33 + y34 + y35'

## What follows is a set of scripts aimed at getting the data ready This function reads in a dataset and prepares it for release.
ReadAndPrep.Data<-function(XLSXIn){
#DataCSV <- read_excel(file.choose(), sheet = 1)
DataCSV<-XLSXIn
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
DecisionsContentcolnames<-gsub("Decisions_Content(.*)","Decisions_Content",names(DataCSV)[c(grep("Decisions_Content \\- Selected Choice", names(DataCSV)))])
##Collect all the PIPS data together with first and last name.
PIPSData<-cbind(DataCSV[,c("Recipient Last Name","Recipient First Name")],DataCSV[c(grep("^PIPS", names(DataCSV)))])
DecisionsContentData<-cbind(DataCSV[,c("Recipient Last Name","Recipient First Name")],DataCSV[c(grep("Decisions_Content \\- Selected Choice", names(DataCSV)))])
DecisionsApproachData<-cbind(DataCSV[,c("Recipient Last Name","Recipient First Name")],DataCSV[c(grep("Decisions_Approach \\- Selected Choice", names(DataCSV)))])

##Count the number of PIPS Questions
numberPips<-length(unique(colnames))
firstoccurance<-3

##The PIPS allowed for one instructor to fill out the form for multiple courses.
##The result was a data file in which each instructor had filled out the PIPS questions multiple times
##We chose to keep all of this data as unque entries.
##Hence we aim to build a list of data sets so that each iteration of the pips becomes a new item in the list
##The result will then be moved into a single dataframe called master data frame.
my_data_list<-list()
colnamesforloop<-c("Last Name","First Name","DecisionsContent","DecisionsApproach",colnames[1:numberPips])
for (i in c(1:(length(colnames)/numberPips))){
  my_data_list[[i]]<-DataCSV[,c("Recipient Last Name","Recipient First Name")]
  my_data_list[[i]]<-cbind(my_data_list[[i]],DecisionsContentData[,firstoccurance+i-1])
  my_data_list[[i]]<-cbind(my_data_list[[i]],DecisionsApproachData[,firstoccurance+i-1])
  my_data_list[[i]]<-cbind(my_data_list[[i]],PIPSData[,(firstoccurance+numberPips*(i-1)):(firstoccurance+numberPips*(i)-1)])
  names(my_data_list[[i]])<-colnamesforloop
  }
masterDataframe<-my_data_list[[1]]
for (i in c(2:(length(colnames)/numberPips))){
  masterDataframe<-rbind(masterDataframe, my_data_list[[i]])
}
masterDataframe<-delete.na(masterDataframe,numberPips-1)
return(masterDataframe)
}

library(easycsv)
folderPath<-choose_dir()
file.list <- list.files(folderPath,pattern='*.xlsx')

mergeddata<-ReadAndPrep.Data(read_excel(paste(folderPath,file.list[1],sep = ""), sheet = 1))
for (i in c(2:(length(file.list)))){
  print(i)
  tempmergeddata<-ReadAndPrep.Data(read_excel(paste(folderPath,file.list[i],sep = ""), sheet = 1))
  mergeddata<-rbind(mergeddata,tempmergeddata)
}


##We now work toward conducting the CFI.
##The first thing we do is try to follow the work in the SPIPS factor analysis. To this end we choose out the columns that match that work.

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

#All this code aims to prepare the data for factor analysis.
DFData <-mergeddata[,PIPSCol]
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

##CFA failed to have a good CFI so we moved to an EFA

## make correlation 'heat map'
library("corrplot")
corrplot(cor(DFDataM), order = "hclust", tl.col='black', tl.cex=.75, method = 'square')
fit <- princomp(DFDataM, cor=TRUE)
plot(fit, yaxp=c(0,8,8), main="Scree Plot, PIPS")

## let's look at the factor sets for 2-5 factors
library("psych")
library("GPArotation")
twofactor <- fa(DFDataM,nfactors=2,rotate="promax",fm="minres", alpha = 0.05)
print(threefactor)
print(twofactor$loadings, cutoff = 0.32, digits = 6)

#A two factor model does not seem to work well so we will move to the whole data set.
#All this code aims to prepare the data for factor analysis.
DFData2 <-mergeddata[,c(5:45)]
DFData2<-apply(DFData2,1,as.numeric)
DFData2<-t(DFData2)
t.f.remove <- apply(DFData2, 1, should.remove2) # should row be removed
DFData2 <- as.data.frame(DFData2[!t.f.remove, ])
DFDataM2<-as.matrix(DFData2)
DFDataM2<-apply(DFDataM2,1,as.numeric)
DFDataM2<-t(DFDataM2)
row.names(DFDataM2) <- NULL
colnames(DFDataM2)<-c(1:41)

## make correlation 'heat map'
library("corrplot")
corrplot(cor(DFDataM2), order = "hclust", tl.col='black', tl.cex=.75, method = 'square')
fit <- princomp(DFDataM2, cor=TRUE)
plot(fit, yaxp=c(0,8,8), main="Scree Plot, PIPS")

## let's look at the factor sets for 2-5 factors
library("psych")
library("GPArotation")
twofactor <- fa(DFDataM,nfactors=2,rotate="promax",fm="minres", alpha = 0.05)
print(twofactor)
print(twofactor$loadings, cutoff = 0.32, digits = 6)



#We now explore using control for various levels of decision making.
controlledmergeddata<-subset(mergeddata,DecisionsContent=="Someone else makes most decisions.")
DFData3 <-controlledmergeddata[,PIPSCol]
DFData3<-apply(DFData3,1,as.numeric)
DFData3<-t(DFData3)
t.f.remove <- apply(DFData3, 1, should.remove2) # should row be removed
DFData3 <- as.data.frame(DFData3[!t.f.remove, ])
DFDataM3<-as.matrix(DFData3)
DFDataM3<-apply(DFDataM3,1,as.numeric)
DFDataM3<-t(DFDataM3)
row.names(DFDataM3) <- NULL
colnames(DFDataM3) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8","x9", "x10", "x11", "x12", "x13","x14","x13.1", "x15", "x16", "x18", "x17", "x19", "x20", "x21", "x22")


## make correlation 'heat map'
library("corrplot")
corrplot(cor(DFDataM3), order = "hclust", tl.col='black', tl.cex=.75, method = 'square')
fit <- princomp(DFDataM3, cor=TRUE)
plot(fit, yaxp=c(0,8,8), main="Scree Plot, PIPS")

## testing data w cfa
fit <- cfa(cfa.model2, data=DFDataM3, estimator="MLM")
summary(fit, fit.measures=TRUE)


DecisionContentChoices<-unique(mergeddata[,"DecisionsContent"])
for (i in DecisionContentChoices){
  controlledmergeddata<-subset(mergeddata,DecisionsContent==i)
  print(i)
  DFData3 <-controlledmergeddata[,c(5:45)]
  DFData3<-apply(DFData3,1,as.numeric)
  DFData3<-t(DFData3)
  t.f.remove <- apply(DFData3, 1, should.remove2) # should row be removed
  DFData3 <- as.data.frame(DFData3[!t.f.remove, ])
  DFDataM3<-as.matrix(DFData3)
  DFDataM3<-apply(DFDataM3,1,as.numeric)
  DFDataM3<-t(DFDataM3)
  row.names(DFDataM3) <- NULL
  colnames(DFDataM3) <- c(1:41)
  ## make correlation 'heat map'
  library("corrplot")
  corrplot(cor(DFDataM3), order = "hclust", tl.col='black', tl.cex=.75, method = 'square') 
}


twofactor <- fa(DFDataM3,nfactors=2,rotate="promax",fm="minres", alpha = 0.05)
print(twofactor)
print(twofactor$loadings, cutoff = 0.32, digits = 6)


DecisionApproachChoices<-na.omit(unique(mergeddata[,"DecisionsApproach"]))
for (i in DecisionApproachChoices){
  print(i)
  controlledmergeddata<-subset(mergeddata,DecisionsApproach==i)
  DFData3 <-controlledmergeddata[,c(5:45)]
  DFData3<-apply(DFData3,1,as.numeric)
  DFData3<-t(DFData3)
  t.f.remove <- apply(DFData3, 1, should.remove2) # should row be removed
  DFData3 <- as.data.frame(DFData3[!t.f.remove, ])
  DFDataM3<-as.matrix(DFData3)
  DFDataM3<-apply(DFDataM3,1,as.numeric)
  DFDataM3<-t(DFDataM3)
  row.names(DFDataM3) <- NULL
  #colnames(DFDataM3) <-c(1:41)
  colnames(DFDataM3) <- c("x1","x2","y23","x3","y24","y25","x4","y26","x5","y27","y28","x6","x7","x8","x9","x10","x11","x12","y29","y30","y31","y32","y33","y34","y35","x13","x14","x13.1","x15","x16","z36","z37","z38","z39","z40","x18","x17","x19","x20","x21","x22")
  print(dim(DFDataM3))
  ## make correlation 'heat map'
  library("corrplot")
  corrplot(cor(DFDataM3), order = "hclust", tl.col='black', tl.cex=.75, method = 'square') 
  fit <- cfa(cfa.model.A, data=DFDataM3, estimator="MLM")
  model_performance(fit,metrics="TLI",verbose=TRUE)
  #summary(fit, fit.measures=TRUE)
  }






twofactor <- fa(DFDataM3,nfactors=2,rotate="promax",fm="minres", alpha = 0.05)
print(twofactor)
print(twofactor$loadings, cutoff = 0.32, digits = 6)


