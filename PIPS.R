## Author: Nathan Wakefield Based on the work of Molly Creagar in the factorAnalysisSPIPS.R file
## Written for SEMINAL Project research
## code to accompany cleaningR.R

should.remove2 <- function(row) {
  
  #checking for if students answered all questions
  if(sum(is.na(row)) >0)
  {return(TRUE)}
  else
  {return(FALSE)}
  # FALSE
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

library("lavaan")
## do a confirmatory factor analysis with other data sets - Fall 2019, Spring 2020
## see if model holds with these years after cleaning
## testing if model holds with another site
## x16 maybe in 2, x20 maybe in 2.. best fit is when x16, x20 included in both places

cfa.model2 <-' engagement  =~ x1 + x2 + x5 + x11 + x12 + x17 + x18 + x19 + x22
collaboration =~ x6 + x7 + x8 + x10 + x15 + x16 + x20
participation   =~ x13 + x16 + x21
thinking =~ x3 + x4 + x20'

## getting data ready

DataCSV <- #removed for blinding the project
DataCSV <- as.data.frame(sapply(DataCSV,gsub,pattern='\x89۪' ,replacement="'"))
DFData <- DataCSV[,84:105] #questions are in same order
DFData <- DFData[,-9]
DFData <- DFData[,-13]
t.f.remove <- apply(DFData, 1, should.remove2) # should row be removed
DFData <- as.data.frame(DFData[!t.f.remove, ])
DFDataM<-as.matrix(DFData)
DFDataM<-apply(DFDataM,1,as.numeric)
DFDataM<-t(DFDataM)
row.names(DFDataM) <- NULL
colnames(DFDataM) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


## testing data w cfa
fit <- cfa(cfa.model2, data=DFDataM, estimator="MLM")
summary(fit, fit.measures=TRUE)


#checking discriminant validity
library("semTools")
discriminantValidity(fit, cutoff = 0.85)

