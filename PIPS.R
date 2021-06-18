## Author: Nathan Wakefield
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

numCleanF20 <- clean(numericDF)

pathNumericS20 <- #removed for blinding the project
numericDF<-read.csv2(pathNumericS20,encoding="UTF-8",check.names = F,sep=",",stringsAsFactors = FALSE)
numericDFs20 <- as.data.frame(sapply(numericDF,gsub,pattern='\x89۪' ,replacement="'"))
names(numericDFs20) <-gsub('\x89۪ ', "'",names(numericDFs20))
numerS20 <- clean(numericDFs20)


questions <- colnames(numCleanF20)

## the PIPS questions are in these cols:
ques <- c(questions[70:91])


DFa <- numerF20[,70:91]
t.f.remove <- apply(DFa, 1, should.remove2) # should row be removed
DFa <- as.data.frame(DFa[!t.f.remove, ])
DFab<-as.matrix(DFa)
DFab<-apply(DFab,1,as.numeric)
DFab<-t(DFab)

## make correlation 'heat map'
library("corrplot")
corrplot(cor(DFab), order = "hclust", tl.col='black', tl.cex=.75, method = 'square')
fit <- princomp(DFab, cor=TRUE, na.action=na.omit)
plot(fit, yaxp=c(0,8,8), main="Scree Plot, Fall 2020")


## naming columns
row.names(DFab) <- NULL
colnames(DFab) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


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

alls19 <- #removed for blinding the project
alls19 <- as.data.frame(sapply(alls19,gsub,pattern='\x89۪' ,replacement="'"))
DFs19all <- alls19[,84:105] #questions are in same order
DFs19all <- DFs19all[,-9]
DFs19all <- DFs19all[,-13]
t.f.remove <- apply(DFs19all, 1, should.remove2) # should row be removed
DFs19all <- as.data.frame(DFs19all[!t.f.remove, ])
s19allm<-as.matrix(DFs19all)
s19allm<-apply(s19allm,1,as.numeric)
s19allm<-t(s19allm)
row.names(s19allm) <- NULL
colnames(s19allm) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


## testing data w cfa
fitS19 <- cfa(cfa.model2, data=s19allm, estimator="MLM")
summary(fitS19, fit.measures=TRUE)


#checking discriminant validity
library("semTools")
discriminantValidity(fitF19, cutoff = 0.85)

