## Author: Molly Creagar
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


pathNumericF19 <- #removed for blinding the project
numericDF<-read.csv2(pathNumericF19,encoding="UTF-8",check.names = F,sep=",",stringsAsFactors = FALSE)
numericDFf19 <- as.data.frame(sapply(numericDF,gsub,pattern='\x89۪' ,replacement="'"))
names(numericDFf19) <-gsub('\x89۪ ', "'",names(numericDFf19))
numerF19 <- numericDFf19
numCleanF19 <- clean(numerF19)


questions <- colnames(numCleanF20)

## the SPIPS questions are in these cols:
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

## let's look at the factor sets for 2-5 factors
library("psych")
twofactor <- fa(DFab,nfactors=2,rotate="promax",fm="minres", alpha = 0.05)
threefactor <- fa(DFab,nfactors=3,rotate="promax",fm="minres", alpha = 0.05)
fourfactor <- fa(DFab,nfactors=4,rotate="promax",fm="minres", alpha = 0.05)
fivefactor <- fa(DFab,nfactors=5,rotate="promax",fm="minres", alpha = 0.05)

## then find the questions that go with a factor. Ideally 5 or 6 there are
## cross-loading questions with each of 3, 4, 5 factors. should either add more
## questions to start with or drop the cross-loading questions however, using 4
## factors with 0.32 cutoff seems pretty good...

print(fourfactor$loadings, cutoff = 0.32, digits = 6)

fa.parallel(DFab,fm="minres", fa="fa")
KMO(fourfactor$r)
cortest.bartlett(fourfactor$r, n=nrow(DFab))

## can make histograms for each item
## used to verify/check normality
for(i in 1:ncol(DFab))
{ hist(DFab[,i], main="Histogram", xlab = paste(i))
}

factanal(DFab, 4)
print(fourfactor$loadings,cutoff = 0.5)

fa.diagram(fourfactor)

## Reliability calculation as determined by Cronbach's alpha
DFabF1 <- DFab[,c(1,2,5,11,12,17,18,19,22)]
DFabF2 <- DFab[,c(6,7,8,10,15,16,20)]
DFabF3 <- DFab[,c(13, 16, 21)]
DFabF4 <- DFab[,c(3, 4, 20)]
print(psych::alpha(DFabF1), digits=5)
print(psych::alpha(DFabF2), digits=5)
print(psych::alpha(DFabF3), digits=5)
print(psych::alpha(DFabF4), digits=5)

## dropping items that do not load onto any factor
DFab <- DFab[,-14]
DFab <- DFab[,-9]

## naming columns
row.names(DFab) <- NULL
colnames(DFab) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


## factor grouping of questions here
ques1 <- c(ques[1:2], ques[5], ques[11:12], ques[17:19], ques[22])
ques2 <- c(ques[6:8], ques[10], ques[15:16], ques[20])
ques3 <- c(ques[13], ques[16], ques[21])
ques4 <- c(ques[3:4], ques[20])

## cross-loading questions are-- 
## Q16: There is a sense of community among the students in my class [crL onto (2) (3)]
## Q20: I share my ideas (or my groups's ideas) during whole class discussions [crL onto (2) (4)]


library("lavaan")
## do a confirmatory factor analysis with other data sets - Fall 2019, Spring 2020
## see if model holds with these years after cleaning
## testing if model holds with another site
## x16 maybe in 2, x20 maybe in 2.. best fit is when x16, x20 included in both places

cfa.model2 <-' engagement  =~ x1 + x2 + x5 + x11 + x12 + x17 + x18 + x19 + x22
collaboration =~ x6 + x7 + x8 + x10 + x15 + x16 + x20
participation   =~ x13 + x16 + x21
thinking =~ x3 + x4 + x20'


fitF20 <- cfa(cfa.model2, data=DFab, estimator = "MLM")

# display summary output
summary(fitF20, fit.measures=TRUE, standardize = TRUE, rsquare = TRUE)

res1 <- residuals(fitF20, type = "cor")$cov
res1[upper.tri(res1,diag=T)] <- NA
v1 <- as.vector(res1)
v2  <- v1[!is.na(v1)]
qqnorm(v2,id=F)
library("semPlot")
semPaths(fitF20, residuals=F,sizeMan=5,
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="spring")


## getting Fall 19 data ready
DF19 <- numCleanF19[,57:78] #questions are in same order
DF19 <-DF19[,-14]
DF19 <- DF19[,-9]
t.f.remove <- apply(DF19, 1, should.remove2) # should row be removed
DF19 <- as.data.frame(DF19[!t.f.remove, ])
DFm19<-as.matrix(DF19)
DFm19<-apply(DFm19,1,as.numeric)
DFm19<-t(DFm19)
#DFm19 <-DFm19[,-14]
#DFm19 <- DFm19[,-9]
row.names(DFm19) <- NULL
colnames(DFm19) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


## testing fall 19 data w cfa
fitF19 <- cfa(cfa.model2, data=DFm19, estimator="MLM")
summary(fitF19, fit.measures=TRUE)
## model is debateably not great. but, let's see what happens with spring 2020 data...

## getting spring 2020 data ready
DFs20 <- numerS20[,84:105]
DFs20 <- DFs20
DFs20<- DFs20[,-9]
DFs20 <-DFs20[,-13]
t.f.remove <- apply(DFs20, 1, should.remove2) # should row be removed
DFs20 <- as.data.frame(DFs20[!t.f.remove, ])
DFms20<-as.matrix(DFs20)
DFms20<-apply(DFms20,1,as.numeric)
DFms20<-t(DFms20)
row.names(DFms20) <- NULL
colnames(DFms20) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")
colnames(DFs20) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21")


## testing spring 2020 data w cfa
fitS20 <- cfa(cfa.model2, data=DFms20, estimator="MLM")
summary(fitS20, fit.measures=TRUE)

## spring seems to fit a little better than fall 2019. Which should be expected..
## due to COVID presence

## after testing informally without cross-loading, we see that the fit is better
## when we use cross-loading, which matches the intuition from fall 2020 EFA

## for RMSEA between 0.05 and 0.08, we say it is mediocre to good. fails good fit, but also fails poor fit

## Spring 2019 and Fall 2018 are from all SEMINAL institutions
## data was already cleaned when obtained

## getting S19 data ready

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


## testing spring 2019 data w cfa
fitS19 <- cfa(cfa.model2, data=s19allm, estimator="MLM")
summary(fitS19, fit.measures=TRUE)

## getting all F18 data ready
allf18 <- #removed for blinding the project
allf18 <- as.data.frame(sapply(allf18,gsub,pattern='\x89۪' ,replacement="'"))
DFf18all <- allf18[,75:96] #questions are in same order
DFf18all <- DFf18all[,-9]
DFf18all <- DFf18all[,-13]
t.f.remove <- apply(DFf18all, 1, should.remove2) # should row be removed based on answering all same number
DFf18all <- as.data.frame(DFf18all[!t.f.remove, ])
f18allm<-as.matrix(DFf18all)
f18allm<-apply(f18allm,1,as.numeric)
f18allm<-t(f18allm)
row.names(f18allm) <- NULL
colnames(f18allm) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x10", "x11", "x12", "x13", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22")


## testing fall 2018 data w cfa
fitF18 <- cfa(cfa.model2, data=f18allm, estimator ="MLM")
summary(fitF18, fit.measures=TRUE)

#checking discriminant validity
library("semTools")
discriminantValidity(fitF18, cutoff = 0.85)
discriminantValidity(fitS19, cutoff = 0.85)
discriminantValidity(fitF19, cutoff = 0.85)
discriminantValidity(fitS20, cutoff = 0.85)
discriminantValidity(fitF20, cutoff = 0.85)