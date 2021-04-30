##Author: Molly Creagar
##Written for SEMINAL Project Data

## EXAMPLE USAGE: replace pathText and pathNumeric with the appropriate path to the raw data files
## Run this file. Then, run the snippet: numericCleaned<-clean(numericDF)


pathText<-#removed for blinding the project
pathNumeric<-#removed for blinding the project

textDF<-read.csv2(pathText,encoding="UTF-8",check.names = F,sep=",",stringsAsFactors = FALSE)
numericDF<-read.csv2(pathNumeric,encoding="UTF-8",check.names = F,sep=",",stringsAsFactors = FALSE)
cols<-gsub('\x89۪ ', "'",names(textDF))
cols<-gsub('_\xd1\xe9', "'", cols)
textDF <- as.data.frame(sapply(textDF,gsub,pattern='\x89۪' ,replacement="'"))
numericDF <- as.data.frame(sapply(numericDF,gsub,pattern='\x89۪' ,replacement="'"))
names(numericDF) <- gsub('_\xd1_ -', ",", names(numericDF))
names(numericDF) <-gsub('\x89۪ ', "'",names(numericDF))
names(numericDF)<-gsub('_\xd1\xe9', "'", names(numericDF))

## remove essentially blank rows (those for which everything after the consent is blank)
## if you have other criteria, change the should.remove function
should.remove <- function(row) {
  
  #threshhold for how blank is "essentially blank"
  if (sum(is.na(row)) > 198) {
    return(TRUE)
  }
  
  return(FALSE)
}



###############################################################################

### Function to check if all entries of a vector are the same by comparing each
### entry to the first entry
all.same <- function (x) {
  return(all(x[1]==x))
}

###############################################################################

clean<-function(DF){
  
  
  ## get rid of the whole row for preview
  if('Distribution Channel' %in% names(DF)){
    DF<-subset(DF, 'Distribution Channel'!="preview")
  }
  
  ## The next line only keeps the rows for students that have given consent
  if('ConsentPart' %in% names(DF)){
    DF<-subset(DF, ConsentPart=="I agree that my survey data may be used for the research study" | ConsentPart=="I agree that my survey data and my student information may be used for the research study" | ConsentPart==1 | ConsentPart==2)
  }
  if('ConsentAll' %in% names(DF)){
    DF<-subset(DF, ConsentAll=="I agree that my survey data may be used for the research study" | ConsentAll=="I agree that my survey data and my student information may be used for the research study" | ConsentAll=="I agree that my survey data and my student information data may be used for the research study" | ConsentAll==1 | ConsentAll==2 | ConsentAll=="1"| ConsentAll=="2")
  }
  
  print(length(DF$StudentID))
  print(length(unique(DF$StudentID)))
  
  
  ## rearrange the rows of the data frame so that responses with the most
  ## progress are placed first. This is helpful later so that the first response
  ## listed is the one to keep. The documentation in R for the 'sort.list' function
  ## says that the sort is stable meaning that in the case of a tie for how much
  ## progress there is, the entry that is currently first will stay first
  DF <- DF[sort.list(DF$Progress, decreasing = TRUE), ]
  
  ## vector indicating which columns are freeform answers
  freeform <- which(grepl("- TEXT", colnames(DF)))
  
  ## T/F vector indicating which columns are PIPS answers
  ## (Note: there are also some "PIPS_Lab -" columns in addition to "PIPS -" columns)
  pips <- grepl("PIPS -", colnames(DF))
  
  ## T/F vector indicating if an ID has shown up in a previous row
  dups <- duplicated(DF$StudentID)
  
  ## a data frame for first, (non-duplicated) responses and
  ## a data frame for any number of duplicated responses
  DF.first.responses <- DF[!dups,]
  DF.extra.responses <- DF[dups,]
  print(dim(DF.first.responses))
  print(dim(DF.extra.responses))
  
  if( (dim(DF.extra.responses)[1] )!= 0){
    for (i in 1:nrow(DF.first.responses)) { # for each unique person
      for (j in 1:nrow(DF.extra.responses)) { # check all duplicates of everyone
        ## if student from row i has a duplicate in row j do some stuff (IDs match)
        if (DF.first.responses$StudentID[i] == DF.extra.responses$StudentID[j]) {
          for (k in freeform) { # for each freeform comment
            ## if responses are same in both submissions, do nothing otherwise if
            ## they are different, copy content from duplicate to original
            ## (original has higher progress). The copying process below may happen 
            ## multiple times if there are multiple duplicates, so an entry may end up with 
            ## multiple '[[Dupe: ... ]]' parts.
            if (DF.first.responses[i,k] != DF.extra.responses[j,k]) {
              DF.first.responses[i,k] <- paste0(
                DF.first.responses[i,k], " [[Dupe: ",  DF.extra.responses[j,k], "]]"
              )
            }
          }   
        }
      }
    }
  }
  
  ## change spaces or blank cells to NA
  DF.first.responses <- apply(DF.first.responses, 2, function(x) gsub("^$|^ $", NA, x))
  
  ## check which students answered same value for all PIPS question
  t.f.remove <- apply(DF.first.responses, 1, function(row) {
    return(all.same(row[pips])) # are all PIPS entries the same
  })
  
  ## apply should.remove to each row of DF keeping only rows where it is false
  t.f.remove <- t.f.remove | apply(DF.first.responses, 1, should.remove) # should row be removed
  DF.first.responses <- as.data.frame(DF.first.responses[!t.f.remove, ])
  
  
  return(DF.first.responses) 
  
}

