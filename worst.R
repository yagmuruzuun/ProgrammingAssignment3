library(gtools)
worst <- function(state, outcome){
data<- read.csv("outcome-of-care-measures.csv")
p<-1
if (!state %in% unique(data$State)){
  p=0
  stop("Invalid State")
}
if (!outcome %in% c("heart attack","heart failure","pneumonia")){
  p=0
  stop("Invalid Outcome")
}
if (p==1){
  if (outcome=="heart attack"){
    desired<-data[data$State==state,]
    desired<-desired[mixedorder(desired$Hospital.Name),]
    desired<-desired[desired$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==max(desired[,11]),]
    return(desired[1,2])
  }
  if(outcome=="heart failure"){
    desired<-data[data$State==state,]
    desired<-desired[mixedorder(desired$Hospital.Name),]
    desired<-desired[desired$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==max(desired[,17]),]
    return(desired[1,2])
  }
  if(outcome=="pneumonia"){
    desired<-data[data$State==state,]
    desired<-desired[mixedorder(desired$Hospital.Name),]
    desired<-desired[desired$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==max(desired[,23]),]
    return(desired[1,2])
  }
}
}

