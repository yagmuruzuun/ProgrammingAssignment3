library(gtools)
rankhospital <- function(state,outcome,num="best"){
data<-read.csv("outcome-of-care-measures.csv")
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
    if (num=="best"){
      return(best(state,outcome))
    } else if (num=="worst"){
      return(worst(state,outcome))
    } else {
      desired<-data[data$State==state,]
      desired<-desired[mixedorder(desired$Hospital.Name),]
      desired<-desired[mixedorder(desired$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      return(desired[num,2])
    }
  }
  if(outcome=="heart failure"){
    if (num=="best"){
      return(best(state,outcome))
    } else if (num=="worst"){
      return(worst(state,outcome))
    } else {
      desired<-data[data$State==state,]
      desired<-desired[mixedorder(desired$Hospital.Name),]
      desired<-desired[mixedorder(desired$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      return(desired[num,2])
      }
  }
  if(outcome=="pneumonia"){
    if (num=="best"){
      return(best(state,outcome))
    } else if (num=="worst"){
      return(worst(state,outcome))
    } else {
      desired<-data[data$State==state,]
      desired<-desired[mixedorder(desired$Hospital.Name),]
      desired<-desired[mixedorder(desired$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      return(desired[num,2])
    }
  }
}
}
