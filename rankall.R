library(gtools)
library(stringr)
rankall<- function(outcome,num="best"){
data <- read.csv("outcome-of-care-measures.csv")
states<-unique(data[,7])
states<-str_sort(states)
desired=data.frame()
p<-1
if(!outcome %in% c("heart attack","heart failure","pneumonia")){
  p<-0
  stop("Invalid Outcome")
}
if (p==1){
  if(outcome=="heart attack"){
    if (num=="best"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else if(num=="worst"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,decreasing = TRUE),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else{
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[num,c(2,7)])
        }
    }
  }
  if(outcome=="heart failure"){
    if (num=="best"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else if(num=="worst"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,decreasing = TRUE),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else{
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[num,c(2,7)])
        }
    }
  }
  if(outcome=="pneumonia"){
    if (num=="best"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else if(num=="worst"){
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,decreasing = TRUE),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[1,c(2,7)])
        }
    } else{
        for (i in 1:length(states)){
            output<-data[data$State==states[i],]
            output<-output[mixedorder(output$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
            output<-output[mixedorder(output$State),]
            desired<-rbind(desired,output[num,c(2,7)])
        }
    }
  }
}
return(desired)
}
