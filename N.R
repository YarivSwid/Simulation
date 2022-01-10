#create a table so we can view the values of the test
dataset <- 0
dataset <- data.frame(QueueMaxLengthInGroundWorkout=0,howMuchLeftOfTiredness=0,finishedPrec=0)

lambda <- 0.05
lambdaNew <- lambda/(1+lambda)
#divide by the amount of the alternatives
a <- 0.09/3

#get the delta for the n test
sdGround <- sd(groudWorkoutRep$MaxQueueLength)
meanGround <- mean(groudWorkoutRep$MaxQueueLength)
deltaGround <-  t*(sdGround/sqrt(n0))
deltaByMeanGround <- deltaGround/meanGround
#paste(deltaByMeanGround)
dataset$QueueMaxLengthInGroundWorkout <- deltaByMeanGround

#get the delta for the n test
sdTiredness <- sd(howMuchLeftOfTiredness$prec)
meanTiredness <- mean(howMuchLeftOfTiredness$prec)
deltaTiredness <-  (t*sdTiredness/sqrt(n0))
deltaBymeanTiredness <- deltaTiredness/meanTiredness
#paste(deltaBymeanTiredness)
dataset$howMuchLeftOfTiredness <- deltaBymeanTiredness

#get the delta for the n test
sdFinished <- sd(finishedPrec$Prec)
meanFinished <- mean(finishedPrec$Prec)
deltaFinished <-  t*(sdFinished/sqrt(n0))
deltaByMeanFinished <- deltaFinished/meanFinished
#paste(deltaByMeanFinished) 
dataset$finishedPrec <- deltaByMeanFinished

Nnew <- 15

#check which aspect has the bigger value
#check if the value is bigger then new lambda
if(deltaByMeanFinished<deltaBymeanTiredness & deltaByMeanGround<deltaBymeanTiredness & deltaBymeanTiredness>lambdaNew){
  #deltaByMeanTiredness was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew <- n0*((t*sdTiredness/sqrt(n0))/(meanTiredness*lambdaNew))^2
  Nnew <- ceiling(Nnew)
  paste("The new n is =",Nnew)
}
if(deltaByMeanFinished<deltaByMeanGround & deltaByMeanGround>deltaBymeanTiredness & deltaByMeanGround>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew <- n0*((t*sdGround/sqrt(n0))/(meanGround*lambdaNew))^2
  Nnew <- ceiling(Nnew)
  paste("The new n is =",Nnew)
}
if(deltaByMeanFinished>deltaByMeanGround & deltaByMeanFinished>deltaBymeanTiredness & deltaByMeanFinished>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew <- n0*((t*sdFinished/sqrt(n0))/(meanFinished*lambdaNew))^2
  Nnew <- ceiling(Nnew)
  paste("The new n is =",Nnew)
}else{}

##############option 1
dataset1 <- 0
dataset1 <- data.frame(QueueMaxLengthInGroundWorkout=0,howMuchLeftOfTiredness=0,finishedPrec=0)

#get the delta for the n test
sdGround1 <- sd(groudWorkoutRep1$MaxQueueLength)
meanGround1 <- mean(groudWorkoutRep1$MaxQueueLength)
deltaGround1 <-  t*(sdGround1/sqrt(n0))
deltaByMeanGround1 <- deltaGround1/meanGround1
#paste(deltaByMeanGround1)
dataset1$QueueMaxLengthInGroundWorkout <- deltaByMeanGround1

#get the delta for the n test
sdTiredness1 <- sd(howMuchLeftOfTiredness1$prec)
meanTiredness1 <- mean(howMuchLeftOfTiredness1$prec)
deltaTiredness1 <-  (t*sdTiredness1/sqrt(n0))
deltaBymeanTiredness1 <- deltaTiredness1/meanTiredness1
#paste(deltaBymeanTiredness1)
dataset1$howMuchLeftOfTiredness <- deltaBymeanTiredness1

#get the delta for the n test
sdFinished1 <- sd(finishedPrec1$Prec)
meanFinished1 <- mean(finishedPrec1$Prec)
deltaFinished1 <-  t*(sdFinished1/sqrt(n0))
deltaByMeanFinished1 <- deltaFinished1/meanFinished1
#paste(deltaByMeanFinished1) 
dataset1$finishedPrec <- deltaByMeanFinished1

#n0 was 15 so we created a default value
Nnew1 <- 15

#check which aspect has the bigger value
#check if the value is bigger then new lambda
if(deltaByMeanFinished1<deltaBymeanTiredness1 & deltaByMeanGround1<deltaBymeanTiredness1 & deltaBymeanTiredness1>lambdaNew){
  #deltaByMeanTiredness was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew1 <- n0*((t*sdTiredness1/sqrt(n0))/(meanTiredness1*lambdaNew))^2
  Nnew1 <- ceiling(Nnew1)
  paste("The new n is =",Nnew1)
}
if(deltaByMeanFinished1<deltaByMeanGround1 & deltaByMeanGround1>deltaBymeanTiredness1 & deltaByMeanGround1>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew1 <- n0*((t*sdGround1/sqrt(n0))/(meanGround1*lambdaNew))^2
  Nnew1 <- ceiling(Nnew1)
  paste("The new n is =",Nnew1)
}
if(deltaByMeanFinished1>deltaByMeanGround1 & deltaByMeanFinished1>deltaBymeanTiredness1 & deltaByMeanFinished1>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew1 <- n0*((t*sdFinished1/sqrt(n0))/(meanFinished1*lambdaNew))^2
  Nnew1 <- ceiling(Nnew1)
  paste("The new n is =",Nnew1)
}else{}

#########################option 2
dataset2 <- 0
dataset2 <- data.frame(QueueMaxLengthInGroundWorkout=0,howMuchLeftOfTiredness=0,finishedPrec=0)

#get the delta for the n test
sdGround2 <- sd(groudWorkoutRep2$MaxQueueLength)
meanGround2 <- mean(groudWorkoutRep2$MaxQueueLength)
deltaGround2 <-  t*(sdGround2/sqrt(n0))
deltaByMeanGround2 <- deltaGround2/meanGround2
#paste(deltaByMeanGround2)
dataset2$QueueMaxLengthInGroundWorkout <- deltaByMeanGround2

#get the delta for the n test
sdTiredness2 <- sd(howMuchLeftOfTiredness2$prec)
meanTiredness2 <- mean(howMuchLeftOfTiredness2$prec)
deltaTiredness2 <-  (t*sdTiredness2/sqrt(n0))
deltaBymeanTiredness2 <- deltaTiredness2/meanTiredness2
#paste(deltaBymeanTiredness2)
dataset2$howMuchLeftOfTiredness <- deltaBymeanTiredness2

#get the delta for the n test
sdFinished2 <- sd(finishedPrec2$Prec)
meanFinished2 <- mean(finishedPrec2$Prec)
deltaFinished2 <-  t*(sdFinished2/sqrt(n0))
deltaByMeanFinished2 <- deltaFinished2/meanFinished2
#paste(deltaByMeanFinished2) 
dataset2$finishedPrec <- deltaByMeanFinished2

Nnew2 <- 15

#check which aspect has the bigger value
#check if the value is bigger then new lambda
if(deltaByMeanFinished2<deltaBymeanTiredness2 & deltaByMeanGround2<deltaBymeanTiredness2 & deltaBymeanTiredness2>lambdaNew){
  #deltaByMeanTiredness was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew2 <- n0*((t*sdTiredness2/sqrt(n0))/(meanTiredness2*lambdaNew))^2
  Nnew2 <- ceiling(Nnew2)
  paste("The new n is =",Nnew2)
}
if(deltaByMeanFinished2<deltaByMeanGround2 & deltaByMeanGround2>deltaBymeanTiredness2 & deltaByMeanGround2>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew2 <- n0*((t*sdGround2/sqrt(n0))/(meanGround2*lambdaNew))^2
  Nnew2 <- ceiling(Nnew2)
  paste("The new n is =",Nnew2)
}
if(deltaByMeanFinished2>deltaByMeanGround2 & deltaByMeanFinished2>deltaBymeanTiredness2 & deltaByMeanFinished2>lambdaNew){
  #deltaByMeanGround was the maximum value and it was bigger then the new lambda
  #we will use the details of the table in order to find the new number of runs 
  Nnew2 <- n0*((t*sdFinished2/sqrt(n0))/(meanFinished2*lambdaNew))^2
  Nnew2 <- ceiling(Nnew2)
  paste("The new n is =",Nnew2)
}else{}

########### used functions to get data table to order details
#create a table for all data parameters
# table_sd_Mean_For_All_parameters <- data.frame(sd = c(sdTiredness,sdGround,sdFinished) , 
#                                                mean = c(meanTiredness,meanGround,meanFinished),
#                                                Relative_accuracy=c(deltaBymeanTiredness,deltaByMeanGround,deltaByMeanFinished)
# )
# rownames(table_sd_Mean_For_All_parameters) <- c("Tiredness","Ground","Finished")

##get Relative_accuracy 
# table_Relative_accuracy <- data.frame(Relative_accuracy=c(deltaBymeanTiredness,deltaByMeanGround,deltaByMeanFinished)
# )
# rownames(table_Relative_accuracy) <- c("Tiredness","Ground","Finished")