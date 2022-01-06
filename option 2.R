##----------------------------------------- 0.  fit gymnas arrival distribution from imported data ------------------------------------------------

#table <- read.csv(file.choose(),header = T)
#male_arrival_time <- table[,5] 
#female_arrival_time <- table[,6]  

#fit_male1 <- fitdist(male_arrival_time,"norm")
#fit_male2 <- fitdist(male_arrival_time,"exp") 

#summary(fit_male1)  
#summary(fit_male2)


#fit_female1 <- fitdist(female_arrival_time,"norm")
#fit_female2 <- fitdist(female_arrival_time,"exp") 

#summary(fit_female1)  
#summary(fit_female2)

#we chose exponnetial distribution for both male and female bacause they have better likehood, AIC and BIC

#male_lamda <- as.double(fit_male2$estimate[1])
#female_lamda <- as.double(fit_female2$estimate[1])


##----------------------------------------- 1.  all functions ------------------------------------------------



avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }#if
    }#for
  }#end if
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}#end func

#basic resource service 
addService<- function  (path,sname,timeDist){ 
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}
#return value from trimmedNorm distribution
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}


#Returns the start time of a nutrition lecture(full hour) 
findStartTime<- function(now){ 
  thisTime <- now
  endTime <- 60-thisTime%%60
  return(endTime)
}

#Check if the gymnast_man should continue to access the appliances (will not continue if he performed all the appliances or got tired)
getIfMaxTiredMan <- function(tiredness,times){
  continue <- TRUE
  if(tiredness>=2.9){
    continue <- FALSE
  }
  if(times>=6){
    continue <- FALSE
  }
  return(continue)
}

#Check if the gymnast_Woman should continue to access the appliances (will not continue if he performed all the appliances or got tired)
getIfMaxTiredWoman <- function(tiredness,times){
  continue <- TRUE
  if(tiredness>=2.4){
    continue <- FALSE
  }
  if(times>=4){
    continue <- FALSE
  }
  return(continue)
}

#return the update Priority for Man
getPriorityMan <- function(tiredness){
  if(tiredness>=2.9){
    return(1)
  }
  return(0)
}

#return the update Priority for Man
getPriorityWoman <- function(tiredness){
  if(tiredness>=2.4){
    return(1)
  }
  return(0)
}

#calculate the tierdness for each gymnast after a appliances
tierdnessValue <- function()
{
  u1 <- runif(1,0,1)
  u2 <- runif(1,0,1)
  
  if(u1 <1/3)
  {AddToTiredness <-  (u2/8)^(1/3)}
  
  if(1/3<= u1 & u1<2/3)
  {AddToTiredness <- u2/6}
  
  if(2/3<= u1 & u1<1)
  {AddToTiredness <-  (1+(9-u2)^(1/2))/3}
  
  return(AddToTiredness) 
}

#return the total time of Video watching (until the gymnast entered the video room)
timeoutVideo <- function(counter){
  timeDist <- 0
  for(i in 1:counter){
    timeDist <- timeDist +  trimmedNorm(3,45/60)
  } 
  return(timeDist) 
}

#return the starting value of man Attribute vector
manAttributeInf<-function(){
  vidRoom <- getVideoRoom()
  return(c(0,1,1,1,1,1,1,vidRoom,0,0))
}

#return the starting value of woman Attribute vector
womanAttributeInf<-function(){
  vidRoom <- getVideoRoom()
  return(c(0,1,1,1,1,vidRoom,0,0))
}

#choose Random video room for each gymnast
getVideoRoom<-function(){
  roomNumber <- rdiscrete(1, c(0.2,0.2,0.2,0.2,0.2), values = 1:5)
  return (roomNumber)
}

#return the Probability Vector for the appliances selection
getProbabilityVector<-function(vect){
  leftOvers <- sum(vect)
  vect <- vect/leftOvers
  return (vect)
}

# coffe 
# nuterist on zoom
# tiredness

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------


simulationTimeolimpicsGames<-14*60

#Video testers start the day at 8 am-schedule capacity value 
VideoTesters_schedule<-schedule(timetable = c(0,120), values = c(0, 2), period = Inf)

#the gymnast need to return to performed the appliances if the Video testers did not arrived yet
VideoTesters_schedule_queue<-schedule(timetable = c(0,120), values = c(0, Inf), period = Inf)

#nutritionist start the day at 8 am-schedule capacity value 
nutritionist_schedule<-schedule(timetable = c(0, 120), values = c(0, 1), period = Inf)

#the Physiotherapists squad changed during the day -schedule capacity value 
Physiotherapist_schedule<-schedule(timetable = c(0, 120,360,600), values = c(0,2,5,3), period = Inf)


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

olympicsGames<-
  simmer("olympicsGames") %>%
  add_resource(name="barA", capacity=1,queue_size=Inf) %>% 
  add_resource(name="barB", capacity=1,queue_size=Inf) %>% 
  add_resource(name="GroundWorkeout", capacity=1,queue_size=Inf)%>%
  add_resource(name="ParallelBars", capacity=1,queue_size=Inf)%>%
  add_resource(name="gradualParallelBars", capacity=1,queue_size=Inf)%>%
  add_resource(name="rings", capacity=1,queue_size=Inf)%>%
  add_resource(name="pommelHorse", capacity=1,queue_size=Inf)%>%
  add_resource(name="horizonalBar", capacity=1,queue_size=Inf)%>%
  add_resource(name="jumpToolA", capacity=1,queue_size=Inf)%>%
  add_resource(name="jumpToolB", capacity=1,queue_size=Inf)%>%
  add_resource(name="VideoTestersRoom1", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue)%>%
  add_resource(name="VideoTestersRoom2", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue)%>%
  add_resource(name="VideoTestersRoom3", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue)%>%
  add_resource(name="VideoTestersRoom4", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue)%>%
  add_resource(name="VideoTestersRoom5", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue)%>%
  add_resource(name="nutritionist1", capacity=nutritionist_schedule,queue_size=Inf)%>%
  add_resource(name="nutritionist2", capacity=nutritionist_schedule,queue_size=Inf)%>%
  add_resource(name="Physiotherapist", capacity=Physiotherapist_schedule,queue_size=Inf)%>%
  add_resource(name="MansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="WomansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="mansShower", capacity=5,queue_size=Inf)%>%
  add_resource(name="womansShower", capacity=5,queue_size=Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

#-------------------4.1 All the appliances trajectories-------------------------------

ParallelBarsTrajectory<-trajectory("ParallelBarsTrajectory")%>%
  addService("ParallelBars",function()trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("ParallelBarsDone"),value=function()0)#update that the gymnast performed this appliance

ringsTrajectory<-trajectory("ringsTrajectory")%>%
  addService("rings",function()trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("ringsDone"),value=function()0)

horizonalBarTrajectory<-trajectory("horizonalBarTrajectory")%>%
  addService("horizonalBar",function()trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("horizonalBarDone"),value=function() 0)

pommelHorseTrajectory<-trajectory("pommelHorseTrajectory")%>%
  addService("pommelHorse",function()trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("pommelHorseDone"),value=function()0)

GroundWorkeoutTrajectory<-trajectory("GroundWorkeoutTrajectory")%>%
  addService("GroundWorkeout",function()trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("GroundWorkeoutDone"),value=function()0)

#we have two jump tools, so the gymnast will go to the jump tool with the shortest queue
jumpToolTrajectory<-trajectory("jumpToolTrajectory")%>% 
  simmer::select(resources =c("jumpToolA","jumpToolB"),policy ="shortest-queue-available" ) %>%
  seize_selected(amount = 1) %>%
  timeout(function()trimmedNorm(5*0.75,1.7)) %>%
  release_selected(amount = 1)%>%
  set_attribute(key=c("jumpToolDone"),value=function()0)

#we have two bars, so the gymnast will go to the bar with the shortest queue
BarWorkeoutTrajectory<-trajectory("BarWorkeoutTrajectory")%>%
  simmer::select(resources =c("barA","barB"),policy ="shortest-queue-available" ) %>%
  seize_selected(amount = 1) %>%
  timeout(function()trimmedNorm(5*0.75,1.7)) %>%
  release_selected(amount = 1)%>%
  set_attribute(key=c("BarDone"),value=function()0)

gradualParallelBarsTrajectory<-trajectory("gradualParallelBarsTrajectory")%>%
  addService("gradualParallelBars",function() trimmedNorm(5*0.75,1.7))%>%
  set_attribute(key=c("gradualParallelBarsDone"),value=function()0)


#-------------------4.2Video Testers service trajectories-------------------------------

#success trajectory - (post.seize) - if the gymnast catch the Video Tester (after 8:00 am) he go to this trajectory
VideoTestersTrajectory<-trajectory("VideoTestersTrajectory")%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%#select again the Video Testers Room he success to seize in the main trajectory   
  timeout(function() timeoutVideo(get_attribute(olympicsGames,"counter")))%>% #watch "counter" videos
  release_selected(amount = 1)%>%
  set_attribute(key=c("counter"),value=function() 0)#update the counter value to 0 (how many videos the gymnast has left to watch)

#reject trajectory - (reject) - if the gymnast did not catch the Video Tester (before 8:00 am) he go to this trajectory
didntWatchTheVideo<-trajectory("didntWatchTheVideo")%>%
  log_("")


#-------------------4.3 nutritionist trajectories-------------------------------

nutritionistTrajectory<-trajectory("nutritionistTrajectory")%>%
  batch(10, timeout = function() findStartTime(now(olympicsGames)), permanent = FALSE)%>% #"waiting area"
  simmer::select(resources = c("nutritionist1","nutritionist2"),policy ="shortest-queue") %>%
  seize_selected(amount=1)%>%
  timeout(function() runif(1,30,40))%>%
  release_selected(amount=1)%>%
  separate()


#-------------------4.4 breaks trajectories-------------------------------


#-----sent the Physiotherapists to break

breakPhysiotherapistTrajectory <- trajectory("breakPhysiotherapistTrajectory")%>%
  seize("Physiotherapist",amount=5)%>%#"seize" all the Physiotherapist in the room (sent them to break )
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("Physiotherapist",amount=5)

#---sent the Nutritionists to break

breakNutritionist1Trajectory <- trajectory("breakNutritionist1Trajectory")%>%
  seize("nutritionist1",amount=1)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("nutritionist1",amount=1)

breakNutritionist2Trajectory <- trajectory("breakNutritionist2Trajectory")%>%
  seize("nutritionist2",amount=1)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("nutritionist2",amount=1)


#---sent the Video testers to break

breakVideo1Trajectory <- trajectory("breakVideo1Trajectory")%>%
  seize("VideoTestersRoom1",amount=2)%>%#"seize" both Video testers in room 1 (sent them to break )
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("VideoTestersRoom1",amount=2)

breakVideo2Trajectory <- trajectory("breakVideo2Trajectory")%>%
  seize("VideoTestersRoom2",amount=2)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("VideoTestersRoom2",amount=2)

breakVideo3Trajectory <- trajectory("breakVideo3Trajectory")%>%
  seize("VideoTestersRoom3",amount=2)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("VideoTestersRoom3",amount=2)

breakVideo4Trajectory <- trajectory("breakVideo4Trajectory")%>%
  seize("VideoTestersRoom4",amount=2)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("VideoTestersRoom4",amount=2)

breakVideo5Trajectory <- trajectory("breakVideo5Trajectory")%>%
  seize("VideoTestersRoom5",amount=2)%>%
  timeout(function() get_global(olympicsGames,"breakTime"))%>%
  release("VideoTestersRoom5",amount=2)

#---------main break Trajectory 

#clone the fictive entity(break) and send the Duplicates entities to each professional so the entities can send them to a break
breakTrajectory <- trajectory("breakTrajectory")%>%
  set_global(keys = "breakTime",value=trimmedNorm(6,50/60))%>%# save uniform break Time for all professionals
  clone(8,breakPhysiotherapistTrajectory,breakNutritionist1Trajectory,breakNutritionist2Trajectory,breakVideo1Trajectory,breakVideo2Trajectory,breakVideo3Trajectory,breakVideo4Trajectory,breakVideo5Trajectory)%>%
  synchronize(wait=TRUE)



#-------------------4.5 MAIN trajectories-------------------------------

#-----man main trajectory

manTrajectory<-trajectory("manTrajectory")%>%
  set_attribute(keys = c("tiredness","ParallelBarsDone","ringsDone","horizonalBarDone","pommelHorseDone","GroundWorkeoutDone","jumpToolDone","VideoTestersRoom","counter","times"),value = function() manAttributeInf())%>%  #set is a part of the trajectory-always
  addService("MansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  branch(option=function() rdiscrete (1,getProbabilityVector(get_attribute(olympicsGames,c("ParallelBarsDone","ringsDone","horizonalBarDone","pommelHorseDone","GroundWorkeoutDone","jumpToolDone"))),c(1,2,3,4,5,6)) ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),ParallelBarsTrajectory,ringsTrajectory,horizonalBarTrajectory,pommelHorseTrajectory,GroundWorkeoutTrajectory,jumpToolTrajectory)%>%
  set_attribute(key=c("counter"),value=function() get_attribute(olympicsGames,"counter")+1)%>%
  set_attribute(key=c("tiredness"),value=function()tierdnessValue(),mod="+")%>%
  set_attribute(key=c("times"),value=function() get_attribute(olympicsGames,"times")+1)%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%
  seize_selected(1, continue = c(TRUE,TRUE) ,post.seize=VideoTestersTrajectory, reject =didntWatchTheVideo )%>%
  rollback(amount = 6,check = function() getIfMaxTiredMan(get_attribute(olympicsGames,"tiredness"),get_attribute(olympicsGames,"times")))%>%
  branch (option = function() rdiscrete(1,c(0.39,0.61),c(0,1)), continue = c(TRUE) , nutritionistTrajectory)%>%
  set_prioritization(function() c(getPriorityMan(get_attribute(olympicsGames,"tiredness")),2,FALSE))%>%
  addService("Physiotherapist",function() rtriangle(1,25,40,33))%>%
  addService("mansShower",function() runif(1,8,14))


#-----woman main trajectory

womanTrajectory<-trajectory("womanTrajectory")%>%
  set_attribute(keys = c("tiredness","GroundWorkeoutDone","gradualParallelBarsDone","BarDone","jumpToolDone","VideoTestersRoom","counter","times"),value = function() womanAttributeInf())%>%  #set is a part of the trajectory-always
  addService("WomansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  branch(option=function() rdiscrete (1,getProbabilityVector(get_attribute(olympicsGames,c("GroundWorkeoutDone","gradualParallelBarsDone","BarDone","jumpToolDone"))),c(1,2,3,4)) ,continue= c(TRUE,TRUE,TRUE,TRUE),GroundWorkeoutTrajectory,gradualParallelBarsTrajectory,BarWorkeoutTrajectory,jumpToolTrajectory)%>%
  set_attribute(key=c("counter"),value=function() get_attribute(olympicsGames,"counter")+1)%>%
  set_attribute(key=c("tiredness"),value=function()tierdnessValue(),mod="+")%>%
  set_attribute(key=c("times"),value=function() get_attribute(olympicsGames,"times")+1)%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%
  seize_selected(1, continue = c(TRUE,TRUE) ,post.seize=VideoTestersTrajectory, reject =didntWatchTheVideo )%>%
  rollback(amount = 6,check = function() getIfMaxTiredWoman(get_attribute(olympicsGames,"tiredness"),get_attribute(olympicsGames,"times")))%>%
  branch (option = function() rdiscrete(1,c(0.39,0.61),c(0,1)), continue = c(TRUE) , nutritionistTrajectory)%>%
  set_prioritization(function() c(getPriorityWoman(get_attribute(olympicsGames,"tiredness")),2,FALSE))%>%
  addService("Physiotherapist",function() rtriangle(1,25,40,33))%>%
  addService("womansShower",function() runif(1,8,14))

##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

olympicsGames%>% 
  add_generator(name="gymnast_man", trajectory=manTrajectory, distribution=to(540,function() rexp(1,0.89955077)),mon=2,priority=0,preemptible = 3,restart = TRUE)%>%  
  add_generator(name="gymnast_woman", trajectory=womanTrajectory, distribution=to(540,function() rexp(1,0.7903051)),mon=2,priority=0,preemptible = 3,restart = TRUE)%>%
  add_generator(name="break", trajectory=breakTrajectory,distribution=at(420),mon=2,priority=10,restart = TRUE)

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------

#set.seed(456)
#reset(olympicsGames)%>%run(until=simulationTimeolimpicsGames)
mm1envs <- mclapply(1:100, function(i) {
  set.seed(((i+100)^2)*3-7)
  reset(olympicsGames)%>%run(until=simulationTimeolimpicsGames) %>%
    wrap()
})

# now can use simmer functions like get_mon_arrivals on the array envs:
arrivals_Rep1 <- get_mon_arrivals(mm1envs[[1]]) # data of replication 1 only 
arrivals_Rep2 <- get_mon_arrivals(mm1envs[[99]]) # data of replication 2 only 

#With all these replicas, we could, for instance, perform a t-test for the flow time over all the customers in the system

fullData<-get_mon_arrivals(mm1envs) # the full data of all replications
fullDataAtt <- get_mon_attributes(mm1envs)
fullDataFalse<-get_mon_arrivals(mm1envs,T) # the full data of all replications
#fullDataFalse<-get_mon_arrivals(mm1envs,T) # the full data of all replications
resourcesRep<-get_mon_resources(mm1envs) # the full data of all replications

timeAtPhsQueue <- sqldf("select replication, avg(end_time - start_time-activity_time) 
from fullDataFalse
where resource == 'Physiotherapist'
group by replication");


folowVideo <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'VideoTestersRoom1'
group by replication")

groudWorkoutRep <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'GroundWorkeout'
group by replication")

ParallelBarsRep <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'ParallelBars'
group by replication")

gradualParallelBarsRep <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'gradualParallelBars'
group by replication")

ringsRep <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'rings'
group by replication")

FolowMeanData <- sqldf(
  "select replication, avg(end_time - start_time) as meanFlow
from fullData
group by replication")

QueueMean <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullData
group by replication")

FolowMeanDataActivitytime <- sqldf(
  "select replication,activity_time  as meanFlow
from fullData
group by replication")

FolowMeanDataActivitytime <- sqldf(
  "select replication,activity_time  as meanFlow
from fullData
group by replication")

howMuchLeft <- sqldf(
  "select replication,count(*)  as meanFlow
from fullDataAtt
where (key=='tiredness'and value>2.9 and name not like '%woman%')or(key=='tiredness'and value>2.4 and name like '%woman%')
group by replication")

folowVideo <- sqldf(
  "select replication, avg(end_time - start_time-activity_time) as meanFlow
from fullDataFalse
where resource == 'VideoTestersRoom1'
group by replication")
mon_resources <-  get_mon_resources(olympicsGames)
mon_arrivals<-get_mon_arrivals(olympicsGames,ongoing = T)
mon_arrivalsWithoutOngoing<-get_mon_arrivals(olympicsGames,ongoing = F)
mon_attributes <- get_mon_attributes(olympicsGames)

leftTired <- sqldf(" select count(*)
              from mon_attributes
              where value >2.9 and key = 'tiredness'")
paste(leftTired)
barAdata <- sqldf("select *
                     from mon_resources
                     where resource=='barA'")

barBdata <- sqldf("select *
                     from mon_resources
                     where resource=='barB'")
GroundWorkeoutdata <- sqldf("select *
                     from mon_resources
                     where resource=='GroundWorkeout'")
ParallelBarsdata <- sqldf("select *
                     from mon_resources
                     where resource=='ParallelBars'")
gradualParallelBars<- sqldf("select *
                     from mon_resources
                     where resource=='gradualParallelBars'")
time <- as.matrix(ParallelBarsdata$time);
queueLength <- as.matrix(ParallelBarsdata$queue);
avgResQueue <- avgQueue(time, queueLength, simulationTimeolimpicsGames)
paste(avgResQueue)
paste("Average queue len for the barista was ",avgResQueue, "people")



