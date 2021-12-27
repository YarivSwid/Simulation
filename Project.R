findStartTime<- function(now){
  thisTime <- now
  endTime <- 60-thisTime%%60
  return(endTime)
}
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

getPriorityMan <- function(tiredness){
  if(tiredness>=2.9){
    return(1)
  }
  return(0)
}
getPriorityWoman <- function(tiredness){
  if(tiredness>=2.4){
    return(1)
  }
  return(0)
}
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

trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}

timeoutVideo <- function(counter){
  timeDist <- 0
  for(i in 1:counter){
    timeDist <- timeDist +  trimmedNorm(3,45/60)
  } 
  return(timeDist) 
}

manAttributeInf<-function(){
  vidRoom <- getVideoRoom()
    return(c(0,1,1,1,1,1,1,vidRoom,0,0))
}
womanAttributeInf<-function(){
  vidRoom <- getVideoRoom()
  return(c(0,1,1,1,1,vidRoom,0,0))
}
  
getVideoRoom<-function(){
  roomNumber <- rdiscrete(1, c(0.2,0.2,0.2,0.2,0.2), values = 1:5)
  return (roomNumber)
}

getProbabilityVectorMan<-function(vect){
  leftOvers <- sum(vect)
  vect <- vect/leftOvers
  return (vect)
}




##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTimeolimpicsGames<-14*60
VideoTesters_schedule<-schedule(timetable = c(0,120), values = c(0, 2), period = Inf)
VideoTesters_schedule_queue<-schedule(timetable = c(0,120), values = c(0, Inf), period = Inf)
nutritionist_schedule<-schedule(timetable = c(0, 120), values = c(0, 1), period = Inf)
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
  add_resource(name="VideoTestersRoom1", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue,preemptive =TRUE)%>%
  add_resource(name="VideoTestersRoom2", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue,preemptive =TRUE)%>%
  add_resource(name="VideoTestersRoom3", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue,preemptive =TRUE)%>%
  add_resource(name="VideoTestersRoom4", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue,preemptive =TRUE)%>%
  add_resource(name="VideoTestersRoom5", capacity=VideoTesters_schedule,queue_size=VideoTesters_schedule_queue,preemptive =TRUE)%>%
  add_resource(name="nutritionist1", capacity=nutritionist_schedule,queue_size=Inf,preemptive =TRUE)%>%
  add_resource(name="nutritionist2", capacity=nutritionist_schedule,queue_size=Inf,preemptive =TRUE)%>%
  add_resource(name="Physiotherapist", capacity=Physiotherapist_schedule,queue_size=Inf,preemptive = TRUE)%>%
  add_resource(name="MansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="WomansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="mansShower", capacity=5,queue_size=Inf)%>%
  add_resource(name="womansShower", capacity=5,queue_size=Inf)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

ParallelBarsTrajectory<-trajectory("ParallelBarsTrajectory")%>%
  addService("ParallelBars",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ParallelBarsDone"),value=function()0)

ringsTrajectory<-trajectory("ringsTrajectory")%>%
  addService("rings",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ringsDone"),value=function()0)

horizonalBarTrajectory<-trajectory("horizonalBarTrajectory")%>%
  addService("horizonalBar",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("horizonalBarDone"),value=function() 0)

pommelHorseTrajectory<-trajectory("pommelHorseTrajectory")%>%
  addService("pommelHorse",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("pommelHorseDone"),value=function()0)

GroundWorkeoutTrajectory<-trajectory("GroundWorkeoutTrajectory")%>%
  addService("GroundWorkeout",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("GroundWorkeoutDone"),value=function()0)

jumpToolTrajectory<-trajectory("jumpToolTrajectory")%>% 
  simmer::select(resources =c("jumpToolA","jumpToolB"),policy ="shortest-queue-available" ) %>%
  seize_selected(amount = 1) %>%
  timeout(function()trimmedNorm(5,1.7)) %>%
  release_selected(amount = 1)%>%
  set_attribute(key=c("jumpToolDone"),value=function()0)

BarWorkeoutTrajectory<-trajectory("BarWorkeoutTrajectory")%>%
  simmer::select(resources =c("barA","barB"),policy ="shortest-queue-available" ) %>%
  seize_selected(amount = 1) %>%
  timeout(function()trimmedNorm(5,1.7)) %>%
  release_selected(amount = 1)%>%
  set_attribute(key=c("BarDone"),value=function()0)

gradualParallelBarsTrajectory<-trajectory("gradualParallelBarsTrajectory")%>%
  addService("ParallelBars",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("gradualParallelBarsDone"),value=function()0)


VideoTestersTrajectory<-trajectory("VideoTestersTrajectory")%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%
  timeout(function() timeoutVideo(get_attribute(olympicsGames,"counter")))%>%
  release_selected(amount = 1)%>%
  set_attribute(key=c("counter"),value=function() 0)

didntWatchTheVideo<-trajectory("didntWatchTheVideo")%>%
  log_("")
  
nutritionistTrajectory<-trajectory("nutritionistTrajectory")%>%
  batch(10, timeout = function() findStartTime(now(olympicsGames)), permanent = FALSE)%>% #"waiting area"
  simmer::select(resources = c("nutritionist1","nutritionist2"),policy ="shortest-queue-available") %>%
  seize_selected(amount=1)%>%
  timeout(function() runif(1,30,40))%>%
  release_selected(amount=1)%>%
  separate()

breakPhysiotherapistTrajectory <- trajectory("breakPhysiotherapistTrajectory")%>%
    seize("Physiotherapist",amount=5)%>%
    timeout(function() get_global(olympicsGames,"breakTime"))%>%
    release("Physiotherapist",amount=5)

breakNutritionist1Trajectory <- trajectory("breakNutritionist1Trajectory")%>%
    seize("nutritionist1",amount=1)%>%
    timeout(function() get_global(olympicsGames,"breakTime"))%>%
    release("nutritionist1",amount=1)

breakNutritionist2Trajectory <- trajectory("breakNutritionist2Trajectory")%>%
    seize("nutritionist2",amount=1)%>%
    timeout(function() get_global(olympicsGames,"breakTime"))%>%
    release("nutritionist2",amount=1)

breakVideo1Trajectory <- trajectory("breakVideo1Trajectory")%>%
    seize("VideoTestersRoom1",amount=2)%>%
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

breakTrajectory <- trajectory("breakTrajectory")%>%
  set_global(keys = "breakTime",value=trimmedNorm(6,50/60))%>%
  clone(8,breakPhysiotherapistTrajectory,breakNutritionist1Trajectory,breakNutritionist2Trajectory,breakVideo1Trajectory,breakVideo2Trajectory,breakVideo3Trajectory,breakVideo4Trajectory,breakVideo5Trajectory)%>%
  synchronize(wait=TRUE)


manTrajectory<-trajectory("manTrajectory")%>%
  set_global(keys = "HighTirednessMan",value=function() 2.9)%>%
  set_attribute(keys = c("tiredness","ParallelBarsDone","ringsDone","horizonalBarDone","pommelHorseDone","GroundWorkeoutDone","jumpToolDone","VideoTestersRoom","counter","times"),value = function() manAttributeInf())%>%  #set is a part of the trajectory-always
  addService("MansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  branch(option=function() rdiscrete (1,getProbabilityVectorMan(get_attribute(olympicsGames,c("ParallelBarsDone","ringsDone","horizonalBarDone","pommelHorseDone","GroundWorkeoutDone","jumpToolDone"))),c(1,2,3,4,5,6)) ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),ParallelBarsTrajectory,ringsTrajectory,horizonalBarTrajectory,pommelHorseTrajectory,GroundWorkeoutTrajectory,jumpToolTrajectory)%>%
  set_attribute(key=c("counter"),value=function() get_attribute(olympicsGames,"counter")+1)%>%
  set_attribute(key=c("tiredness"),value=function()tierdnessValue(),mod="+")%>%
  set_attribute(key=c("times"),value=function() get_attribute(olympicsGames,"times")+1)%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%
  seize_selected(1, continue = c(TRUE,TRUE) ,post.seize=VideoTestersTrajectory, reject =didntWatchTheVideo )%>%
  rollback(amount = 6,check = function() getIfMaxTiredMan(get_attribute(olympicsGames,"tiredness"),get_attribute(olympicsGames,"times")))%>%
  branch (option = function() rdiscrete(1,c(0.32,0.68),c(0,1)), continue = c(TRUE) , nutritionistTrajectory)%>%
  set_prioritization(function() c(getPriorityMan(get_attribute(olympicsGames,"tiredness")),2,FALSE))%>%
  addService("Physiotherapist",function() rtriangle(1,25,40,33))%>%
  addService("mansShower",function() runif(1,8,14))

womanTrajectory<-trajectory("womanTrajectory")%>%
  set_global(keys = "HighTirednessWoman",value=function() 2.4)%>%
  set_attribute(keys = c("tiredness","GroundWorkeoutDone","gradualParallelBarsDone","BarDone","jumpToolDone","VideoTestersRoom","counter","times"),value = function() womanAttributeInf())%>%  #set is a part of the trajectory-always
  addService("WomansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  branch(option=function() rdiscrete (1,getProbabilityVectorMan(get_attribute(olympicsGames,c("GroundWorkeoutDone","gradualParallelBarsDone","BarDone","jumpToolDone"))),c(1,2,3,4)) ,continue= c(TRUE,TRUE,TRUE,TRUE),GroundWorkeoutTrajectory,gradualParallelBarsTrajectory,BarWorkeoutTrajectory,jumpToolTrajectory)%>%
  set_attribute(key=c("counter"),value=function() get_attribute(olympicsGames,"counter")+1)%>%
  set_attribute(key=c("tiredness"),value=function()tierdnessValue(),mod="+")%>%
  set_attribute(key=c("times"),value=function() get_attribute(olympicsGames,"times")+1)%>%
  simmer::select(resources = function() paste0("VideoTestersRoom",get_attribute(olympicsGames,"VideoTestersRoom"))) %>%
  seize_selected(1, continue = c(TRUE,TRUE) ,post.seize=VideoTestersTrajectory, reject =didntWatchTheVideo )%>%
  rollback(amount = 6,check = function() getIfMaxTiredWoman(get_attribute(olympicsGames,"tiredness"),get_attribute(olympicsGames,"times")))%>%
  branch (option = function() rdiscrete(1,c(0.32,0.68),c(0,1)), continue = c(TRUE) , nutritionistTrajectory)%>%
  set_prioritization(function() c(getPriorityWoman(get_attribute(olympicsGames,"tiredness")),2,FALSE))%>%
  addService("Physiotherapist",function() rtriangle(1,25,40,33))%>%
  addService("womansShower",function() runif(1,8,14))
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
olympicsGames%>% 
  add_generator(name="gymnast_man", trajectory=manTrajectory, distribution=function() trimmedNorm(1.134,1.047),mon=2,priority=0,preemptible = 3,restart = TRUE)%>%  
  add_generator(name="gymnast_woman", trajectory=womanTrajectory, distribution=function() rexp(1,0.881),mon=2,priority=0,preemptible = 3,restart = TRUE)%>%
  add_generator(name="break", trajectory=breakTrajectory,distribution=at(420),mon=2,priority=10,restart = TRUE)

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------

set.seed(345)
reset(olympicsGames)%>%run(until=simulationTimeolimpicsGames)
                                  
mon_resources <-  get_mon_resources(olympicsGames)
mon_arrivals<-get_mon_arrivals(olympicsGames)
mon_attributes <- get_mon_attributes(olympicsGames)

                                
                                