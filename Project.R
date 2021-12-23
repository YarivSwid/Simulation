## ------------------------------------------------------olympics games - attributes -----------------------------------------------------------

##----------------------------------------- 1.  all functions ------------------------------------------------

trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

## add service 
addService<- function  (path,sname,timeDist){
  updatedPath <- seize(path, sname)%>%
    timeout(timeDist) %>%
    release(sname)
  
  return(updatedPath)
}


addServicevideoTesters<- function(path,videoRoom,times){
  #sname <- get_attribute(olympicsGames,"VideoTestersRoom")
  sname <- videoRoom
  #times <- get_attribute(olympicsGames,"counter")
  timeDist <- 0
  for(i in 1:times){
    timeDist <- timeDist+  trimmedNorm(5,1.7)
  }
  #set_attribute(key=c("counter"),value=function()0)
  timeout(path,timeDist)
  release(path,sname,1)
}


manAttributeInf<-function(){
  #return (c(0,1,1,1,1,1,1,getVideoRoom(),0))
  return (c(0,1,1,1,1,1,1,0,0))

}


getTiredness<-function(){
  
  return ()
}

getVideoRoom<-function(){
  roomNumber <- rdiscrete(1, c(0.2,0.2,0.2,0.2,0.2), values = 1:5)
  VideoRoom <- paste0("VideoTestersRoom",roomNumber)  
  return (VideoRoom)
}

getProbabilityVectorMan<-function(){
  #if(gender==man)
  ProbabilityVector <- get_attribute(olympicsGames)[2:7]
  leftOvers <- sum(ProbabilityVector)
  ProbabilityVector <- ProbabilityVector/leftOvers
  return (ProbabilityVector)
}



##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTimeolimpicsGames<-14*60
VideoTesters_schedule<-schedule(timetable = c(0, 120), values = c(0, 2), period = Inf)
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
  add_resource(name="VideoTestersRoom1", capacity=VideoTesters_schedule,queue_size=Inf)%>%
  add_resource(name="VideoTestersRoom2", capacity=VideoTesters_schedule,queue_size=Inf)%>%
  add_resource(name="VideoTestersRoom3", capacity=VideoTesters_schedule,queue_size=Inf)%>%
  add_resource(name="VideoTestersRoom4", capacity=VideoTesters_schedule,queue_size=Inf)%>%
  add_resource(name="VideoTestersRoom5", capacity=VideoTesters_schedule,queue_size=Inf)%>%
  add_resource(name="nutritionist1", capacity=nutritionist_schedule,queue_size=Inf)%>%
  add_resource(name="nutritionist2", capacity=nutritionist_schedule,queue_size=Inf)%>%
  add_resource(name="Physiotherapist", capacity=Physiotherapist_schedule,queue_size=Inf)%>%
  add_resource(name="MansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="WomansLockeRooms", capacity=20,queue_size=Inf)%>%
  add_resource(name="mansShower", capacity=5,queue_size=Inf)%>%
  add_resource(name="womansShower", capacity=5,queue_size=Inf)



#ParallelBarsTrajectory,ringsTrajectory,horizonalBarTrajectory,pommelHorseTrajectory,GroundWorkeoutTrajectory,jumpToolTrajectory) 

ParallelBarsTrajectory<-trajectory("ParallelBarsTrajectory")%>% 
  addService("ParallelBars",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ParallelBarsDone"),value=function()0)

ringsTrajectory<-trajectory("ringsTrajectory")%>% 
  addService("rings",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ringsDone"),value=function()0)

horizonalBarTrajectory<-trajectory("horizonalBarTrajectory")%>% 
  addService("horizonalBar",function()trimmedNorm(5,1.7))%>%
  set_attribute(key=c("horizonalBarDone"),value=function()0)

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

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------
VideoTestersTrajectory<-trajectory("VideoTestersTrajectory")%>% 
addServicevideoTesters("GroundWorkeout",1)
#addServicevideoTesters("GroundWorkeout",get_attribute(olympicsGames,"counter"))

manTrajectory<-trajectory("manTrajectory")%>% 
  set_attribute(keys=c("tiredness","ParallelBarsDone","ringsDone","horizonalBarDone","pommelHorseDone","GroundWorkeoutDone","jumpToolDone","VideoTestersRoom","counter"),value=function() manAttributeInf())%>%  #set is a part of the trajectory-always
  addService("MansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  # branch(option=function() rdiscrete (1, function()getProbabilityVectorMan(),c(1,2,3,4,5,6)) ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),ParallelBarsTrajectory,ringsTrajectory,horizonalBarTrajectory,pommelHorseTrajectory,GroundWorkeoutTrajectory,jumpToolTrajectory)%>% 
  # set_attribute(key=c("counter"),value=get_attribute(olympicsGames,"counter")+1)%>%
  # seize(function()get_attribute(olympicsGames,"VideoTestersRoom"), 1, continue = c(TRUE,TRUE) ,post.seize=VideoTestersTrajectory, reject =NULL )%>%
  # rollback(amount = 3, times = 5)



womanTrajectory<-trajectory("womanTrajectory")%>% 
  #עייפות-set_attribute(keys=c("makeTime","drinkTime"),value=function() coffeeInf())%>%  #set is a part of the trajectory-always
  addService("MansLockeRooms",function() runif(1,3,5))%>% #Getting organized in locker rooms
  
  
  
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
olympicsGames%>% 
  add_generator(name="gymnast_man", trajectory=manTrajectory, distribution=#קובץ אקסל)
                  add_generator(name="gymnast_woman", trajectory=womanTrajectory, distribution=#קובץ אקסל)
                                  
                                  
                                  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
                                
                                
                                
                                
                                