
# the main function for writing data based bus stop schedules for weekdays,
# Saturdays and Sundays
# input:
# stops		GTFS stops.txt file contents as a data frame
# TT		data frame containing link travel times from several weeks of working days
# TTsa		data frame containing link travel times from several Saturdays
# TTsu		data frame containing link travel times from several Sundays
# limit		the proportion of data that is taken into use, e.g. limit 0.9 indicates that 
#			5% of the earliest and 5% of the latest observations are discarded
# output:
# no output, the results are written in files 
WriteAll = function(stops,TT,TTsa,TTsu,limit){
  WriteSchedules(stops,TT,limit,"WeekDays")
  WriteSchedules(stops,TTsa,limit,"Saturdays")
  WriteSchedules(stops,TTsu,limit,"Sundays")
}

# intermediate level function for writing all bus schedules
# input:
# stops		GTFS stops.txt file contents as a data frame
# TT		data frame containing link travel times from several weeks of working days
# limit		the proportion of data that is taken into use, e.g. limit 0.9 indicates that 
#			5% of the earliest and 5% of the latest observations are discarded
# days		string indicating the validity time of the schedules, either "Weekdays", "Saturdays" or "Sundays"
# output:
# no output, results are written in files
WriteSchedules = function(stops,TT,limit,days){
  a=sapply(stops$stop_code,StopSchedule,TT,limit,days,stops,origins,destinations)
}

# the function that actually computes the bus stop schedule
# input:
# stop_code		the bus stop code
# TT			data frame containing link travel times from several weeks of working days	
# limit			the proportion of data that is taken into use
# days			string indicating the validity time of the schedules, either "Weekdays", "Saturdays" or "Sundays"
# output:
# no output, the schedule is written in a file			
StopSchedule = function(stop_code,TT,limit,days){
  
  S=TT[which(TT$stopcode==stop_code),]
  stopc=stop_code
  
  # if this is a terminus stop, remove lines with arriving buses
  i=which(S$destination==stopc)
  if(length(i)>0){S=S[-i,]}
  if(nrow(S)==0){return(NULL)}
  # separate by departures
  L=split(S,list(S$line,S$direction,S$dvjr,S$origin),drop=TRUE)
  if(length(L)==0){print(paste0("return",stop_code));return()}
  reducedS = rbindlist(lapply(L,RemoveSpurious,2))
  linenum=as.numeric(gsub("[^[:digit:]]","",as.character(reducedS$line)))
  L=split(reducedS,list(linenum,reducedS$direction,reducedS$dvjr,reducedS$origin),drop=TRUE)
  if(length(L)==0){print(paste0("return",stop_code));return(NULL)}
  # convert the timestamps to times of day
  Ltod = lapply(L,LeavingTimeOfDay)
  limits = rbindlist(lapply(Ltod,Limits,limit))
  
  line=stack(lapply(L,function(x){as.character(x$line[1])})); #line=as.numeric(gsub("[^[:digit:]]","",line$values))
  line=line$values
  dvjr=stack(lapply(L,function(x){x$dvjr[1]})); originAimedDepartureTime=dvjr$values

  earliest = ConvertTimeMin(limits$low)
  timebetween = round(limits$high-limits$low)

  origin=stack(lapply(L,function(x){x$origin[1]}))                                                           
  origin=origin$values

  destination=stack(lapply(L,function(x){x$destination[1]})); 
  destination=destination$values
  
  SS=data.frame(line,originAimedDepartureTime,origin,destination,earliest,timebetween) 
  ind=which(SS$earliest=="-1:00")
  if(length(ind)>0){print("NA"); SS=SS[-ind,]}
  SS=SS[order(SS$earliest),]

  write.csv(SS,paste0("C:/Users/ps413734/Journeys/StopSchedules",days, "/StopSchedule", days ,"_",stop_code,".csv"),row.names=FALSE) 

}

# returns the departure as time of day value
LeavingTimeOfDay = function(A){
  return(A$leaving_tod)
}

# this function computes the earliest and latest departure time given the limit
# input:
# x				the departure times
# percentage 	the same as "limit" above
# output:
# low			the earliest departure
# high			the latest departure
Limits = function(x,percentage){
  p=percentage/100
  s=sort(x)
  n=length(x)
  if(n==1){
    low=s[1]
    high=s[1]
  }else{
    low=s[ceiling(n*(1-p)/2)]
    high=s[ceiling(n*(1+p)/2)]
  }
  return(data.frame(low,high))
}

# this function converts the time of day to the string form "HH:MM:SS"
ConvertTime = function(m){
  return(paste0(sprintf("%02d",floor(m/3600)),":",sprintf("%02d",floor((m%%3600)/60)),":",sprintf("%02d",round(m%%60))))
}

# this function converts the time of day to the string form "HH:MM"
ConvertTimeMin = function(m){
  ind=which(is.na(m))
  if(length(ind)>0){m[ind]=-3600} # returns "-1:00", removed later
  return(paste0(sprintf("%02d",floor(m/3600)),":",sprintf("%02d",floor((m%%3600)/60))))
}
