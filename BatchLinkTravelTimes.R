

# the main function that reads the raw, preprocessed data and works it into travel time segments,
# also called links
# input:
# date			date string in the format "yyyy-mm-dd"
# folder		path of the folder where the results are written
# output:
# no output, the results are written to a file
SegmentJourneys = function(date,folder="C:/SIRIdata/BusDataSegments/"){

  D=fread(paste0("C:/SIRIdata/BusData/bus_",date,".csv"),header=TRUE,sep=",",stringsAsFactors=FALSE,
          select=c(1:11),colClasses=list(double=2))
  
  # read locations 
  locations = read.table("C:/trafficdata/traffic_locations.csv",as.is=TRUE,encoding="UTF-8",sep=",",header=TRUE); 
  
  # split the data frames into journeys
  J=split(D,list(D$line,D$direction,D$dvjr,D$origin,D$destination,D$vehicle),drop=TRUE)
  
  # apply journey segmentation to all the journeys
  S=lapply(J,SegmentJourneyLevel1,locations) 
  SegmentData=rbindlist(S)
  write.csv(SegmentData,paste0(folder,"segments_",date,".csv"),row.names=FALSE) 

}

# an intermediate level function that correctly splits journeys that appear around midnight
# input:
# J			one bus journey as a data frame
# locations origin and destination codings in a local database
# output:
# rbindlist(S)	the segmented journey(s) stacked in one data frame
SegmentJourneyLevel1 = function(J,locations){
	  
	  if(J$vehicle[1]==""){return(NULL)}
	  
	  dt=diff(J$timestamp)  #/1000 if the timestamp is in milliseconds (depends on data source)
	  # for journeys that appear with same id-fields in the beginning and end of day, but are 
	  # actually different journeys, split the journeys into two parts
	  ind=which(abs(dt)>70000)
	  if(length(ind>0)){
		K=list(J[1:ind[1],],J[(ind[1]+1):nrow(J),])
	  }else{
		K=list(J)
	  }
		S=lapply(K,SegmentJourneyWithTripID)
	  return(rbindlist(S))
}

# This function performs the offline segmentation of journey J, into links between bus stops
# Various tricks are used to get rid of any inconsistent data
# input:
# J			one bus journey as a data frame
# output:
# segments	the journey J reduced to arrival and leaving times to and from bus stops
SegmentJourneyWithTripID = function(J){ 
  
  MAXDIST=30
  trip_id=FindTripID(J)
  stopseq=stop_times[as.character(stop_times$trip_id)==as.character(trip_id),]

  if(nrow(stopseq)<1){#print("no stops");
    return(NULL)}
  s=lapply(stopseq$stop_id,function(x){return(stops[stops$stop_id==x,])})
  stoppos=rbindlist(s) 
  
  # TRICK:
  # if there is a long gap in the data, remove the rest
  GAPTIMELIMIT = 300
  td=diff(J$timestamp)
  ind = which(td>GAPTIMELIMIT)
  if(length(ind)>0){
    # remove rows after too long time gap
    J=J[-(ind[1]:nrow(J)),]
  }
  
  # **
  deg2rad=pi/180
  stop_lat=stoppos$stop_lat*deg2rad
  stop_lon=stoppos$stop_lon*deg2rad
  J$lat=J$lat*deg2rad
  J$lon=J$lon*deg2rad
  # **
  
  # find points according to the first and last stop
  endpoints = FindEndPoints(J,stop_lat,stop_lon)
  # cut off parts of journey not between the terminus stops
  J=J[endpoints[1]:endpoints[2],]
  if(nrow(J)<5 | is.na(J$line[1])){return(NULL)}
  
  # sanity checking: remove suspicious parts of the journey,
  # this is often observed from the weird delays
  if(max(J$delay)<(-300) | median(J$delay)<(-600)){
    return(NULL)
  }
  if(min(J$delay)>3000){return(NULL)}

  # ************** heuristics *****************
  # TRICK:
  # DURING TRANSFER DRIVES, MEASUREMENTS WITH LARGE DELAYS (not always)
  # TEND TO EXIST AS BURSTS WITH ABOUT 30 SECONDS TIMEDIFF
  # TRY TO REMOVE THIS KIND OF BURSTS!
  indburst=which(diff(J$timestamp)>=25)  # timediff>=25
  
  # now look at all the measurements after the first burst occurred
  if(length(indburst)>2){
    s=sort(diff(J$timestamp[indburst[1]:nrow(J)]))
    # if at least 25% of the time differences are > 25 seconds, discard the data
    # onwards from the first burst
    s75=s[floor(0.75*length(s))]
    #print(s75)
    if(s75>=25){
      J=J[-(indburst[1]:nrow(J)),]
    }
  }
  
  busstops=cbind(stoppos$stop_id,stop_lat,stop_lon)

  times=matrix(data=0,nrow=nrow(busstops),ncol=5)
  time_leaving_previous_stop=0
  for(i in 1:nrow(busstops)){
    
    times[i,]=StopArrivalAndLeaveSequential(busstops[i,],J,time_leaving_previous_stop)
    if(times[i,1]!=0){
      time_leaving_previous_stop = times[i,3]
    }
  }

  rem=which(times[,1]==0)
  # remove bus stops not along the journey
  if(length(rem)>0){times=times[-rem,]}
  if(nrow(matrix(times,ncol=5))<3){return(NULL)}
  # order bus stops according to ascending arrival times
  #times=times[order(times[,2]),]
  n=nrow(times)
  line=rep(J$line[1],n)
  direction=rep(J$direction[1],n)
  dvjr=rep(J$dvjr[1],n)
  trip_id=rep(trip_id,n)
  origin=rep(J$origin[1],n)
  destination=rep(J$destination[1],n)
  vehicle=rep(J$vehicle[1],n)
  stopcode=times[,1]
  arrival_time=times[,2]
  leaving_time=times[,3]
  delay_at_arrival=times[,4]
  delay_at_leaving=times[,5]
  Segments=data.frame(line,direction,dvjr,trip_id,origin,destination,vehicle,stopcode,
                      arrival_time,leaving_time,delay_at_arrival,delay_at_leaving) 
  return(Segments)
  
}

# From journey J and known known journey bus stop locations,
# identify the actual starting and ending points of J
# input:
# J			one bus journey as a data frame
# stoplat	bus stop latitudes (radians) along J, in the correct order
# stoplon	bus stop longitudes (radians) along J, in the correct order
# output:
# first_ind		the row number in J that corresponds to the first stop
# last_ind		the row number in J that corresponds to the last stop
FindEndPoints = function(J,stoplat,stoplon){
  
  # find first point close to the first stop
  d=SimpleDistance(J$lat,J$lon,stoplat[1],stoplon[1])
  nearfirst = which(d<100)
  if(length(nearfirst)>0){
    first_ind=nearfirst[1]
  }else{first_ind=1}
  
  n=length(stoplat)
  d=SimpleDistance(J$lat,J$lon,stoplat[n],stoplon[n])
  nearlast=which(d<100)
  if(length(nearlast)>0){
    last_ind = nearlast[length(nearlast)]
  }else{last_ind=nrow(J)}
  
  return(c(first_ind,last_ind))
  
}

# Using the journey data, find the corresponding GTFS trip id
# J is the journey data frame with fields
# "line"        "timestamp"   "direction" "dvjr" "lat" "lon" "bearing" "delay" "origin" "destination" "vehicle"
# (of these, only timestamp, line and dvjr are actually needed, in addition to origin)
FindTripID = function(J){

# read the GTFS data  
 trips=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/trips.txt"); names(trips)[1]="route_id"
 calendar=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/calendar.txt"); names(calendar)[1]="service_id"
 calendar_dates=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/calendar_dates.txt"); names(calendar_dates)[1]="service_id"
 routes=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/routes.txt"); names(routes)[1]="route_id"
 shapes=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/shapes.txt"); names(shapes)[1]="shape_id"
 stop_times=read.csv("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/stop_times.txt"); names(stop_times)[1]="trip_id"
 stops=read.table("C:/SIRIdata/GTFS_fall_2014/tamperefeed_01082014/stops.txt",as.is=TRUE,encoding="UTF-8",sep=",",header=TRUE); names(stops)[1]="stop_id"
locations = read.csv("C:/trafficdata/traffic_locations.csv")
  
  # find the stop id of the first stop
 # first_stop = stops$stop_code[as.character(stops$stop_name)==as.character(origin)]
  first_stop=J$origin
 # if(as.character(origin)=="Vatiala"){first_stop=4600}
  # find all journeys with the same first stop
  all = rbindlist(lapply(first_stop,function(x){stop_times[(stop_times$stop_id==x & stop_times$stop_sequence==1),]}))
  # convert the departure times to the same format as in J
  dvjr = sapply(all$departure_time,function(x){a=strsplit(as.character(x),":"); return(as.numeric(a[[1]][1])*100 + as.numeric(a[[1]][2]))})
  # find the matching journeys
  possible_ones = all[(dvjr==as.numeric(J$dvjr[1])),]
  # check which of the possible ones is valid with this date
  valid=sapply(possible_ones$trip_id,CheckTime,J,trips,calendar,calendar_dates)
  
  if(any(valid)){            # at least one of the possible trip id:s is valid
    chosen=possible_ones[valid,]
    if(nrow(chosen)>1){         # for some reason, several valid choices??
     # print(chosen)
      chosen=chosen[1,]         # just guessing, take the first option 
    }
    return(as.character(chosen$trip_id))
  } else {
  #  print(J[1,])        # why didn't we find a matching trip_id?
    return(NULL)
  }
  
}




