# In this file, there is the R code used for collecting the Tampere bus movement data published
# in http://data.itsfactory.fi/siriaccess/vm/json . The function polls the site, reads the JSON
# and writes the data into csv-files in the local disc.


# the main function
# if something goes wrong, it prints the time of the problems on the screen and tries again
SIRI <- function(){
  
  for(i in 1:10000){
    try(SiriLoggingProcess())
    print(i)
    print(Sys.time())
  }
  
}

# The functin that polls the SIRI data site and writes the current data into a file
SiriLoggingProcess = function(){
  
  while(TRUE){
    newdata=ReadSiriData()
    if(is.null(newdata)){
      Sys.sleep(1)
      next
    }
    foldername=paste("C:/data/siri_",format(Sys.time(),"%Y-%m-%d"),sep="");
    
    if(!file.exists(foldername)){
      dir.create(foldername);
    }
    
    filename=paste(foldername,"/siri_",format(Sys.time(),"%Y-%m-%d_%H"),".csv",sep="")    
    write.table(newdata,file=filename,append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
    Sys.sleep(0.3)
  } 
}

# This function reads the JSON data from the data site and converts it to R data frame
ReadSiriData <- function(){

	siridata=fromJSON(paste(readLines("http://data.itsfactory.fi/siriaccess/vm/json",warn=FALSE), collapse=""))

	bearingdata=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
					   function(x){x$MonitoredVehicleJourney$Bearing})

	latdata=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
				   function(x){x$MonitoredVehicleJourney$VehicleLocation$Latitude})
	londata=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
				   function(x){x$MonitoredVehicleJourney$VehicleLocation$Longitude})
	journeyrefs=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
					   function(x){x$MonitoredVehicleJourney$FramedVehicleJourneyRef$DatedVehicleJourneyRef})
	linerefs=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
					function(x){x$MonitoredVehicleJourney$LineRef$value})
	directionrefs=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
						 function(x){x$MonitoredVehicleJourney$DirectionRef$value})
	timestamps=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity,
					  function(x){x$RecordedAtTime})
	delays=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
				  function(x){x$MonitoredVehicleJourney$Delay})
	origins=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
				   function(x){x$MonitoredVehicleJourney$OriginName$value})
	destinations=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
						function(x){x$MonitoredVehicleJourney$DestinationName$value})
	vehicles=sapply(siridata$Siri$ServiceDelivery$VehicleMonitoringDelivery[[1]]$VehicleActivity, 
					function(x){x$MonitoredVehicleJourney$VehicleRef$value})

	# check if the siridata was empty (no busses driving currently (night))
	if( length(bearingdata) < 1 ||
		  length(latdata) < 1 ||
		  length(londata) < 1 ||
		  length(journeyrefs) < 1 || 
		  length(linerefs) < 1 ||
		  length(directionrefs) < 1 ||
		  length(timestamps) < 1 ||
		  length(delays) < 1){
	  return(NULL);
	}

	delays_sec=ParseDelays(delays);

	newsiri.data <- data.frame(linerefs,timestamps,directionrefs,journeyrefs,latdata,londata,
							   bearingdata,delays_sec,origins,destinations,vehicles);



	return(newsiri.data);

}

# This function converts the delay expressed as string to numeric seconds
ParseDelays <- function( delays_pattern ){
  
  # delays are of the form "P0Y0M0DT0H6M1.000S"
  # it is assumed that Y, M and D are always 0
  
  n = length(delays_pattern);
  delays_sec=rep(0,n);
  
  for( i in 1:n ){
    patt=delays_pattern[i];   
    if(is.character(patt)){    
      if(length(j<-grep("-",patt))==1){
        sign=(-1.0);
      }else{ sign=1.0; }
      
      sp=strsplit(patt,"[T,H,M,S]");
      h=as.numeric(sp[[1]][3]); # hours
      m=as.numeric(sp[[1]][4]); # minutes
      s=as.numeric(sp[[1]][5]); # seconds
      
      delays_sec[i]=sign*(h*3600+m*60+s);
    }else{
      delays_sec[i]=NA;
    }
  } 
  return(delays_sec);
  
}