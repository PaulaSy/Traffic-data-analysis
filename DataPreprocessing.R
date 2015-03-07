
# the main function for processing the raw bus movement data of one day
# input: 
# date			date string in the format "yyyy-mm-dd"
# output
# no output, the resulting preprocessed data is written in a file for later use
ProcessOneDaysData = function(date){
	CombineFiles(date)
	j=Preprocess(date)
}

# The data is collected in hourly (or smaller) csv-files.
# This function combines the small files into one file that comprises the
# bus movement data of one day.
# input:
# date			date string in the format "yyyy-mm-dd"
# output:
# no output, the combined data is written in a file
CombineFiles <- function(date){

  path=paste0("C:/data/siri_",date,"/")  
  filenames=list.files(path)
  
  for( i in 1:length(filenames)){    
    fname=filenames[i];
    siridata=read.csv(paste(path,fname,sep=""))    
    # combine the data as one big data frame
    if(i==1){
      alldata=siridata;
    }else{
      alldata=rbind(alldata,siridata);
    }   
  }
  write.csv(alldata,paste(path,"alldata.csv",sep=""))
}


# This function reorganizes the daily data into journeys and attempts to clean
# the inconsistent observations from the data. Note that the function is in its
# original form, and many operations could be done more efficiently.
# input:
# date			date string in the format "yyyy-mm-dd"
# output:
# all_journeys	one day's cleaned and organized journeys combined in one data frame
Preprocess <- function(date){

	filename = paste0("C:/SIRIdata/siri_",date,"/alldata.csv")
	SD=read.csv(filename,header=TRUE);

	# find all the different journey tags in the file
	journeys=unique(data.frame(SD$linerefs,SD$directionrefs,SD$journeyrefs));
	all_journeys=list();
	wJ={}
	num=0;
	# collect each journey in its own data frame
	for( j in 1:nrow(journeys)){
	  iL=which(as.character(SD$linerefs)==as.character(journeys$SD.linerefs[j]) );
	  iD=which(SD[iL,]$directionrefs==journeys$SD.directionrefs[j]);
	  iJ=which(SD[iL[iD],]$journeyrefs==journeys$SD.journeyrefs[j]);	  
	  vehdata=SD[iL[iD[iJ]],];
	  
	  # identify garbage along the journey
	  # 1: try this: cut off all the data with duplicated time stamps
	  vehdata=vehdata[!duplicated(vehdata$timestamps),];

	  # in CleanData function, outliers and duplicates are discarded and 
	  # speed estimates are added to the data
	  vehdata=CleanData(vehdata);
	  if(is.null(vehdata)){
		next;
	  }
	  num=num+1;
	  all_journeys[[num]]=vehdata;
	  
	  if(is.null(wJ)){
		wJ=vehdata
	  }else {
		wJ=rbind(wJ,vehdata)
	  }
	}
	write.csv(wJ,paste("C:/SIRIdata/CleanerJourneys/journeys_",date,".csv",sep="")) 
	return(all_journeys);
}

# This function contains a number of tricks to get rid of the garbage data.
# Examples: Data from much earlier or later than the journey's expected starting and
# ending times are discarded. These are usually caused by the transmitter being on when 
# the bus in actually not on its route. Also, observations related to excessive 
# speeds are discarded, these are due to noisy observations. 
# Buses on transfer drives are tried to be identified and removed from the data.
# input:
# journey		data frame containing one journey's data
# output:
# journey		the same journey's data in a data frame, with garbage removed and some additional columns such as speed estimate
CleanData = function(journey){
  
  # if any of the time of day -values are more than 2 hours later or  than the 
  # scheduled departure, consider this as a "ghost drive" and discard the data
  sched_t=journey[1,4]
  sched_h=floor(sched_t/100)
  sched_m=sched_t%%100
  sched_departure_tod=sched_h*3600+sched_m*60
  st_diff=journey[,9]-sched_departure_tod
  stind1=which(st_diff<(-120) & st_diff>(-22*3600)) # before leaving
  #stind3=which(st_diff[stind1] > (-22*3600))
  #stind1=stind1[-stind3]
  stind2=which(st_diff>(2*3600))  # more than two hours after departure
  if(length(stind1)>0 | length(stind2)>0){journey=journey[-c(stind1,stind2),]}
  
  if( is.null(nrow(journey))){return(NULL)} 
  
  # remove rows with duplicated time stamps
  journey=journey[!duplicated(journey[,2]),]
  
  if( is.null(nrow(journey))){return(NULL)} 
  if( nrow(journey) < 10 ){return(NULL)}
  
  Nekala_garage=c(61.47854*pi/180,23.79672*pi/180)
  
  R1 = 6370000; # earth radius, meters
  R2 = R1*cos(61.5*pi/180);  # R1*cos(latitude)
  
  n=nrow(journey);
  
  if(n<2){
    return(NULL);
  }
  
  # any points coming from Nekala garage? -> discarded
  dNekala=SimpleDistance(journey[,5]*(pi/180),journey[,6]*(pi/180),Nekala_garage[1],Nekala_garage[2])
  iNekala=which(dNekala<50)
  if(length(iNekala)>0){ journey=journey[-iNekala,]}

  if(is.null(nrow(journey))){return(NULL)}
  
  # compute distances and times between successive points
  nj=nrow(journey)
  if(nj<10){ return(NULL)}
  distances=SimpleDistance(journey[2:nj,5]*(pi/180),journey[2:nj,6]*(pi/180),
                           journey[1:(nj-1),5]*(pi/180),journey[1:(nj-1),6]*(pi/180))
  distances=c(0,distances)
  
  td=diff(journey[,2])/1000
  timediff=c(1,td)
  speeds_kmh=(distances/timediff)*3.6
  
  # add new columns to journey
  journey=cbind(journey,as.matrix(speeds_kmh)) #,as.matrix(timediff),as.matrix(distances))
  
  # remove any measurements coming from past
  tind=which(timediff<0)    # timediff < 0
  if(length(tind)>0){journey=journey[-tind,]}
  
  # heuristics: if there is a long break, the journey might have been ended
  # (with bad luck there was just a connection problem and we miss the data)
  tind=which(timediff>300)
  if(length(tind>0)){
    # more heuristics: discard only if the cutpoint is in the latter part of the journey
    tind=which(tind>(round(0.75*nrow(journey))))
    if(length(tind>0)){ journey=journey[-tind,]}
  }
  
  # DURING TRANSFER DRIVES, MEASUREMENTS WITH LARGE DELAYS (not always)
  # TEND TO EXIST AS BURSTS WITH ABOUT 30 SECONDS TIMEDIFF
  # TRY TO REMOVE THIS KIND OF BURSTS!
  indburst=which(timediff>=25)  # timediff>=25
  
  # now look at all the measurements after the first burst occurred
  if(length(indburst)>2){
    s=sort(timediff[indburst[1]:length(timediff)])
    # if at least 25% of the time differences are > 25 seconds, discard the data
    # onwards from the first burst
    s75=s[floor(0.75*length(s))]
    #print(s75)
    if(s75>=25){
      journey=journey[-(indburst[1]:nrow(journey)),]
    }
  }
  
  if(is.null(nrow(journey))){
    return(NULL)
  }
  
  # no movement (sum of distances is small)
  if( (sum(distances,na.rm=TRUE))<1000){
    return(NULL)
  }
  
  # remove the remaining non-moving data
  u=which(distances>1.0)
  last_moving=u[length(u)]
  if(last_moving<(nrow(journey)-1)){
    journey=journey[-((last_moving+1):nrow(journey)),]
  }
  
  # Remove speed outliers, 
  # use here a stricter value for the "normal" window,
  # 1.25 instead of the default 1.5
  oi=RemoveOutliers(journey[,11],1.25)
  
  if(length(oi)>0){
    journey= journey[-(oi),]
  }
  
  if(is.null(nrow(journey))){
    return(NULL)
  }
  
  # in some consistent two-track-cases, there may exist two tracks that
  # are far apart from each other, and the speeds are consistently too high
  # but not detected by outlier detection
  high_speed_ind=which(journey[,11]>120)
  #print(paste("high-speed",high_speed_ind))
  if(length(high_speed_ind)>0){
    journey=journey[-high_speed_ind,]
  }
  
  if(is.null(nrow(journey))){
    return(NULL)
  }
  
  # long continuous lines with non-changing delays in the beginning end 
  # of journey are suspicious
  # => remove
  diffdelays=diff(journey[,8])
  # find the last nonzero delay difference
  nonzerodiff=which(diffdelays != 0)
  if( length(nonzerodiff)>0){
    lastnonzero=nonzerodiff[length(nonzerodiff)]+1
    firstnonzero=nonzerodiff[1]
    
    if(lastnonzero<nrow(journey)){
      journey=journey[-((lastnonzero+1):nrow(journey)),]
    }
    if(firstnonzero>1){
      journey=journey[-(1:(firstnonzero-1)),]
    }  
  }else{
    #all delays are the same ??
    return(NULL)   
  }
  jump=which(abs(diffdelays)>100)
  if(length(jump)>0){
    journey=journey[-(jump[1]:nrow(journey)),]
  }  
  # remove rows that contain outlier delays
  oi=RemoveOutliers(journey[,8])
  if(length(oi)>0){
    journey=journey[-oi,]
  }
  return(journey);  
}


# this function removes outliers outside of the +/- 1.5 IQR-region from
# lower and upper quartiles
# input:
# datavector      a vector of observations
# IQR_coeff       the coefficient the defines the range outside which the outliers are discarded
#                 by default 1.5
# output:
# outlier_indices indices of the observations that are considered outliers, NULL if none
RemoveOutliers <- function(datavector,IQR_coeff=1.5){
  
  sorted=sort(datavector);
  n=length(sorted); 
  median=sorted[round(0.5*n)];
  lower_quartile=sorted[round(0.25*n)];
  upper_quartile=sorted[round(0.75*n)];
  IQR=upper_quartile-lower_quartile;
  outlier_indices=c(which(datavector<(lower_quartile-IQR_coeff*IQR)),
                    which(datavector>(upper_quartile+IQR_coeff*IQR)))
   
  if(length(outlier_indices)==0){
    return(NULL)
  }else{
    return(outlier_indices)
  }  
}

# ********************************************************************************************************
# function for computing distance of geodetic coordinates in the simplest form
# latA and lonA are vectors, latB and lonB are scalars or vectors, [radians]
SimpleDistance <- function(latA,lonA,latB,lonB){

  R1 = 6370000; # earth radius, meters
  R2 = R1*cos(latA[1]);  # R1*cos(latitude)
  
  dlat=R1*(latA-latB)
  dlon=R2*(lonA-lonB)
  d=sqrt(dlat*dlat+dlon*dlon)

  return(d)
}
