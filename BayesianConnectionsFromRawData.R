# Given raw data, the function evaluates the number of successful connections between given bus departures
# input:
# line1		line ID of the first bus
# dir1      direction of the first bus (1 or 2)
# dep1      dvjr of the first bus
# stop1     the connection bus stop of the first bus
# M1        journey data frame containing data of the first bus journeys
# line2		line ID of the second bus
# dir2      direction of the second bus (1 or 2)
# dep2      dvjr of the second bus
# stop2     the connection bus stop of the second bus
# M2        journey data frame containing data of the second bus journeys 
# STOPS		GTFS stops.txt content as a data frame
# output:
# S			binary vector idicating the successes and failures
ConnectionSuccess = function(line1,dir1,dep1,stop1,M1,line2,dir2,dep2,stop2,M2,STOPS){
  add1=ArrivedAt(line1,dir1,dep1,STOPS,stop1,M1)
  add2=ArrivedAt(line2,dir2,dep2,STOPS,stop2,M2)
  
  # the minimum required time between the arrival of bus 1 and departure of bus 2
  DT=60
  S=rep(0,nrow(add1))
  num=0
  
  for(i in 1:nrow(add1)){
    day=add1[i,3]
    t1=add1[i,1]
    ind=which(add2[,3]==day)
    if(length(ind)==1){
      t2=add2[ind,1]
      num=num+1
      if((t1+DT)<t2){
       # s=s+1  # successfull connection
        S[num]=1
      }else{
      #  n=n+1  # connection failed
      }

    }
  }

  S=S[-((num+1):length(S))] 
  return(S)
  
}

# given raw data, this function finds the arrival times of certain
# bus departures at a given bus stop
# input:
# line		line ID
# direction	direction, either 1 or 2
# departure	dvjr
# STOPS		GTFS stops.txt content as a data frame
# stop_code	the stopcode of the bus stop of interest
# D			data frame of bus journey data
# output:
# a dataframe containing information about the arrival times
ArrivedAt = function(line,direction,departure,STOPS,stop_code,D){
  
  deg2rad=pi/180
  
  ind=which(STOPS$stop_code==stop_code)
  stoplat=STOPS$stop_lat[ind]*deg2rad
  stoplon=STOPS$stop_lon[ind]*deg2rad
  if(departure==1){
    M=D[which(D[,1]==line & D[,3]==direction),]
  }else{
    M=D[which(D[,1]==line & D[,3]==direction & D[,4]==departure),]
  }
  
  
  Mlat=M[,5]*deg2rad
  Mlon=M[,6]*deg2rad
  
  # pick observations made at the stop (or near it)
  d=SimpleDistance(Mlat,Mlon,stoplat,stoplon)
  atstop=which(d<15)
  
  if(length(atstop)<1){return(NULL)}
  
  A=M[atstop,]

  # pick instances when the bus arrived at the stop
  dt=diff(A[,2]/1000)
  #i=which(dt>80000)  # next day
  i=which(dt>120)
  t=c(1,(i+1))
  
  if(t[length(t)]>nrow(A)){
    t=t[-length(t)]
  }
  
  arrival_times=A[t,9]
  arrival_delays=A[t,8]
  arrival_days=floor(A[t,2]/(86400*1000))
  arrival_departure=A[t,4]
  
  return(data.frame(arrival_times,arrival_delays,arrival_days,arrival_departure))
  
}

# this function returns the delay values at a certain bus stop
# input:
# line		line ID
# direction	1 or 2
# STOPS 	GTFS stops.txt as a data frame
# stop_code	the stopcode of the stop of interest
# D			the raw data frame
# output:
# tds		data frame conataining information of the delays at this stop
DelayAt = function(line,direction,STOPS,stop_code,D){
  
  departures=unique(D[,4])
  tds=matrix(data=0,nrow=length(departures),ncol=4)
  
  for(i in 1:length(departures)){
    departure=departures[i]
    atd=ArrivedAt(line,direction,departure,STOPS,stop_code,D)
    
    if(is.null(atd)){next}
    if(nrow(atd)<3){next}
    
    tds[i,1]=mean(atd[,1]) # mean time of arrival
    tds[i,2]=mean(atd[,2]) # mean delay at arrival time
    tds[i,3]=sd(atd[,2])   # standard deviation of delay at arrival time (equal to std of arrival time)
    tds[i,4]=departure
  }
  ind=which(tds[,4]==0)
  tds=tds[-ind,]
  
  return(tds)
}

# returns the delays at each bus stop along one line's route
# input:
# line		line ID
# direction	1 or 2
# STOPS 	GTFS stops.txt as a data frame
# D 		the raw data frame
# output:
# dar		delays along route
DelaysAlongRoute = function(line,direction,STOPS,D){
  
  line_stops=read.csv(paste("C:/SIRIdata/stops_",line,"_d",direction,".csv",sep=""))
  dar=matrix(data=0,nrow=nrow(line_stops),ncol=3)
  
  for(i in 1:nrow(line_stops)){
    stop_code=line_stops$stop_code[i]
    ad=DelayAtStop(line,direction,STOPS,stop_code,D)
    if(is.null(ad)){next}
    if(length(ad)<3){next}
    dar[i,1]=stop_code
    dar[i,2]=mean(ad)
    dar[i,3]=sd(ad)
  }
  return(dar)
}

# this function returns the delay of a given journey at a given bus stop
DelayAtStop = function(line,direction,STOPS,stop_code,D,departure=1,returntime=0){
  
  deg2rad=pi/180
  
  ind=which(STOPS$stop_code==stop_code)
  stoplat=STOPS$stop_lat[ind]*deg2rad
  stoplon=STOPS$stop_lon[ind]*deg2radpl
  
  if(length(departure)==1){
    if(departure==1){
      # choose all departures in direction
      M=D[which(D[,1]==line & D[,3]==direction),]
    }else{
      M=D[which(D[,1]==line & D[,3]==direction & D[,4]==departure),]
    }
  }else{
    if( length(departure)==2){
      M=D[which(D[,1]==line & D[,3]==direction & D[,4]>=departure[1] & D[,4]<=departure[2] ),]
    }
  }
  
  Mlat=M[,5]*deg2rad
  Mlon=M[,6]*deg2rad
  
  # pick observations made at the stop (or near it)
  d=SimpleDistance(Mlat,Mlon,stoplat,stoplon)
  atstop=which(d<15)
  
  if(length(atstop)<1){return(NULL)}
  
  A=M[atstop,]
  
  # pick instances when the bus arrived at the stop
  dt=diff(A[,2]/1000)
  i=which(dt>(2*60)) 
  t=c(1,(i+1))
  
  if(t[length(t)]>nrow(A)){
    t=t[-length(t)]
  }

  if(returntime==0){
    arrival_delays=A[t,8]
  }else{
    arrival_delays=A[t,c(4,8,9)]
  }
  
  return(arrival_delays)
  
}

# This function evaluates the BINOMIAL posterior connection success of a given connection
# input:
# line1		line ID of the first bus
# dir1      direction of the first bus (1 or 2)
# dep1      dvjr of the first bus
# stop1     the connection bus stop of the first bus
# M1        journey data frame containing data of the first bus journeys
# line2		line ID of the second bus
# dir2      direction of the second bus (1 or 2)
# dep2      dvjr of the second bus
# stop2     the connection bus stop of the second bus
# M2        journey data frame containing data of the second bus journeys 
# STOPS		GTFS stops.txt content as a data frame
# alfa		binomial alfa parameter
# beta 		binomial beta parameter
# output:
# theta		the posterior estimate
PosteriorSuccess = function(line1,dir1,dep1,stop1,M1,line2,dir2,dep2,stop2,M2,STOPS,alfa,beta){
  
  CS=ConnectionSuccess(line1,dir1,dep1,stop1,M1,line2,dir2,dep2,stop2,M2,STOPS)
  
  theta=rep(0,(length(CS)+1))
  theta[1]=alfa/(alfa+beta)
  
  for(i in 1:length(CS)){
    n=i
    s=sum(CS[1:i])
    theta[i+1]=BinomialPosteriorMean(alfa,beta,s,n)
  }
  plot(theta,xlab="Number of trials n",ylab="Posterior mean of connection success")
  return(theta)
}

# Evaluates the binomial posterior mean
# input:
# alfa		binomial alfa
# beta 		binomial beta
# s			number of successes
# n			total number of trials
# output:
# b			the posterior mean
BinomialPosteriorMean = function(alfa,beta,s,n){
  return((alfa+s)/(alfa+beta+n))
}

# These functions estimate alfa and beta from a sample mean and variance
alfa = function(m,w){
  a=m*(m-(m^2)-w)/w
  return(a)
}
beta = function(m,w){
  b=(m-(m^2)-w)*(1-m)/w
  return(b)
}

# this function evaluates the NORMAL posterior connection success
# input:
# m1		posterior mean of first bus delay
# w1		posterior variance of first bus delay
# m2 		posterior mean of second bus delay
# w2 		posterior variance of second bus delay
# ds	    the scheduled time difference of the bus arrivals
# DT		the time needed for the physical bus change
# output:
# p			connection probability
ProbabilityOfConnectionSuccessNormal = function(m1,w1,m2,w2,ds,DT){
  # how much do the expected delays of both lines change the connection time
  # this is also the mean of the distribution of d2-d1
  dc=m2-m1
  # the variation of d2-d1
  wc=w1+w2
  # probability of d2-d1 > DT-ds
  p=pnorm(DT-ds,mean=dc,sd=sqrt(wc),lower.tail=FALSE)
  return(p)
}

# This function returns the connection success probabilities versus time allocated for the connection
# input:
# line1		line ID of the first bus
# d1      	direction of the first bus (1 or 2)
# stop1     the connection bus stop of the first bus
# M1        journey data frame containing data of the first bus journeys
# line2		line ID of the second bus
# d2      	direction of the second bus (1 or 2)
# stop2     the connection bus stop of the second bus
# M2        journey data frame containing data of the second bus journeys 
# STOPS		GTFS stops.txt content as a data frame
# DT		the time needed for the physical bus change 
# output:
# p			the connection probabilities in a vector
ProbabilityVsConnectionTime = function(line1,d1,stop1,M1,line2,d2,stop2,M2,STOPS,DT){
  v=100^2
  d1=DelayAtStop(line1,d1,STOPS,stop1,M1,1)
  d2=DelayAtStop(line2,d2,STOPS,stop2,M2,1)
  # choose the number of observations used for the posterior
  N=10
  #N=5
  N1=round(length(d1))-N
  N2=round(length(d2))-N
  mw1=NormalPosteriorBatch(mean(d1[1:N1]),sd(d1[1:N1])^2,v,d1[(length(d1)-N+1):length(d1)])
  print(mw1)
  mw2=NormalPosteriorBatch(mean(d2[1:N2]),sd(d2[1:N2])^2,v,d2[(length(d2)-N+1):length(d2)])
  print(mw2)
  
  minctime=-5
  maxctime=5
  times=((2*minctime):(2*maxctime))*0.5
  p=rep(0,length(times))
  for(i in 1:length(times)){
    p[i]=ProbabilityOfConnectionSuccessNormal(mw1[1],mw1[2],mw2[1],mw2[2],times[i]*60,DT)
  }
  plot(times,p)
  return(p)
}

# Gaussian posterior computed sequentially
# input:
# m			prior mean
# w			prior variance
# v			the (known, constant) variance of the observations
# y			observation
# output:
# mk		posterior mean
# wk		posterior variance of mk
NormalPosteriorSequential = function(m,w,v,y){
  wk=1/(1/w + 1/v)
  mk=wk*(m/w + y/v)
  return(c(mk,wk))
}

# Gaussian posterior computed in a batch
# input:
# m			prior mean
# w			prior variance
# v			the (known, constant) variance of the observations
# y			observations in a vector
# output:
# mn		posterior mean
# wn		posterior variance of mn
NormalPosteriorBatch = function(m,w,v,y){
  mn=(m/w+sum(y)/v)/(1/w+length(y)/v)
  wn=1/(1/w+length(y)/v)
  return(c(mn,wn))
}

# This function estimates the posterior delay given a set of raw data
# input:
# line		line ID
# dir		direction (1 or 2)
# departure	dvjr
# stop_code	stopcode
# STOPS		GTFS stops.txt as a data frame
# M			raw data frame
# m0		prior mean
# w0 		prior variance
# v			the (known, constant) variance of the observations
# output:
# mk		vector of posterior mean delays (after each observation)
# wk		vector of posterior variance of mk (after each observation)
PosteriorDelay = function(line,dir,departure,stop_code,STOPS,M,m0,w0,v){
  # departure=1 means that look at all departures
  d=DelayAtStop(line,dir,STOPS,stop_code,M,departure)
  
  mk=rep(0,(length(d)+1))
  mk[1]=m0 # prior
  wk=rep(0,(length(d)+1))
  wk[1]=w0
  
  for(i in 1:length(d)){
    mw=NormalPosteriorSequential(mk[i],wk[i],v,d[i])
    mk[i+1]=mw[1]
    wk[i+1]=mw[2]
  }
  return(cbind(mk,wk))
}

# The same function as PosteriorDelay, but with delays directly as input
# input:
# d			vector of observed delays
# m			prior mean
# w			prior variance
# v			the (known, constant) variance of the observations
# output:
# mk		posterior mean delay
# wk		posterior variance of mk
SequentialPosteriorDelay = function(d,m,w,v){
  mk=rep(0,(length(d)+1))
  wk=rep(0,(length(d)+1))
  mk[1]=m
  wk[1]=w
  for(i in 1:length(d)){
    mw=NormalPosteriorSequential(mk[i],wk[i],v,d[i])
    mk[i+1]=mw[1]
    wk[i+1]=mw[2]
  }
  return(cbind(mk,wk))
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

SimpleDistanceDeg <- function(deglatA,deglonA,deglatB,deglonB){
  latA=deglatA*pi/180
  lonA=deglonA*pi/180
  latB=deglatB*pi/180
  lonB=deglonB*pi/180
  return(SimpleDistance(latA,lonA,latB,lonB))
}