# This function groups the link data input by date, calls change points
# detection for each data group and combines the data again in one data structure.
# input:
# L			data from one link (several days' travel times, e.g. 2 months)
# upper_p	the value of upper percentage to be tabulated, e.g. 0.95
# LM		table of link medians
# output:
# SD		a travel time model table for this link
ChangePointSegmentingDaily = function(L,upper_p,LM){
  
  # Group the link data by date
  DL=split(L,list(L$date),drop=TRUE)
  SD=rbindlist(lapply(DL,ChangePointSegmenting,upper_p,LM))
  return(SD)
  
}


# This function is for finding the change point candidates from one link's
# one day data. Within the function, the changepoint candidates are also validated and
# pruned. 
# Input:
# L			travel time data from one link and one day
# upper_p	the value of upper percentage to be tabulated, e.g. 0.95
# LM		table of link medians
# Output:
# a data frame consisting of the travel time model table for this link and date
ChangePointSegmenting = function(L,upper_p,LM){
  
  L=L[order(L$arrival_tod),]
  tb=L$timebetween
  # Limit the monitoring between 5 AM and 10 PM
  morning=5*3600
  evening=22*3600
  # Recursively compute the change point candidates
  cp=ChangePoints(tb)
  # find the link median for this link
  m=LM$med[LM$stopcode==L$stopcode[1] & LM$prevstop==L$prevstop[1]]
  
  if(!is.null(cp)){
    cp=sort(cp)
  }
  # loop through the change point list, checking the statistical conditions
  # of the data samples on both sides of the candidate change point, pruning
  # as needed and validating until no modifications to the change point list are made
    while(TRUE){
      cp_with_endpoints=c(1,cp,length(tb))
     upper=rep(0,length(cp_with_endpoints)-1)
     # s=rep(0,length(cp_with_endpoints)-1)
      med=rep(0,length(cp_with_endpoints)-1)
      sample=list()
      for(i in 2:length(cp_with_endpoints)){
		# each list item sample[[i]] consists of the travel time observations 
		# between (i-1):th and i:th change points
        sample[[i-1]]=tb[cp_with_endpoints[i-1]:cp_with_endpoints[i]]
        sorted=sort(sample[[i-1]])
#         m[i-1]=mean(sample[[i-1]])
#         s[i-1]=sd(sample[[i-1]])
        med[i-1]=median(sample[[i-1]])
        upper[i-1]=sorted[round(length(sorted)*upper_p)]
      }
      removecp=NULL
      if(length(sample)==1){break}
      for(i in 2:length(sample)){
        if(!MannWhitneySampleDifferenceTest(sample[[i-1]],sample[[i]])){
          # samples are not considered significantly different
          removecp=c(removecp,i-1)
        }
      }
	  # if no modifications were made, leave the loop, the list of change points is finished
      if(is.null(removecp)){
        # no changepoints were removed
        break
      }
      cp=cp[-removecp]
      
    }
    
    changetimes=L$arrival_tod[cp] 
    n=length(changetimes)+1
    stopcode=rep(L$stopcode[1],n)
    prevstop=rep(L$prevstop[1],n)
    date=rep(L$date[1],n)
    starttime=c(morning,changetimes)
    endtime=c(changetimes,evening)
    level10=round(10*log10(med/m))
    level=round(10*log(med/m))
    
    return(data.frame(stopcode,prevstop,date,starttime,endtime,med,upper,level10,level))
  
}

# This function recursively finds changepoints from a data set x. 
# For both sides of x divided by the changepoints, the function calls itself
# to find the possible changepoints in both sides, until there are less than n
# observations in x. The validity of each change point is checked using BootstrapCusum.
# input:
# x 		a data set (travel times sorted by arrival time)
# output:
# list of proposed change points
ChangePoints = function(x){
  n=length(x)
 # if(n<6){return(NULL)}
 if(n<10){return(NULL)}
  
  changepoint = BootstrapCusum(x)
  cp1=NULL
  cp2=NULL
  if(!is.null(changepoint)){
    cp1=ChangePoints(x[1:(changepoint-1)])
    cp2=ChangePoints(x[changepoint:n])
    if(!is.null(cp2)){
      cp2=cp2+changepoint-1
    }
  }
  return(c(changepoint,cp1,cp2))
}

# This fucntion computes the "cusum", the cumulative sum minus the mean
# input:
# x			data vector
# output:
# cs		the value of cusum
Cusum = function(x){
  
  mx=mean(x)
  cs=cumsum(x-mx)
  return(cs)
  
}

# This function finds the maximum difference in cusum (which is the change point  
# candidate, and checks for the significance of the cusum value by comparing
# to bootstrapped cusum values (to see if data in random order produce larger
# cusum difference values than the ordered data).
# input:
# x			data vector
# output:
# the location index of the identified change point candidate
BootstrapCusum = function(x){
  
  CONFLEVEL_THRESHOLD = 0.95
  
  cs_orig=Cusum(x)
  Sdiff_orig=max(cs_orig)-min(cs_orig)
  
  # bootstap
  repeats = 1000
  Sdiff=rep(0,repeats)
  for(i in 1:repeats){
    
    xbootstrap = sample(x,replace=FALSE)
    cs_bootstrap = Cusum(xbootstrap)
    Sdiff[i] = max(cs_bootstrap) - min(cs_bootstrap)
    
  }
  
  conflevel = length(which(Sdiff<Sdiff_orig))/repeats
  if(conflevel<CONFLEVEL_THRESHOLD){return(NULL)}
  
  Sm = which(abs(cs_orig)==max(abs(cs_orig)))
  Sm=Sm[1]
  
  return(Sm+1)
  
}


MSE = function(x,m){
  n=length(x)
  if(m>n){return(NULL)}
  mx1 = mean(x[1:m])
  mx2 = mean(x[(m+1):n])
  mse = sum((x[1:m]-mx1)^2)+sum((x[(m+1):n]-mx2)^2)
  return(mse)
}

# This function compares two samples for their statistical difference.
# input:
# sample1		data vector of length n1
# sample2 		data vector of length n2
# output:
# TRUE or FALSE, indicating whether the samples are statistically different
# (TRUE) or not (FALSE)
MannWhitneySampleDifferenceTest = function(sample1,sample2){
  n1=length(sample1)
  n2=length(sample2)
  N=n1+n2
  values=c(sample1,sample2)
  samples=c(rep(1,n1),rep(2,n2))
  ranks=rank(values)
  
  Tobs1=sum(ranks[samples==1])
  Tobs2=sum(ranks[samples==2])
  T12=sum(ranks)
  
  T1=n1*(N+1)/2
  T2=n2*(N+1)/2
  if(Tobs1>T1){
    a=-0.5
  }else{
    a=0.5
  }
  sigmaT=sqrt(n1*n2*(N+1)/12)
  z=((Tobs1-T1)+a)/sigmaT
  # 0.05 level of non-directional significance z_critical = 1.960
  # http://vassarstats.net/textbook/ch11a.html 
  z_critical=1.960

  if(abs(z)<z_critical){
    # The zero hypothesis holds: samples are not different
    return(FALSE)
  }else{
    return(TRUE)
  }
}

# This function finally modifies the model CD, composed by stacking the daily link models
# into one table, to a uTable format.
# input:
# CD		model produced by stacking the output of ChangePointSegmentingDaily for every link
# output:
# ut		the uTable
uTable = function(CD){
  # set the resolution: e.g. check the value of u at every 60 seconds
  resolution=60*5
  # split the model into links
  L=split(CD,list(CD$stopcode,CD$prevstop),drop=TRUE)
  # for each link, create the u values from 5AM to 10PM, at steps indicated by resolution
  ut=rbindlist(lapply(L,uValues,resolution))
  return(ut)
}

# This function composes the u and m values for each desired time of day instant for one link model
# input:
# l			the CD model of one link
# res		the resolution of the model, e.g. 300 seconds
# output:
# D			the uTable values for this one link
uValues = function(l,res){
  stopcode=l$stopcode[1]
  prevstop=l$prevstop[1]
  morning=5*3600
  evening=22*3600-res
  timesteps=seq(from=morning,to=evening,by=res)
  u=rep(0,length(timesteps))
  m=rep(0,length(timesteps))
  # percentage p
  p=0.75
  
  for(i in 1:length(timesteps)){
    # for each observation day, find the upper value corresponding to the timestep
    val=l$upper[which(l$starttime<=timesteps[i] & l$endtime>timesteps[i])]
    # sort the values
    su=sort(val)
    u[i]=su[round(p*length(su))]
    
    # take also the median of medians
    mval=l$med[which(l$starttime<=timesteps[i] & l$endtime>timesteps[i])]
    sm=sort(mval)
    m[i]=round(median(sm))
    
  }
  D=data.frame(stopcode,prevstop,timesteps,u,m)
  return(D)
}

