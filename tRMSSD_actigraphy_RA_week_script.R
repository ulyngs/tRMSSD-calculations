# read in file (change filename each time here)
datac=read.csv('/Users/priyankapanchal/Documents/Oxford/COMET/Behavioural/Actigraphy/tRMSSD/Data/c81_actigraphy.csv')
datac$datetime=as.POSIXct(paste(as.Date(datac$calender_date,format='%Y-%m-%d')))
datac$timestamp=as.numeric(datac$datetime)

# tRMSSD calculation
tRMSSD <- function(mood,timestamp){
  
  if (length(act)<=1){
    return(NA)
  }
  
  else{
    # Sort timestamp and mood
    act=act[order(timestamp)]
    timestamp=timestamp[order(timestamp)]
    n=length(act)
    dtime = timestamp[2:n]-timestamp[1:n-1]
    dact = act[2:n]-act[1:n-1]
    value = sqrt(1/n*sum((dact/dtime)^2))
    
    return(value)
  }
}  

# define mood and calculate weeks
act=datac$RA  # Used whatever measurement of mood/actigraphy you need - using RA here
ts=datac$timestamp
nWeeks=floor(length(ts)/7) # This calculates the number of weeks you have in your sample
rmssdPerWeek=vector('double',nWeeks) # This creates an empty vector that will contain the weekly RMSSD values

# calculate tRMSSD per week
for (i in 1:nWeeks){
  actForTheWeek=act[((i-1)*7+1):(i*7)]
  tsForTheWeek=ts[((i-1)*7+1):(i*7)]
  rmssdPerWeek[i]=tRMSSD(actForTheWeek,tsForTheWeek)
}

# write to .txt file (change filename each time here)
write(rmssdPerWeek, file = "/Users/priyankapanchal/Documents/Oxford/COMET/Behavioural/Actigraphy/tRMSSD/weekly_tRMSSD/c81_week_trmssd_actigraphy_RA.txt")
