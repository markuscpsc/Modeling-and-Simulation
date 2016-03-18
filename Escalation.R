#GitHub 
#Escalation Trap
#Nuclear Escalation
#Markus Malmgren


rm(list=ls())
time.delay<-3



#itot and ttoi
itot <- function(i) (i-1)*delta.t + 1950
ttoi <- function(t) (t-1950)/delta.t + 1

escalation.sim<-function(
#Parameters
init.USA.GDP=1.5*10^12, #US dollars
init.USSR.GDP=5.1*10^11, #also in US dollars
init.US.Nukes=10, #Nukes
init.USSR.Nukes=0, #Nukes
EscalationConstant=3, 
NukeCost=1*10^9, #Dollars/Nuke
USeconomicGrowthRate=.07, #percent/year
SovietEconomicGrowthRate= .05, #percent/year
sim.length=2000
) {
  
#Time
delta.t<-1 #year
time<-seq(1950,sim.length,delta.t) 

USN<-vector(length=length(time))
USSRN<-vector(length=length(time))
USresources<-vector(length=length(time))
USSRresources<-vector(length=length(time))

#Initial Conditions
USN[1]<-init.US.Nukes
USSRN[1]<-init.USSR.Nukes
USresources[1]<-init.USA.GDP
USSRresources[1]<-init.USSR.GDP
collapsed<-FALSE

# Simulation
for (i in 2:length(time)) {
  #Default values 
  US.Nukes.Prime<-0 #nukes/yr
  USSR.Nukes.Prime<-0 #nukes/yr
  
  # Compute flows
  if((USN[i-1]<USSRN[i-1]) && (USresources[i-1]>0)){
    US.Nukes.Prime <- USSRN[i-1] * EscalationConstant   # Nukes/yr
  }
  else if(USN[i-1]>USSRN[i-1]) {
    US.Nukes.Prime<-0 #Nukes/year
  }
  else if((USresources[i-1]<=0)&&(collapsed==FALSE)){
    print(c("United States Collapsed in",time[i-2]))
    
    US.Nukes.Prime<-0 #Nukes/year
    collapsed<-TRUE
    collapseYR<-time[i-2]
  }
  
  if((USN[i-1]>USSRN[i-1]) && (USSRresources[i-1]>0)){
    USSR.Nukes.Prime<-USN[i-1] * EscalationConstant #Nukes/yr
  }   
  else if(USN[i-1]<USSRN[i-1]){
    USSR.Nukes.Prime<-0 #Nukes/yr
  }
  else if((USSRresources[i-1]<=0)&&(collapsed==FALSE)){
    print(c("Soviet Union Collapsed in",time[i-2]))
    
    USSR.Nukes.Prime<-0 #Nukes/yr
    collapsed<-TRUE
    collapseYR<-time[i-2]
  }
  
  US.GDP.Prime<- USresources[i-1]*USeconomicGrowthRate - NukeCost*US.Nukes.Prime    #Dollars/year
  USSR.GDP.Prime<-USSRresources[i-1]*SovietEconomicGrowthRate - NukeCost*USSR.Nukes.Prime   #Dollars/year
  
  
  # Compute stocks
  USN[i]<- USN[i-1] + US.Nukes.Prime *delta.t #Nukes
  USSRN[i]<- USSRN[i-1] + USSR.Nukes.Prime #Nukes
  USresources[i]<- USresources[i-1] + US.GDP.Prime*delta.t #Dollars
  USSRresources[i]<-USSRresources[i-1] + USSR.GDP.Prime*delta.t #Dollars
  }
return(data.frame(time=time,USNukes=USN,USSRNukes=USSRN,USGDP=USresources,USSRGDP=USSRresources))
}
plot.escaltion.sim<-function(sim.results){
all.values<-c(USN,USSRN)
plot(sim.results$time,sim.results$USN,type="l",col="blue",lwd=2,ylim=c(min(all.values),max(all.values)),
     main="Nuclear Escalation",
     xlab="Year", ylab="# of Nukes")
lines(sim.results$time,sim.results$USSRN,col="red",lty="dotted",lwd=2)
#abline(v=collapseYR,col="purple",untf=FALSE,lty="dashed")
legend("topleft",legend=c("US Nukes", "Soviet Nukes"),
       fill=c("blue","red"))

Sys.sleep(time.delay)
max.values<-c(USresources,USSRresources)
plot(time,USresources,col="green",lty="dotted",lwd=2,ylim=c(min(max.values),max(max.values)),main="The Economic Cost of Nuclear Escalation",
     xlab="Year", ylab="National Resources")
lines(time,USSRresources,col="black",lty="dotted",lwd=3)
abline(h=0, col="red",untf=FALSE,lty="dashed")
abline(v=collapseYR,col="purple",untf=FALSE,lty="dashed")
legend("topleft",legend=c("US Resources", "Soviet Resources","Economic Collapse Point","Year of Economic Collapse"),
       fill=c("green","black","red","purple"))
}

