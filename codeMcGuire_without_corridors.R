#############################################################
# Temperature Propagation + Assessing climatic connectivity #
#############################################################
library(plyr)
library (matlab)
library(data.table)
library(readxl)

setwd("C:/Users/charlotp/Documents/propagation/data_all_Austria/mean_500m")

#Inputs
MAT <- read.csv("MAT.csv")
Neighbors<-read.csv("Neighbors.csv")

#First we will determine the Origin (warmer) & Dest (cooler) Cores for all adjacent core pairs
Neighbors<-cbind(Neighbors,Origin=0, Dest=0)
colnames(Neighbors)[1]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean1"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores1"
colnames(Neighbors)[2]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean2"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores2"
n<-nrow(Neighbors)
for (i in 1:n)
{
  if (Neighbors$Mean1[i]>Neighbors$Mean2[i])
  {Neighbors$Origin[i]<-Neighbors$Cores1[i]
  Neighbors$Dest[i]<-Neighbors$Cores2[i]}
  
  else {Neighbors$Origin[i]<-Neighbors$Cores2[i]
  Neighbors$Dest[i]<-Neighbors$Cores1[i]}
}
Neighbors<-Neighbors[,(1:4)]
colnames(Neighbors)[colnames(Neighbors)=="Origin"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanO"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Origin"
colnames(Neighbors)[colnames(Neighbors)=="Dest"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanD"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Dest"
Neighbors<-cbind(Neighbors, eucDist=0, lcDist=0, lcpLength=0, cwdToEucRatio=0, cwdToPathRatio=0)
LinkStep<-Neighbors

connections<-as.data.frame(cbind(Origin=LinkStep$Origin,Dest=LinkStep$Dest, EucDist=LinkStep$eucDist, LeastCost=LinkStep$lcDist, LCP=LinkStep$lcpLength))
connections<-unique(connections)
coreID<-unique(c(LinkStep$Origin,LinkStep$Dest)) 
coreID<-as.data.frame(coreID, colname="Cores")
coreID<-as.data.frame((coreID[order(coreID[,1]),]))
colnames(coreID)[1]<-"Cores"
temps<-as.data.frame(join(coreID,MAT, type="inner", by="Cores"))

##This calculates the coolest destination reachable from any origin core
howmuch<-function(connections,temps,coreID)
{
  #set up output file
  r<-nrow(temps)
  howfar<-as.data.frame(cbind(Core=coreID[,1],Final_Temp=rep(0,r),Final_Core=rep(0,r), Final_Cost=rep(0,r), EucDist=rep(0,r), PathDist=rep(0,r)))
  #set up variables
  runningcore<-coreID
  connectsto<-vector("list", r)
  uniquetemps<-sort(unique(temps$Mean))
  runningtemps<-temps
  n<-length(uniquetemps)
  runningcosts<-cbind(Cores=coreID, cost=rep(0,r))
  
  #populate "connectsto", which is a vector of the cores to which each origin core connects
  #each row is the list of cores to which the origin core (in order of coreID) connects
  for (i in 1:r) {
    inds<-which(connections$Origin==coreID[i,"Cores"])
    connectsto[[i]]= connections[inds,"Dest"]}
  
  #feeds through each of the unique temperatures colder to warmer
  #finds the list of cores that correspond with each unique temperature
  for (j in 1:n) {
    warmer<-uniquetemps[j]
    inds<-which(runningtemps$Mean==warmer)
    #feeds through the list of cores that corresponds with the active unique temperature
    for (k in 1:(length(inds))) {
      ii<-inds[k]
      #if the active core does not connect to any other core, populate howfar w/
      #Final_Temp with the active temperature
      #Final_Core with the current core # (itself)
      if (isempty (connectsto[[ii]])){
        howfar$Final_Temp[ii]<-warmer
        howfar$Final_Core[ii]<-runningcore[ii,"Cores"]
      } 
      #if the active core does connect to another core
      #make an index of where the destination (colder) cores are in the initial core list
      #find the minimum temperature of those cores and figure out to which core it belongs
      else {
        tocoresinds<-match(connectsto[[ii]],coreID[,"Cores"])
        t<-min(runningtemps$Mean[tocoresinds])
        a<-which(runningtemps$Mean[tocoresinds]==min(runningtemps$Mean[tocoresinds]))
        #if there is only one coldest core, index that destination (colder) core & 
        #make the cost of that link= the cost of the cold core to which it connects + the cost of the new link
        if (length(a)==1)
        {
          minind<-tocoresinds[a]
          #this figures out the cost of the new link
          b<-which(connections$Origin==coreID$Cores[ii]&connections$Dest==coreID$Cores[minind])
          linkcosts<-connections$LeastCost[b]
          #this figures out the previous summed cost for the destination (colder) core
          initcosts<-runningcosts$cost[minind]
          #this sums the two to create an updated cost
          cost<-linkcosts+initcosts
        }
        #if there is more than one colder core w/ = temperatures, select the path with the lowest cost
        else
        {
          #this figures out the cost of the new link
          b<-which(connections$Origin==coreID$Cores[ii])
          linkcosts<-connections$LeastCost[b]
          linkcosts<-linkcosts[a]
          #this figures out the previous summed cost for the destination (colder) core options
          c<-which(runningcosts$Cores %in% coreID$Cores[tocoresinds])
          initcosts<-runningcosts$cost[c]
          initcosts<-initcosts[a]
          d<-linkcosts+initcosts
          #this selects the path with the lowest cost & assigns that low cost to be the new cost
          cost<-min(d)
          e<-which(d==min(d))
          #some destination (colder) cores have the same low temp & cost; arbitrarily select the first one
          length(e)<-1
          #this indexes the destination (colder) core selected (w/ coldest temp & lowest cost)
          minind<-tocoresinds[a[e]]
        }
        #make the final temp of the active core the new minimum temperature
        #make the current listed temperature of that core now == the new min. temperature
        #make the final core of the active core the new colder (destination) core #
        #have new colder (destination) core # replace the core # in runningcore
        #update the running cosst & the final cost of the active core
        #sum the Euc. distance and path length of the old & new path
        howfar$Final_Temp[ii]<-t
        runningtemps$Mean[ii]<-t
        howfar$Final_Core[ii]<-runningcore[minind,]
        runningcore[ii,]<-runningcore[minind,]
        runningcosts$cost[ii]<-cost
        howfar$Final_Cost[ii]<-cost
        distind<-which(connections$Origin==coreID$Cores[ii]&connections$Dest==coreID$Cores[minind])
        howfar$EucDist[ii]<-(howfar$EucDist[minind]+connections$EucDist[distind])
        howfar$PathDist[ii]<-(howfar$PathDist[minind]+connections$LCP[distind])
      }
    }
  }
  howfar<-cbind(howfar, CostToEuc=(howfar$Final_Cost/howfar$EucDist), CostToPath=(howfar$Final_Cost/howfar$PathDist))
  return(howfar)
}
howmuch<-howmuch(connections,temps,coreID)
howmuch<-cbind(howmuch, wrong=0)
for (m in 1:nrow(howmuch)){if(howmuch$Final_Temp[m]!=temps[which(temps$Cores==howmuch$Final_Core[m]),"Mean"])
{howmuch$wrong[m]<-1}}
which(howmuch$wrong==1)
for (m in 1:nrow(howmuch)){if(howmuch$Final_Cost[m]==0 & howmuch$EucDist[m]>0)
{howmuch$wrong[m]<-1}}
which(howmuch$wrong==1)
howmuch<-howmuch[,-9] # ??
howmuch <- replace(howmuch, is.na(howmuch), 0)
write.csv(howmuch, file="Output/howmuch.csv")

print(paste("number isolated patches: ", dim(MAT)[1] - dim(howmuch)[1], sep=""))


# Alps <- read_excel("C:/Users/charlotp/Documents/propagation/alpine_cores.xlsx")
# Alps$alpine <- c("alpine")
# bb <- merge(Alps,Areas, by.x="core_ID", by.y="Cores", all=T)
# bb <- bb[,c(1,5,6)]
# bb$alpine[is.na(bb$alpine)] <- c("nonalpine")
# write.csv(bb, file ="Areas2.csv")
  
#Loop to assess Climatic Connectivity Success for all dates and RCPs scenario
list_successful = c()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    
    AllData<-howmuch
    newcores <- howmuch
    oldcores <- fread(paste("rcp",scenario,"_",year,".csv", sep ="")) #"rcpSCENARIO_YEAR.csv"
    cores <- oldcores[which(oldcores$Cores %in%  newcores$Core),]
    colnames(howmuch)[colnames(howmuch)=="Core"]<-"Cores"
    bonusdata <- join(cores, howmuch, by = "Cores")
    colnames(bonusdata)[colnames(bonusdata)=="Final_Temp"]<-"TempF_0"
    colnames(bonusdata)[colnames(bonusdata)=="Final_Core"]<-"CoreF_0"
    nameBD <- paste("BonusData_rcp", scenario, "_", year, ".csv", sep ="")
    write.csv(bonusdata, file = nameBD)
    BonusData <- bonusdata
    FutMAT<-as.data.frame(cbind(Cores=BonusData$Cores, Fut_T=BonusData$Mean_fut))
    colnames(AllData)[colnames(AllData)=="Core"]<-"Cores"
    AllData<-join(AllData, MAT, by="Cores", type="left")
    AllData<-join(AllData, FutMAT, by="Cores", type="left")
    AdjData<-as.data.frame(cbind(Cores=BonusData$Cores, TempF_0=BonusData$TempF_0, CoreF_0=BonusData$CoreF_0))
    AllData<-join(AllData, AdjData, by="Cores", type="left")
    colnames(FutMAT)<-c("CoreF_0", "FTempF_0")
    
    AllData<-join(AllData, FutMAT, by="CoreF_0", type="left")
    
    colnames(FutMAT)<-c("Cores", "Fut_T")
    AllData<-cbind(AllData, dFut_T=(AllData$Fut_T-AllData$Mean), 
                   TempD_0=(AllData$Mean-AllData$TempF_0), 
                   FTempD_0=(AllData$Fut_T-AllData$FTempF_0), 
                   HowFar_0=(AllData$Mean-AllData$FTempF_0))
    #HowFar = T Origine Current - T Destination Future
    #HowFar < 0 <=> TcO < TfD : FAIL
    #HowFar >= 0 <=> TcO >= TfD : SUCCESS
    
    nameAD <- paste("Output/AllData_rcp", scenario, "_", year, ".csv", sep ="")
    write.csv(AllData, file=nameAD)
    
    ### Always successful patches
    AL <- AllData[which(AllData$HowFar_0 >= 0),]
    PatSuc <- AL$Cores
    list_successful <- append(list_successful, PatSuc)
    
    ###Statistics!
    #Areas<-read.csv("CoreAreas.csv")
    Areas<-fread("Areas2.csv")
    AuData <- AllData
    Tarea1<-sum(Areas$Area)
    DataAustria<-join(AuData, Areas, type="left", by="Cores" )
    nameS2 <- paste("Output/CSindetails_rcp", scenario, "_", year, ".csv", sep ="")
    write.csv(DataAustria, file=nameS2)
    
    Tsuccess0<-subset(DataAustria, DataAustria$HowFar_0 > 0)
    Tsuccess0bis<-subset(DataAustria, DataAustria$HowFar_0 >= 0)
    Tarea<-sum(DataAustria$Area)
    Tarea #areas sum of cores which are not isolated (1397 cores)
    Tarea1 #areas sum of all cores (1677 cores)
    Success<-as.data.frame(cbind(Adj.count=c(0,0), Adj.freq=c(0,0), Adj.area=c(0,0), Adj.pctA=c(0,0)))
    row.names(Success)<-c("Total >", "Total >=")
    Success$Adj.count[1]<-nrow(Tsuccess0) #number of cores connected to future suitable climate (TcO > TfD)
    Success$Adj.count[2]<-nrow(Tsuccess0bis)#number of cores connected to future suitable climate (TcO >= TfD)
    Success$Adj.freq[1]<-round((nrow(Tsuccess0)/nrow(DataAustria))*100, digits=1)#number connected cores/numner total cores
    Success$Adj.freq[2]<-round((nrow(Tsuccess0bis)/nrow(DataAustria))*100, digits=1)
    Success$Adj.area[1]<-(sum(Tsuccess0$Area))#area of connected cores
    Success$Adj.area[2]<-(sum(Tsuccess0bis$Area))
    Success$Adj.pctA[1]<-round((Success$Adj.area[1]/Tarea1)*100, digits=1)# proportion of area (connected area/total area)
    Success$Adj.pctA[2]<-round((Success$Adj.area[2]/Tarea1)*100, digits=1)
    nameS <- paste("Output/Success_rcp", scenario, "_", year, ".csv", sep ="")
    write.csv(Success, file=nameS)
    #print(Success)
    rm(AdjData, BonusData,bonusdata,FutMAT,Success, oldcores,Tsuccess0, Tsuccess0bis, DataAustria, AllData)
    ##
    }
}

######### FIND OUT WHICH PATCHES ARE POTENTIALLY UNSUCCESSFUL
### = all patches - those successfull in ALL (rcp * dates)
freq_patch <- as.data.frame(table(list_successful))
always_successful <- freq_patch[which(freq_patch$Freq == 18),] 
write.csv(always_successful, file="Output/always_successful_patches.csv")
#2235 / 7530 always successfull
#7530-2235 = 5295 patches potentially unsuccessful, then we have to base on them to build corridors
pot_unsuccess = subset(MAT, !(MAT$Cores %in% always_successful$list_successful))
write.csv(pot_unsuccess, file="Output/pot_unsuccess_patches.csv")

###############

