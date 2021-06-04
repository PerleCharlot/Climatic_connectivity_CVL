#############################################################
# Temperature Propagation + Assessing climatic connectivity #
#############################################################
library(plyr)
library (matlab)
library(data.table)

setwd("C:/Users/charlotp/Documents/propagation/data_all_Austria/mean_500m")

#Inputs
MAT <- read.csv("MAT.csv")
Neighbors<-read.csv("Neighbors.csv")
AllAUCorridors<-read.csv("C:/Users/charlotp/Documents/MONOTONY/AllAUCorridors.csv") 

# Origin = coreId1 (origin core); Dest = coreId2 (destination core); 
# CW_Dist = lcDist (distance in cost); LCP_Length = lcpLength (corridor length, we want to keep those <= 10 000m);
# Euc_Dist = eucDist (euclidien) don't know what is used for

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


#Second we will determine the Origin (warmer) & Dest (cooler) Cores for corridors
AllAUCorridors<-cbind(AllAUCorridors,Origin=0, Dest=0)
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="From_Core"]<-"Cores"
#colnames(AllAUCorridors)[1]<-"Cores"
AllAUCorridors<-join(AllAUCorridors, MAT, type= "full", by= "Cores")
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Mean"]<-"Mean1"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Cores"]<-"Cores1"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="To_Core"]<-"Cores"
#colnames(AllAUCorridors)[2]<-"Cores"
AllAUCorridors<-join(AllAUCorridors, MAT, type= "inner", by= "Cores")
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Mean"]<-"Mean2"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Cores"]<-"Cores2"
n<-nrow(AllAUCorridors)
for (i in 1:n)
{
  if (AllAUCorridors$Mean1[i]>AllAUCorridors$Mean2[i])
  {AllAUCorridors$Origin[i]<-AllAUCorridors$Cores1[i]
  AllAUCorridors$Dest[i]<-AllAUCorridors$Cores2[i]}
  
  else {AllAUCorridors$Origin[i]<-AllAUCorridors$Cores2[i]
  AllAUCorridors$Dest[i]<-AllAUCorridors$Cores1[i]}
}
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Origin"]<-"Cores"
AllAUCorridors<-join(AllAUCorridors, MAT, type= "inner", by= "Cores")
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Mean"]<-"MeanO"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Cores"]<-"Origin"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Dest"]<-"Cores"
AllAUCorridors<-join(AllAUCorridors, MAT, type= "inner", by= "Cores")
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Mean"]<-"MeanD"
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Cores"]<-"Dest"

KEEP <- AllAUCorridors
AllAUCorridors<-AllAUCorridors[,c(6:12,17,19,20,23,24)] 
colnames(AllAUCorridors)[colnames(AllAUCorridors)=="Euc_Dist"]<-"eucDist"

write.csv(AllAUCorridors, file = "C:/Users/charlotp/Documents/MONOTONY/AllAUCorridors_ordered.csv")

##Next we will combine the table of adjacent connections with the table of corridor connections
Links<-as.data.frame(AllAUCorridors)
LinkStep<-Links

#renumber & reorder "Links", the file that contains corridor costs
LinkStep<-LinkStep[order(LinkStep$Origin,LinkStep$Dest),]
row.names(LinkStep)<-seq.int(1,nrow(LinkStep))
colnames(LinkStep)[colnames(LinkStep)=="Origin"]<-"Cores"
LinkStep<- join(LinkStep, MAT, type= "inner", by= "Cores")
colnames(LinkStep)[colnames(LinkStep)=="Mean"]<-"MeanO"
colnames(LinkStep)[colnames(LinkStep)=="Cores"]<-"Origin"
colnames(LinkStep)[colnames(LinkStep)=="Dest"]<-"Cores"  
LinkStep<- join(LinkStep, MAT, type= "inner", by= "Cores")
colnames(LinkStep)[colnames(LinkStep)=="Mean"]<-"MeanD"
colnames(LinkStep)[colnames(LinkStep)=="Cores"]<-"Dest"
colnames(LinkStep)[colnames(LinkStep)=="CW_Dist"]<-"lcDist"
colnames(LinkStep)[colnames(LinkStep)=="Euc_Dist"]<-"eucDist"
colnames(LinkStep)[colnames(LinkStep)=="LCP_Length"]<-"lcpLength"

LinkStep <- LinkStep[,-c(13,14)]

### Subset corridors
## Keep only corridors which are shorter than 10 km
LinkStep2 <- subset(LinkStep, LinkStep$lcpLength <= 10000) #From 31 739 to 21 801 corridors
## Keep only corridors which are strictly monotonic
non.monotonic <- fread(file="C:/Users/charlotp/Documents/MONOTONY/monotony_summary.csv")
todelete <- subset(non.monotonic, non.monotonic$Status == "Non-monotonic")
LinkStep2 <- LinkStep2[-which(LinkStep2$corridor_ID %in% todelete$Corridor),] 
#From 21 801 to 11 086 corridors
write.csv(LinkStep2, file="C:/Users/charlotp/Documents/MONOTONY/AllAUCorridors_subset.csv")

# #Merge dataset of neighbors & links with mean MATs
neigh_imp <- Neighbors
neigh_imp$corridor_ID <- NA
colnames(LinkStep2)[colnames(LinkStep2)=="cw_to_Euc_Dist_Ratio"]<-"cwdToEucRatio"
colnames(LinkStep2)[colnames(LinkStep2)=="cwd_to_Path_Length_Ratio"]<-"cwdToPathRatio"

LinkStep <-rbind(LinkStep2, neigh_imp)
LinkStep<-LinkStep[order(LinkStep$Origin,LinkStep$Dest),]
row.names(LinkStep)<-seq.int(1,nrow(LinkStep))

#test for any duplicates
LinkStep4 <-cbind(LinkStep, corecode=paste(as.character(LinkStep$Origin),as.character(LinkStep$Dest)))
dupcorecode<-which(duplicated(LinkStep4$corecode)==TRUE)
LinkStep <- LinkStep[-dupcorecode,] #17 848

#Create a list of connections from hot to cold + the costs for each if using corridors
connections<-as.data.frame(cbind(Origin=LinkStep$Origin,Dest=LinkStep$Dest, EucDist=LinkStep$eucDist, 
                                 LeastCost=LinkStep$lcDist, LCP=LinkStep$lcpLength))
connections<-unique(connections)
#write.csv(connections, file="C:/Users/charlotp/Documents/MONOTONY/connections_withCOR.csv")
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
howmuch<-howmuch[,-9]
howmuch <- replace(howmuch, is.na(howmuch), 0)
write.csv(howmuch, file="Output/howmuch_COR.csv")

print(paste("number isolated patches with corridors: ", dim(MAT)[1] - dim(howmuch)[1], sep=""))

#Loop to assess Climatic Connectivity Success for all dates and RCPs scenario
#COR_test = all corridors, including those >10 km + non-monotonic
list_successful = c()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    #scenario=85
    #year=2070
    AllData<-howmuch
    newcores <- howmuch
    oldcores <- fread(paste("rcp",scenario,"_",year,".csv", sep ="")) #"rcpSCENARIO_YEAR.csv"
    cores <- oldcores[which(oldcores$Cores %in%  newcores$Core),]
    colnames(howmuch)[colnames(howmuch)=="Core"]<-"Cores"
    bonusdata <- join(cores, howmuch, by = "Cores")
    colnames(bonusdata)[colnames(bonusdata)=="Final_Temp"]<-"TempF_100"
    colnames(bonusdata)[colnames(bonusdata)=="Final_Core"]<-"CoreF_100"
    nameBD <- paste("BonusData_rcp", scenario, "_", year, ".csv", sep ="")
    write.csv(bonusdata, file = nameBD)
    BonusData <- bonusdata
    FutMAT<-as.data.frame(cbind(Cores=BonusData$Cores, Fut_T=BonusData$Mean_fut))
    colnames(AllData)[colnames(AllData)=="Core"]<-"Cores"
    AllData<-join(AllData, MAT, by="Cores", type="left")
    AllData<-join(AllData, FutMAT, by="Cores", type="left")
    AdjData<-as.data.frame(cbind(Cores=BonusData$Cores, TempF_0=BonusData$TempF_0, CoreF_0=BonusData$CoreF_0))
    AllData<-join(AllData, AdjData, by="Cores", type="left")
    colnames(AllData)[colnames(AllData)=="Final_Core"]<-"CoreF_100" #destination patch
    colnames(FutMAT)<-c("CoreF_100", "FTempF_100")
    AllData<-join(AllData, FutMAT, by="CoreF_100", type="left")
    colnames(FutMAT)<-c("Cores", "Fut_T")
    colnames(AllData)[colnames(AllData)=="Final_Temp"]<-"TempF_100" #temperature destination patch current
    AllData<-cbind(AllData, dFut_T=(AllData$Fut_T-AllData$Mean), 
                   TempD_100=(AllData$Mean-AllData$TempF_100), 
                   FTempD_100=(AllData$Fut_T-AllData$FTempF_100), 
                   HowFar_100=(AllData$Mean-AllData$FTempF_100))
    
    #HowFar = T Origine Current - T Destination Future
    #HowFar < 0 <=> TcO < TfD : FAIL
    #HowFar >= 0 <=> TcO >= TfD : SUCCESS
    
    nameAD <- paste("Output/AllData_rcp", scenario, "_", year, "_COR.csv", sep ="")
    write.csv(AllData, file=nameAD)
    
    ### Always successful patches
    #AL <- AllData[which(AllData$HowFar_100 >= 0),]
    #PatSuc <- AL$Cores
    #list_successful <- append(list_successful, PatSuc)
    
    ###Statistics!
    Areas<-read.csv("Areas2.csv")
    AuData <- AllData
    Tarea1<-sum(Areas$Area)
    DataAustria<-join(AuData, Areas, type="left", by="Cores" )
    
    nameS2 <- paste("Output/CSindetails_rcp", scenario, "_", year, "_COR.csv", sep ="")
    write.csv(DataAustria, file=nameS2)
    
    Tsuccess0<-subset(DataAustria, DataAustria$HowFar_100 > 0)
    Tsuccess0bis<-subset(DataAustria, DataAustria$HowFar_100 >= 0)
    Tarea<-sum(DataAustria$Area)
    Tarea #areas sum of cores which are not isolated (7093 cores)
    Tarea1 #areas sum of all cores (7530 cores)
    Success<-as.data.frame(cbind(Corrid.count=c(0,0),Corrid.freq=c(0,0), Corrid.area=c(0,0), Corrid.pctA=c(0,0)))
    row.names(Success)<-c("Total >", "Total >=")
    Success$Corrid.count[1]<-nrow(Tsuccess0) #number of cores connected to future suitable climate (TcO > TfD)
    Success$Corrid.count[2]<-nrow(Tsuccess0bis)#number of cores connected to future suitable climate (TcO >= TfD)
    Success$Corrid.freq[1]<-round((nrow(Tsuccess0)/nrow(DataAustria))*100, digits=1)#number connected cores/numner total cores
    Success$Corrid.freq[2]<-round((nrow(Tsuccess0bis)/nrow(DataAustria))*100, digits=1)
    Success$Corrid.area[1]<-(sum(Tsuccess0$Area))#area of connected cores
    Success$Corrid.area[2]<-(sum(Tsuccess0bis$Area))
    Success$Corrid.pctA[1]<-round((Success$Corrid.area[1]/Tarea1)*100, digits=1)# proportion of area (connected area/total area)
    Success$Corrid.pctA[2]<-round((Success$Corrid.area[2]/Tarea1)*100, digits=1)
    print(Success)
    nameS <- paste("Output/Success_rcp", scenario, "_", year, "_COR.csv", sep ="")
    write.csv(Success, file=nameS)
    rm(AdjData, BonusData,bonusdata,FutMAT,Success, oldcores,Tsuccess0, Tsuccess0bis, DataAustria, AllData)
  }
}
