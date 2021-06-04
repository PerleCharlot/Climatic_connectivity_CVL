##############################################
# Order Corridors from Origin to Destination #
##############################################
library(plyr)
library (matlab)

setwd("C:/Users/charlotp/Documents/propagation/data_sample_area/smooth_500m")

#this is a list of the mean MAT of each core
MAT <- read.csv("MAT.csv")

#this is a table describing the connectivity of adjacent cores: 
#note that these connections are not ordered by warmer to colder cores; that is done here
Neighbors<-read.csv("AllAUCorridors1.csv")

#First we will determine the Origin (warmer) & Dest (cooler) Cores for all adjacent core pairs
Neighbors<-cbind(Neighbors,Origin=0, Dest=0)

#joining the temperature of each set of cores (Core1 & Core2)
colnames(Neighbors)[1]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "full", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean1"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores1"

colnames(Neighbors)[2]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean2"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores2"

n<-nrow(Neighbors)

#lists the hotter core # as the "Origin" & cooler core # as the "Dest"
for (i in 1:n)
{
  if (Neighbors$Mean1[i]>Neighbors$Mean2[i])
  {Neighbors$Origin[i]<-Neighbors$Cores1[i]
  Neighbors$Dest[i]<-Neighbors$Cores2[i]}
  
  else {Neighbors$Origin[i]<-Neighbors$Cores2[i]
  Neighbors$Dest[i]<-Neighbors$Cores1[i]}
}

#Delete the temperatures used for det. origin & Dest.
Neighbors<-Neighbors[,(1:4)]

#Joins Mean MATs for Neighbor Origin & Dest Cores
colnames(Neighbors)[colnames(Neighbors)=="Origin"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanO"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Origin"

colnames(Neighbors)[colnames(Neighbors)=="Dest"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanD"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Dest"

Neighbors<-cbind(Neighbors, eucDist=0, lcDist=0, lcpLength=0, cwdToEucRatio=0, cwdToPathRatio=0)
write.csv(Neighbors, file = "corridors_ordered.csv")
