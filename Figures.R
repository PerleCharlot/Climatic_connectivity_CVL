###########
# Figures #
###########
library(data.table)
library(ggplot2)
library(ggpubr)
library(plotly)

setwd("C:/Users/charlotp/Documents/propagation/data_all_Austria/mean_500m")

Areas<-read.csv("CoreAreas.csv")
tot_area<-sum(Areas$Area)

####### *** Fig 2: CCS in details **** ######
##### Density chart #####

#### 2030 #### 
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2030_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2030_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2030_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2030.csv")
rcp45A = fread("Output/CSindetails_rcp45_2030.csv")
rcp85A = fread("Output/CSindetails_rcp85_2030.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL30 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL30$RCP <- as.factor(TOTAL30$RCP)
#### 2040 ####
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2040_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2040_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2040_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2040.csv")
rcp45A = fread("Output/CSindetails_rcp45_2040.csv")
rcp85A = fread("Output/CSindetails_rcp85_2040.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL40 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL40$RCP <- as.factor(TOTAL40$RCP)


#### 2050 #### 
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2050_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2050_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2050_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2050.csv")
rcp45A = fread("Output/CSindetails_rcp45_2050.csv")
rcp85A = fread("Output/CSindetails_rcp85_2050.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL50 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL50$RCP <- as.factor(TOTAL50$RCP)

#### 2060 #### 
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2060_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2060_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2060_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2060.csv")
rcp45A = fread("Output/CSindetails_rcp45_2060.csv")
rcp85A = fread("Output/CSindetails_rcp85_2060.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL60 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL60$RCP <- as.factor(TOTAL60$RCP)

#### 2070 #### 
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2070_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2070_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2070_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2070.csv")
rcp45A = fread("Output/CSindetails_rcp45_2070.csv")
rcp85A = fread("Output/CSindetails_rcp85_2070.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL70 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL70$RCP <- as.factor(TOTAL70$RCP)
#### 2080 #### 
#Load corridors data
rcp26 = fread("Output/CSindetails_rcp26_2080_COR.csv")
rcp45 = fread("Output/CSindetails_rcp45_2080_COR.csv")
rcp85 = fread("Output/CSindetails_rcp85_2080_COR.csv")
rcp26 <- rcp26[,c(16,17)]
rcp26$RCP <- c(rep(26,length(rcp26$HowFar_100)))
rcp45 <- rcp45[,c(16,17)]
rcp45$RCP <- c(rep(45,length(rcp45$HowFar_100)))
rcp85 <- rcp85[,c(16,17)]
rcp85$RCP <- c(rep(85,length(rcp85$HowFar_100)))

count_T <- c(table(rcp26$HowFar_100))
count_A <- aggregate(rcp26$Area, by=list(Category=rcp26$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45$HowFar_100))
count_A <- aggregate(rcp45$Area, by=list(Category=rcp45$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85$HowFar_100))
count_A <- aggregate(rcp85$Area, by=list(Category=rcp85$HowFar_100), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_COR <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_COR$condition <- c(rep("Corridors",length(TOTAL_COR$HowFar_Area)))

#Load adjacency data
rcp26A = fread("Output/CSindetails_rcp26_2080.csv")
rcp45A = fread("Output/CSindetails_rcp45_2080.csv")
rcp85A = fread("Output/CSindetails_rcp85_2080.csv")

rcp26A <- rcp26A[,c(18,19)]
rcp26A$RCP <- c(rep(26,length(rcp26A$HowFar_0)))
rcp45A<- rcp45A[,c(18,19)]
rcp45A$RCP <- c(rep(45,length(rcp45A$HowFar_0)))
rcp85A <- rcp85A[,c(18,19)]
rcp85A$RCP <- c(rep(85,length(rcp85A$HowFar_0)))

count_T <- c(table(rcp26A$HowFar_0))
count_A <- aggregate(rcp26A$Area, by=list(Category=rcp26A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x
BON <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(26,length(rep(df$Category,df$new_freq2)))))
names(BON) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp45A$HowFar_0))
count_A <- aggregate(rcp45A$Area, by=list(Category=rcp45A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON45 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(45,length(rep(df$Category,df$new_freq2)))))
names(BON45) <- c("HowFar_Area","RCP")

count_T <- c(table(rcp85A$HowFar_0))
count_A <- aggregate(rcp85A$Area, by=list(Category=rcp85A$HowFar_0), FUN=sum)
df <-data.frame(cbind(count_T,count_A))
df$new_freq <- (df$x/tot_area)*df$count_T
df$new_freq2 <- df$x 
BON85 <- as.data.frame(cbind(rep(df$Category,df$new_freq2),rep(85,length(rep(df$Category,df$new_freq2)))))
names(BON85) <- c("HowFar_Area","RCP")

TOTAL_ADJ <- as.data.frame(rbind(BON,BON45,BON85))
TOTAL_ADJ$condition <- c(rep("Adjacency",length(TOTAL_ADJ$HowFar_Area)))

#Merge Cor and Adj
TOTAL80 <- as.data.frame(rbind(TOTAL_ADJ,TOTAL_COR))
TOTAL80$RCP <- as.factor(TOTAL80$RCP)
  
# ## Checks we have the same values in connected areas as previous analyses
# sum(rcp26A$Area[which(rcp26A$HowFar_0 >= 0)])

#### ALL DATES ####
a <- TOTAL30
a$year <- c(rep("2030",length(a$HowFar_Area)))
b <- TOTAL40
b$year <- c(rep("2040",length(b$HowFar_Area)))
c <- TOTAL50
c$year <- c(rep("2050",length(c$HowFar_Area)))
d <- TOTAL60
d$year <- c(rep("2060",length(d$HowFar_Area)))
e <- TOTAL70
e$year <- c(rep("2070",length(e$HowFar_Area)))
f <- TOTAL80
f$year <- c(rep("2080",length(f$HowFar_Area)))
tentative <- data.frame(rbind(a,b,c,d,e,f))

ggplot(tentative,aes(x=HowFar_Area)) + 
  geom_histogram(aes(y = ..density..,  fill = RCP), color = "black",size=0.3,
                 alpha = 0.3, position = "identity",binwidth = 1) + 
  geom_density(aes(color = RCP), size =1, adjust=2)+
  facet_grid(condition~year,scales="free",space="free")+
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'))+
  scale_colour_manual( values = c('skyblue','palegreen3','tomato'))+
  scale_x_continuous(limits=c(-6,12), breaks = seq(-6,12,1))+
  scale_y_continuous(labels=scales::percent,limits = c(0,0.23))+
  labs(x= "Margin of success or failure", y = "Protected Area Network")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white", color="black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(margin=margin(10,0,0,0), size = 15),
        axis.title.y = element_text(margin=margin(0,10,0,0), size = 15),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(colour="black"),
        legend.position="bottom")+
  geom_segment(aes(x = -0.5, y = 0, xend = -0.5, yend = 0.228), linetype=2, size = 1.5,show.legend=F)


ggsave("C:/Users/charlotp/Desktop/figures/figureS1.png", width =60, height =30, units = "cm", dpi=500)




#### 2030 - 2080 ####
a <- TOTAL30
a$year <- c(rep("2030",length(a$HowFar_Area)))
b <- TOTAL80
b$year <- c(rep("2080",length(b$HowFar_Area)))
tentative <- data.frame(rbind(a,b))

ann_text <- data.frame(x=c(-4,7),y=c(0.075,0.075),condition=c("Adjacency"),year=factor("2030",levels=c("2030","2080")),
                       label=c("Failure","Success"),RCP=factor("26",levels=c("26","45","85")))
ann_line<-data.frame(xmid=-0.5,xmin=-6,xmax=11,y0=0.05,y2=0.07,y=0.065, year=factor("2030",levels=c("2030","2080")),
                     condition=factor("Adjacency",levels=c("Adjacency","Corridors")),
                     RCP=factor("26",levels=c("26","45","85")))
ann_lineCor <- data.frame(x=-0.5,xend=-0.5,y=0,yend=0.17, 
                          condition=factor("Corridors",levels=c("Adjacency","Corridors")),
                          RCP=factor("26",levels=c("26","45","85")))
ann_lineAdj <- data.frame(x=-0.5,xend=-0.5,y=0,yend=0.228, 
                          condition=factor("Adjacency",levels=c("Adjacency","Corridors")),
                          RCP=factor("26",levels=c("26","45","85")))


fig2 <- ggplot(tentative,aes(x=HowFar_Area)) + 
  geom_histogram(aes(y = ..density..,  fill = RCP), color = "black",size=0.3,
                 alpha = 0.3, position = "identity",binwidth = 1) + 
  geom_density(aes(color = RCP), size =1, adjust=2)+
  facet_grid(condition~year,scales="free",space="free")+
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'))+
  scale_colour_manual( values = c('skyblue','palegreen3','tomato'))+
  scale_x_continuous(limits=c(-6,12), breaks = seq(-6,12,1))+
  scale_y_continuous(labels=scales::percent,limits = c(0,0.23))+
  labs(x= "Margin of success or failure", y = "Protected Area Network")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white", color="black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(margin=margin(10,0,0,0), size = 15),
        axis.title.y = element_text(margin=margin(0,10,0,0), size = 15),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(colour="black"),
        legend.position="bottom")+
  geom_segment(data=ann_line,aes(x=xmid,xend=xmin,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show.legend=F, size = 1.5)+
  geom_segment(data=ann_line,aes(x=xmid,xend=xmax,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show.legend=F, size = 1.5)+
  geom_text(data=ann_text,aes(x=x,y=y,label=label),size=6,show.legend=F)+
  geom_segment(aes(x = -0.5, y = 0, xend = -0.5, yend = 0.228), linetype=2, size = 1.5,show.legend=F)

# ggarrange(p2030_v2,p2080_v2,labels = c("2030", "2080"),hjust = -12,
#           ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom",
#           font.label = list(size = 20, color = "black", face = "bold", family = NULL))



##### Piechart #####

RCP26 = fread("Output/CSindetails_rcp26_2080.csv")
RCP45 = fread("Output/CSindetails_rcp45_2080.csv")
RCP85 = fread("Output/CSindetails_rcp85_2080.csv")
Area26 = aggregate(RCP26$Area, by=list(Category=RCP26$HowFar_0), FUN=sum)#gives the area for each delta T
Area26$percent <- (Area26$x / sum(Area26$x) )*100
colnames(Area26) <- c("DeltaT", "km2","%km2")
Area45 = aggregate(RCP45$Area, by=list(Category=RCP45$HowFar_0), FUN=sum)
Area45$percent <- (Area45$x / sum(Area45$x) )*100
colnames(Area45) <- c("DeltaT", "km2","%km2")
Area85 = aggregate(RCP85$Area, by=list(Category=RCP85$HowFar_0), FUN=sum)
Area85$percent <- (Area85$x / sum(Area85$x) )*100
colnames(Area85) <- c("DeltaT", "km2","%km2")
Area26$DeltaT #range -3 to 12
Area45$DeltaT #range -4 to 11
Area85$DeltaT #range -6 to 9
Area26 = rbind(Area26, c(-6,0,0),c(-5,0,0), c(-4,0,0))
Area45 = rbind(Area45, c(-6,0,0),c(-5,0,0), c(12,0,0))
Area85 = rbind(Area85, c(10,0,0),c(11,0,0), c(12,0,0))
Area26 = Area26[order(Area26[,1]),]
Area45 = Area45[order(Area45[,1]),]
Area85 = Area85[order(Area85[,1]),]

df2 = data.frame(c(Area26$DeltaT, Area45$DeltaT, Area85$DeltaT),
                 c(Area26$km2, Area45$km2, Area85$km2),
                 c(Area26$`%km2`, Area45$`%km2`, Area85$`%km2`),
                 c(rep("2.6",19),rep("4.5",19),rep("8.5",19)))

colnames(df2) <- c("DeltaT","km2","per_km2","RCP")

# Convert factor variable
df2$DeltaT <- as.factor(df2$DeltaT)
df2$RCP <- as.factor(df2$RCP)


library(RColorBrewer)
library(patternplot)
# ALL RCP
#-6/-5/-4/-3/-2/-1 = FAILURE
#0/1/2/3/4/5/6/7/8/9/10/11/12 = SUCCESS

mycolors <- c("orangered4","red4","red3","red2","red1", "orangered2",
              "yellow","lightblue",
              "lightblue1","skyblue","skyblue1","skyblue2",
              "steelblue1", "steelblue2","steelblue3","blue","blue1","blue2","blue3",
              "black", "grey")
#pie
ggplot(df2, aes(x=2, y=per_km2, fill=DeltaT)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = mycolors)+
  coord_polar("y", start=0) + facet_wrap(~ RCP) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(fill="Delta T", x="Without Corridor", y="")

#donut
ggplot(df2, aes(x=2, y=per_km2, fill=DeltaT)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = mycolors)+
  coord_polar("y", start=0) + facet_wrap(~ RCP) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(fill="Delta T", x="2080", 
       y="Without Corridor", 
       title="Details of Climatic Success", 
       caption="blabla")+
  xlim(0.5, 2.5)


library(RColorBrewer)
library(patternplot)
# ALL RCP
#-6/-5/-4/-3/-2/-1 = FAILURE
#0/1/2/3/4/5/6/7/8/9/10/11/12 = SUCCESS
mycolors <- c("orangered4","orangered3","orangered2","orangered1","darkorange", "orange",
              "white","lightcyan1","lightblue1","skyblue","skyblue1","skyblue2",
              "steelblue1", "steelblue2","steelblue3","blue","blue1","blue2","blue3")

mycolors <- c("orangered4","red4","red3","red2","red1", "orangered2",
              "yellow","lightblue",
              "lightblue1","skyblue","skyblue1","skyblue2",
              "steelblue1", "steelblue2","steelblue3","blue","blue1","blue2","blue3",
              "black", "grey")
#pie
ggplot(df2, aes(x=2, y=per_km2, fill=DeltaT)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = mycolors)+
  coord_polar("y", start=0) + facet_wrap(~ RCP) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(fill="Delta T", x="With Corridor", y="",
       title="Details of Climatic Success - 2080")

#donut
ggplot(df2, aes(x=2, y=per_km2, fill=DeltaT)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = mycolors)+
  coord_polar("y", start=0) + facet_wrap(~ RCP) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(fill="Delta T", x="2080", 
       y="Without Corridor", 
       title="Details of Climatic Success", 
       caption="blabla")+
  xlim(0.5, 2.5)


####### *** Fig 1: Gain in CCS in 10 years **** ######
##### Bar chart #####
gain26 = c(13,13,13,13,13,13)
gain45 = c(13,13,13,13,12,12)
gain85 = c(13,13,15,12,8,8)
years = c(2030,2040,2050,2060,2070,2080)

Gain = c(gain26, gain45, gain85)
Years = c(rep(years,3))
Scenario = c(rep(26,6), rep(45,6), rep(85,6))

data.gain = as.data.frame(cbind(Years, Gain, Scenario))
data.gain$Scenario = as.factor(data.gain$Scenario)

ggplot(data.gain, aes(x=Years, y=Gain, fill = Scenario))+
  geom_bar(stat="identity", position=position_dodge(), color = "black")+
  theme_minimal() + 
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'), labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5")) +
  scale_x_continuous("Year", breaks=c(2030,2040,2050,2060,2070,2080)) +
  scale_y_continuous("% of climatically successful PA")


####### *** Fig 1: Maps of Improvements in CCS  **** ######
setwd("C:/Users/charlotp/Documents/propagation/data_all_Austria/mean_500m/Output")
library(data.table)
Cor_CCS_2080_26 <- fread("AllData_rcp26_2080_COR.csv")
Adj_CCS_2080_26 <- fread("AllData_rcp26_2080.csv")

Cor_CCS_2080_45 <- fread("AllData_rcp45_2080_COR.csv")
Adj_CCS_2080_45 <- fread("AllData_rcp45_2080.csv")

Cor_CCS_2080_85 <- fread("AllData_rcp85_2080_COR.csv")
Adj_CCS_2080_85 <- fread("AllData_rcp85_2080.csv")

#Improvements due to corridors in 2080 for rcp26
imp <- Cor_CCS_2080_26[,c(2,16)]
imp <-merge(imp, Adj_CCS_2080_26, by ="Cores", all.x = T)
imp <- imp[,c(1,2,19)]
imp$success_Cor[which(imp$HowFar_100 >= 0)] <- 1
imp$success_Cor[which(imp$HowFar_100 < 0)] <- 0
imp$success_Adj[which(imp$HowFar_0 >= 0)] <- 1
imp$success_Adj[which(imp$HowFar_0 < 0)] <- 0
imp$success_Adj[which(is.na(imp$success_Adj))] <- 0
imp$improvement <- imp$success_Cor + imp$success_Adj #2: ALWAYS successful; 1: ONLY with corridors; 0: NEVER
write.csv(imp, file="Figures/imp2080_26.csv")

#Improvements due to corridors in 2080 for rcp45
imp <- Cor_CCS_2080_45[,c(2,16)]
imp <-merge(imp, Adj_CCS_2080_45, by ="Cores", all.x = T)
imp <- imp[,c(1,2,19)]
imp$success_Cor[which(imp$HowFar_100 >= 0)] <- 1
imp$success_Cor[which(imp$HowFar_100 < 0)] <- 0
imp$success_Adj[which(imp$HowFar_0 >= 0)] <- 1
imp$success_Adj[which(imp$HowFar_0 < 0)] <- 0
imp$success_Adj[which(is.na(imp$success_Adj))] <- 0
imp$improvement <- imp$success_Cor + imp$success_Adj #2: ALWAYS successful; 1: ONLY with corridors; 0: NEVER
write.csv(imp, file="Figures/imp2080_45.csv")

#Improvements due to corridors in 2080 for rcp85
imp <- Cor_CCS_2080_85[,c(2,16)]
imp <-merge(imp, Adj_CCS_2080_85, by ="Cores", all.x = T)
imp <- imp[,c(1,2,19)]
imp$success_Cor[which(imp$HowFar_100 >= 0)] <- 1
imp$success_Cor[which(imp$HowFar_100 < 0)] <- 0
imp$success_Adj[which(imp$HowFar_0 >= 0)] <- 1
imp$success_Adj[which(imp$HowFar_0 < 0)] <- 0
imp$success_Adj[which(is.na(imp$success_Adj))] <- 0
imp$improvement <- imp$success_Cor + imp$success_Adj #2: ALWAYS successful; 1: ONLY with corridors; 0: NEVER
write.csv(imp, file="Figures/imp2080_85.csv")


####### *** Maps of Improvements in Endemic Species  **** ####### 
setwd("C:/Users/charlotp/Documents/Metrics/mapping")
Cor_end_2080_26 <- fread("metricPA3_withCor_26_2080.csv")
Adj_end_2080_26 <- fread("metricPA1_withoutCor_26_2080.csv")

Cor_end_2080_45 <- fread("metricPA3_withCor_45_2080.csv")
Adj_end_2080_45 <- fread("metricPA1_withoutCor_45_2080.csv")

Cor_end_2080_85 <- fread("metricPA3_withCor_85_2080.csv")
Adj_end_2080_85 <- fread("metricPA1_withoutCor_85_2080.csv")

#Improvements due to corridors in 2080 for rcp26
imp <- Cor_end_2080_26[,c(2,7:9)]
imp <-merge(imp, Adj_end_2080_26, by ="PA_ID", all.x = T)
imp <- imp[,c(1:4,11:12)]
names(imp) <- c("PA_ID","AllEndSp","SucEndSp_Cor","Percent_SucEndSp_Cor","SucEndSp_Adj","Percent_SucEndSp_Adj")
imp$improvement <- imp$Percent_SucEndSp_Cor - imp$Percent_SucEndSp_Adj
write.csv(imp, file="imp_endemicspecies_2080_26.csv")

#Improvements due to corridors in 2080 for rcp45
imp <- Cor_end_2080_45[,c(2,7:9)]
imp <-merge(imp, Adj_end_2080_45, by ="PA_ID", all.x = T)
imp <- imp[,c(1:4,11:12)]
names(imp) <- c("PA_ID","AllEndSp","SucEndSp_Cor","Percent_SucEndSp_Cor","SucEndSp_Adj","Percent_SucEndSp_Adj")
imp$improvement <- imp$Percent_SucEndSp_Cor - imp$Percent_SucEndSp_Adj
write.csv(imp, file="imp_endemicspecies_2080_45.csv")

#Improvements due to corridors in 2080 for rcp85
imp <- Cor_end_2080_85[,c(2,7:9)]
imp <-merge(imp, Adj_end_2080_85, by ="PA_ID", all.x = T)
imp <- imp[,c(1:4,11:12)]
names(imp) <- c("PA_ID","AllEndSp","SucEndSp_Cor","Percent_SucEndSp_Cor","SucEndSp_Adj","Percent_SucEndSp_Adj")
imp$improvement <- imp$Percent_SucEndSp_Cor - imp$Percent_SucEndSp_Adj
write.csv(imp, file="imp_endemicspecies_2080_85.csv")


####### *** Fig 4: Improvements in Endemic Species  **** #######
##### Boxplot #####
setwd("C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/Outputs")
patches.dt <- fread("C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/Inputs/core_PA.csv")
test <-aggregate(Shape_Area ~ PA_ID, patches.dt, function(x) sum(x))
test$Shape_Area <- test$Shape_Area / 1000000

TOTAL_AREA <- sum(test$Shape_Area)

# Corridors
mean_Cor <- data.frame()
abs_Cor <- data.frame()
tot_Cor <- data.frame()
nbr_Cor <- data.frame()
ALL <- data.frame()
ALL_Cor2 <- data.frame()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    
    # year = 2030
    # scenario = 26
    
    
    path <- paste("metricPA3_withCor_",scenario,"_",year,".csv", sep="")
    name <- paste("Cor_",scenario,"_",year,sep="") # "Cor_26_2030
    tab <- assign(name,fread(path))
    tab.m <- merge(test, tab, by="PA_ID")
    tab.m <- tab.m[,c(1:2,9:10)] #PA_ID, Area, nbtotSUC,percentSUC
    
    hist(tab.m$allspSUC, breaks = c(seq(0,120,1)))
    # hist(tab.m$allspSUC, breaks = c(seq(0,120,1)), freq = F)
    # hist(tab.m$allspSUC, breaks = c(seq(0,120,10)), freq = F)
    # 
    # hist(tab.m$percentSUC, breaks = c(seq(0,100,10)))
    # hist(tab.m$percentSUC, breaks = c(seq(0,100,1)), freq = F)
    
    cat0 <- tab.m[which(tab.m$percentSUC == 0),2]
    cat1 <- tab.m[which(tab.m$percentSUC <= 25 & tab.m$percentSUC > 0),2]
    cat2 <- tab.m[which(tab.m$percentSUC <= 50 & tab.m$percentSUC > 25),2]
    cat3 <- tab.m[which(tab.m$percentSUC <= 75 & tab.m$percentSUC > 50),2]
    cat4 <- tab.m[which(tab.m$percentSUC <= 99 & tab.m$percentSUC > 50),2]
    cat5 <- tab.m[which(tab.m$percentSUC > 99),2]
    
    area0 <- sum(cat0) ; nb0 <- length(cat0) ; Parea0 <- (area0/TOTAL_AREA)*100
    area1 <- sum(cat1) ; nb1 <- length(cat1) ; Parea1 <- (area1/TOTAL_AREA)*100
    area2 <- sum(cat2) ; nb2 <- length(cat2) ; Parea2 <- (area2/TOTAL_AREA)*100
    area3 <- sum(cat3) ; nb3 <- length(cat3) ; Parea3 <- (area3/TOTAL_AREA)*100
    area4 <- sum(cat4) ; nb4 <- length(cat4) ; Parea4 <- (area4/TOTAL_AREA)*100
    area5 <- sum(cat5) ; nb5 <- length(cat5) ; Parea5 <- (area5/TOTAL_AREA)*100
    
    cat0a <- tab.m[which(tab.m$allspSUC == 0),2]
    cat1a <- tab.m[which(tab.m$allspSUC == 1),2]
    cat2a <- tab.m[which(tab.m$allspSUC <= 25 & tab.m$allspSUC > 1),2]
    cat3a <- tab.m[which(tab.m$allspSUC <= 50 & tab.m$allspSUC > 25),2]
    cat4a <- tab.m[which(tab.m$allspSUC <= 100 & tab.m$allspSUC > 50),2]
    cat5a <- tab.m[which(tab.m$allspSUC > 100),2]
    
    area0a <- sum(cat0a) ; nb0a <- length(cat0a) ; Parea0a <- (area0a/TOTAL_AREA)*100
    area1a <- sum(cat1a) ; nb1a <- length(cat1a) ; Parea1a <- (area1a/TOTAL_AREA)*100
    area2a <- sum(cat2a) ; nb2a <- length(cat2a) ; Parea2a <- (area2a/TOTAL_AREA)*100
    area3a <- sum(cat3a) ; nb3a <- length(cat3a) ; Parea3a <- (area3a/TOTAL_AREA)*100
    area4a <- sum(cat4a) ; nb4a <- length(cat4a) ; Parea4a <- (area4a/TOTAL_AREA)*100
    area5a <- sum(cat5a) ; nb5a <- length(cat5a) ; Parea5a <- (area5a/TOTAL_AREA)*100
    
    dta <- data.frame(c("0","1","2_25","26_50","51_100",">100"),c(nb0a,nb1a,nb2a,nb3a,nb4a,nb5a),
                     c(area0a,area1a,area2a,area3a,area4a,area5a), c(Parea0a,Parea1a,Parea2a,Parea3a,Parea4a,Parea5a),
                     c(rep(year,6)), c(rep(scenario,6)))
    names(dta) <- c("absolute_endsp","nb_PAs","area","percent_totarea","year","scenario")
    abs_Cor <- rbind(abs_Cor, dta)
    
    dt <- data.frame(c("0","1_25","25_50","50_75","75_99","100"),c(nb0,nb1,nb2,nb3,nb4, nb5),
                     c(area0,area1,area2,area3,area4, area5), c(Parea0,Parea1,Parea2,Parea3,Parea4, Parea5),
                     c(rep(year,6)), c(rep(scenario,6)))
    names(dt) <- c("percent_endsp","nb_PAs","area","percent_totarea","year","scenario")
    mean_Cor <- rbind(mean_Cor, dt)
    
    meanTot <- mean(tab.m$percentSUC); sdTot <- sd(tab.m$percentSUC);medTot <- median(tab.m$percentSUC)
    df <- data.frame(meanTot,sdTot,medTot,year,scenario)
    names(df) <- c("mean","sd","median","year","scenario")
    tot_Cor <- rbind(tot_Cor, df)
    
    meannbr <- mean(tab.m$allspSUC); sdnbr <- sd(tab.m$allspSUC);mednbr <- median(tab.m$allspSUC)
    df2 <- data.frame(meannbr,sdnbr,mednbr,year,scenario)
    names(df2) <- c("mean","sd","median","year","scenario")
    nbr_Cor <- rbind(nbr_Cor, df2)
    
    all <- data.frame(tab.m$PA_ID,tab.m$allspSUC,c(rep(year,854)), c(rep(scenario,854)),c(rep("Cor",854)))
    names(all) <- c("PA_ID","spSUC","year","scenario","condition")
    ALL <- rbind(ALL, all)
    
    all2 <- data.frame(tab.m$PA_ID,tab.m$percentSUC,c(rep(year,854)), c(rep(scenario,854)),c(rep("Cor",854)))
    names(all2) <- c("PA_ID","perSUC","year","scenario","condition")
    ALL_Cor2 <- rbind(ALL_Cor2, all2)
  }}

# adjacency
mean_Adj <- data.frame()
abs_Adj <- data.frame()
tot_Adj <- data.frame()
nbr_Adj <- data.frame()
ALL_Adj <- data.frame()
ALL_Adj2 <- data.frame()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    path <- paste("metricPA1_withoutCor_",scenario,"_",year,".csv", sep="")
    name <- paste("Adj_",scenario,"_",year,sep="") # "Cor_26_2030
    tab <- assign(name,fread(path))
    tab.m <- merge(test, tab, by="PA_ID")
    tab.m <- tab.m[,c(1:2,9:10)] #PA_ID, Area, percentSUC
    
    cat0 <- tab.m[which(tab.m$percentSUC == 0),2]
    cat1 <- tab.m[which(tab.m$percentSUC <= 25 & tab.m$percentSUC > 0),2]
    cat2 <- tab.m[which(tab.m$percentSUC <= 50 & tab.m$percentSUC > 25),2]
    cat3 <- tab.m[which(tab.m$percentSUC <= 75 & tab.m$percentSUC > 50),2]
    cat4 <- tab.m[which(tab.m$percentSUC <= 99 & tab.m$percentSUC > 75),2]
    cat5 <- tab.m[which( tab.m$percentSUC > 99),2]
    
    area0 <- sum(cat0) ; nb0 <- length(cat0) ; Parea0 <- (area0/TOTAL_AREA)*100
    area1 <- sum(cat1) ; nb1 <- length(cat1) ; Parea1 <- (area1/TOTAL_AREA)*100
    area2 <- sum(cat2) ; nb2 <- length(cat2) ; Parea2 <- (area2/TOTAL_AREA)*100
    area3 <- sum(cat3) ; nb3 <- length(cat3) ; Parea3 <- (area3/TOTAL_AREA)*100
    area4 <- sum(cat4) ; nb4 <- length(cat4) ; Parea4 <- (area4/TOTAL_AREA)*100
    area5 <- sum(cat5) ; nb5 <- length(cat5) ; Parea5 <- (area5/TOTAL_AREA)*100
    
    dt <- data.frame(c("0","0_25","25_50","50_75","75_99","100"),c(nb0,nb1,nb2,nb3,nb4,nb5),
                     c(area0,area1,area2,area3,area4,area5), c(Parea0,Parea1,Parea2,Parea3,Parea4,Parea5),
                     c(rep(year,6)), c(rep(scenario,6)))
    names(dt) <- c("percent_endsp","nb_PAs","area","percent_totarea","year","scenario")
    mean_Adj <- rbind(mean_Adj, dt)
    
    cat0a <- tab.m[which(tab.m$allspSUC == 0),2]
    cat1a <- tab.m[which(tab.m$allspSUC == 1),2]
    cat2a <- tab.m[which(tab.m$allspSUC <= 25 & tab.m$allspSUC > 1),2]
    cat3a <- tab.m[which(tab.m$allspSUC <= 50 & tab.m$allspSUC > 25),2]
    cat4a <- tab.m[which(tab.m$allspSUC <= 100 & tab.m$allspSUC > 50),2]
    cat5a <- tab.m[which(tab.m$allspSUC > 100),2]
    
    area0a <- sum(cat0a) ; nb0a <- length(cat0a) ; Parea0a <- (area0a/TOTAL_AREA)*100
    area1a <- sum(cat1a) ; nb1a <- length(cat1a) ; Parea1a <- (area1a/TOTAL_AREA)*100
    area2a <- sum(cat3a) ; nb2a <- length(cat2a) ; Parea2a <- (area2a/TOTAL_AREA)*100
    area3a <- sum(cat3a) ; nb3a <- length(cat3a) ; Parea3a <- (area3a/TOTAL_AREA)*100
    area4a <- sum(cat4a) ; nb4a <- length(cat4a) ; Parea4a <- (area4a/TOTAL_AREA)*100
    area5a <- sum(cat5a) ; nb5a <- length(cat5a) ; Parea5a <- (area5a/TOTAL_AREA)*100

    
    dta <- data.frame(c("0","1","2_25","26_50","51_100",">100"),c(nb0a,nb1a,nb2a,nb3a,nb4a,nb5a),
                      c(area0a,area1a,area2a,area3a,area4a,area5a), c(Parea0a,Parea1a,Parea2a,Parea3a,Parea4a,Parea5a),
                      c(rep(year,6)), c(rep(scenario,6)))
    names(dta) <- c("absolute_endsp","nb_PAs","area","percent_totarea","year","scenario")
    abs_Adj <- rbind(abs_Adj, dta)
    
    meanTot <- mean(tab.m$percentSUC); sdTot <- sd(tab.m$percentSUC);medTot <- median(tab.m$percentSUC)
    df <- data.frame(meanTot,sdTot,medTot,year,scenario)
    names(df) <- c("mean","sd","median","year","scenario")
    tot_Adj <- rbind(tot_Adj, df)
    
    meannbr <- mean(tab.m$allspSUC); sdnbr <- sd(tab.m$allspSUC);mednbr <- median(tab.m$allspSUC)
    df2 <- data.frame(meannbr,sdnbr,mednbr,year,scenario)
    names(df2) <- c("mean","sd","median","year","scenario")
    nbr_Adj <- rbind(nbr_Adj, df2)
    
    all <- data.frame(tab.m$PA_ID,tab.m$allspSUC,c(rep(year,854)), c(rep(scenario,854)),c(rep("Adj",854)))
    names(all) <- c("PA_ID","spSUC","year","scenario","condition")
    ALL_Adj <- rbind(ALL_Adj, all)
    
    all2 <- data.frame(tab.m$PA_ID,tab.m$percentSUC,c(rep(year,854)), c(rep(scenario,854)),c(rep("Adj",854)))
    names(all2) <- c("PA_ID","perSUC","year","scenario","condition")
    ALL_Adj2 <- rbind(ALL_Adj2, all2)
  }}


# the percent of endemic sp suc by PA = 
# tot number of different end sp present in patches that achieve CCS / tot number of different end sp present in the PA
# Then, we average this percentage across all PAs

# Meaning =
# On average, for 1 PA, 
# the addition of corridors allows an additional 30% to 40% of species to be able to reach a suitable temperature 

# But this average has a huge standard deviation, as in reality, 
# either the PA still has 0% of suc species with corridors or get 100%



# Meaning =
# On average, for 1 PA, 
# the addition of corridors allows 2 to 3 additional species to reach an suitable temperature

# But this average has a huge standard deviation (sd= 9 to 12), as in reality, 
# PA get either 0 species more or 10



############################### absolute number
ALL_Gain <- merge(ALL_Adj, ALL, by=c("PA_ID","year","scenario"))

ALL_Gain$Gain <- ALL_Gain$spSUC.y - ALL_Gain$spSUC.x
ALL_Gain$scenario <- as.factor(ALL_Gain$scenario)
ALL_Gain$scenario <- factor(ALL_Gain$scenario, levels=c("26","45","85"),
                            labels = c("RCP 2.6","RCP 4.5","RCP 8.5"))
ALL_Gain <- ALL_Gain[-which(ALL_Gain$Gain < 0),]

save(ALL_Gain, file="C:/Users/perle/OneDrive/Bureau/RDATA/Metrics/gain.RDATA")

ggplot(ALL_Gain, aes(x=year, y=Gain, group=year,fill=scenario)) +
  geom_boxplot(notch=F,outlier.alpha = 0.1, outlier.size = 1)+
  scale_fill_manual(values=c('white','grey','grey40'))+
  theme(legend.position = "none", panel.grid.major = element_line(colour = "grey", linetype=2),
        panel.grid.minor=element_line(colour = "grey", linetype=2), axis.title.y = element_text(margin=margin(0,10,0,0)),
        legend.direction = "horizontal",panel.background =element_rect(fill="white", color="black"),
        axis.text.x = element_text(angle = 45,hjust = 1)) + 
  scale_y_continuous(breaks = seq(0,50,10))+
  facet_grid(cols = vars(scenario))+  scale_x_discrete(limits=c(2030,2040,2050,2060,2070,2080))+
  labs( x="Year", y="Number of endemic species climatically successful per PA")

ggsave("C:/Users/charlotp/Desktop/figures/figure4.png", width =20, height =20, units = "cm")
############################### percent
ALL_Gain2 <- merge(ALL_Adj2, ALL_Cor2, by=c("PA_ID","year","scenario"))
ALL_Gain2$Gain <- ALL_Gain2$perSUC.y - ALL_Gain2$perSUC.x
ALL_Gain2$scenario <- as.factor(ALL_Gain2$scenario)
ALL_Gain2$scenario <- factor(ALL_Gain2$scenario, levels=c("26","45","85"),
                            labels = c("RCP 2.6","RCP 4.5","RCP 8.5"))
ALL_Gain2 <- ALL_Gain2[-which(ALL_Gain2$Gain < 0),]

save(ALL_Gain, file="C:/Users/charlotp/Desktop/figures/fig4.RDATA")

ggplot(ALL_Gain2, aes(x=year, y=Gain, group=year,fill=scenario)) +
  geom_boxplot(notch=F,outlier.alpha = 0.1, outlier.size = 1)+
  scale_fill_manual(values=c('white','grey','grey40'))+
  theme(legend.position = "none", panel.grid.major = element_line(colour = "grey", linetype=2),
        panel.grid.minor=element_line(colour = "grey", linetype=2), axis.title.y = element_text(margin=margin(0,10,0,0)),
        legend.direction = "horizontal",panel.background =element_rect(fill="white", color="black"),
        axis.text.x = element_text(angle = 45,hjust = 1)) + 
  scale_y_continuous(breaks = seq(0,100,10))+
  facet_grid(cols = vars(scenario))+  scale_x_discrete(limits=c(2030,2040,2050,2060,2070,2080))+
  labs( x="Year", y="Percent of endemic species climatically successful per PA")

ggsave("C:/Users/charlotp/Desktop/figures/figure4.png", width =20, height =20, units = "cm")

####### Fig 5: %end sp suc ADJ VS %end sp suc COR, by size ######
## in 2080, rcp 26 and rcp 85

#save(Adj_26_2030,Adj_45_2030,Adj_85_2030, Cor_26_2030,Cor_45_2030, Cor_85_2030, 
#     Adj_26_2080,Adj_45_2080,Adj_85_2080, Cor_26_2080,Cor_45_2080, Cor_85_2080, 
#     test,
#     file="C:/Users/charlotp/Documents/Metrics/Outputs/datafig5.RDATA")


#load("C:/Users/charlotp/Documents/Metrics/Outputs/PA_area.RDATA")
load("C:/Users/charlotp/Documents/Metrics/Outputs/datafig5.RDATA")
#Create size catgeories
# First method: quantiles from data
test <- fread(file="C:/Users/charlotp/Documents/Metrics/Inputs/core_PA.csv")
test$LogArea <- log(test$Shape_Area)
hist(test$LogArea)
a<-summary(test$LogArea) 
q1 <- as.numeric(a[2]) #1st quartile = 10.6
q2 <- as.numeric(a[3]) #median = 12.3
q3 <- as.numeric(a[5]) #3rd quartile = 14.1

test$category <- 0
test$category[which(test$LogArea <= q1)] <- "VS" #522 VERY SMALL
test$category[which(test$LogArea <= q2 & test$LogArea > q1)] <- "S" # 406 SMALL
test$category[which(test$LogArea <= q3 & test$LogArea > q2)] <- "M" # 459 MEDIUM
test$category[which(test$LogArea > q3)] <- "L" # 462 LARGE

# Second method: quantiles from scale
ecart <- max(test$LogArea) - min(test$LogArea)
q1bis <- min(test$LogArea) + ecart * 0.25 # 12
q2bis <- min(test$LogArea) + ecart * 0.5 #14.8
q3bis <- min(test$LogArea) + ecart * 0.75 #17.6

test$categorybis <- 0
test$categorybis[which(test$LogArea <= q1bis)] <- "VS" #928 VERY SMALL
test$categorybis[which(test$LogArea <= q2bis & test$LogArea > q1bis)] <- "S" # 649 SMALL
test$categorybis[which(test$LogArea <= q3bis & test$LogArea > q2bis)] <- "M" # 211 MEDIUM
test$categorybis[which(test$LogArea > q3bis)] <- "L" # 61 LARGE

## Prepare data
# 2030
# RCP 26
tab1A = merge(test,Adj_26_2030, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_26_2030, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2030_26 <- tab1[,-c(8:11)]
tab2030_26$RCP <- c(rep(26,length(tab2030_26$PA_ID)))
tab2030_26$year <- c(rep(2030,length(tab2030_26$PA_ID)))
names(tab2030_26) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")
# RCP 45 
tab1A = merge(test,Adj_45_2030, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_45_2030, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2030_45 <- tab1[,-c(8:11)]
tab2030_45$RCP <- c(rep(45,length(tab2030_45$PA_ID)))
tab2030_45$year <- c(rep(2030,length(tab2030_45$PA_ID)))
names(tab2030_45) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")
# RCP 85 
tab1A = merge(test,Adj_85_2030, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_85_2030, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2030_85 <- tab1[,-c(8:11)]
tab2030_85$RCP <- c(rep(85,length(tab2030_85$PA_ID)))
tab2030_85$year <- c(rep(2030,length(tab2030_85$PA_ID)))
names(tab2030_85) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")

# 2080
# RCP 26
tab1A = merge(test,Adj_26_2080, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_26_2080, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2080_26 <- tab1[,-c(8:11)]
tab2080_26$RCP <- c(rep(26,length(tab2080_26$PA_ID)))
tab2080_26$year <- c(rep(2080,length(tab2080_26$PA_ID)))
names(tab2080_26) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")
# RCP 45 
tab1A = merge(test,Adj_45_2080, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_45_2080, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2080_45 <- tab1[,-c(8:11)]
tab2080_45$RCP <- c(rep(45,length(tab2080_45$PA_ID)))
tab2080_45$year <- c(rep(2080,length(tab2080_45$PA_ID)))
names(tab2080_45) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")
# RCP 85 
tab1A = merge(test,Adj_85_2080, by="PA_ID")
tab1A <- tab1A[,c(1:5,12,13)]
tab1C = merge(test,Cor_85_2080, by="PA_ID")
tab1C <- tab1C[,c(1:5,12,13)]
tab1 <- merge(tab1A,tab1C,by="PA_ID")
tab2080_85 <- tab1[,-c(8:11)]
tab2080_85$RCP <- c(rep(85,length(tab2080_85$PA_ID)))
tab2080_85$year <- c(rep(2080,length(tab2080_85$PA_ID)))
names(tab2080_85) <- c("PA_ID","Area","LogArea","category","categorybis","allspSucAdj","PerSucAdj","allspSucCor","PerSucCor","RCP","Year")
#Merge all
EndspbySize <- data.frame(rbind(tab2030_26,tab2030_45,tab2030_85,tab2080_26,tab2080_45,tab2080_85))
head(EndspbySize)
EndspbySize <- EndspbySize[-which(EndspbySize$PerSucAdj > EndspbySize$PerSucCor),] 

#Rename column to fit in the plot
EndspbySize$category <- factor(EndspbySize$category, levels = c("L","M","S","VS"),
                                  labels = c("Large","Medium","Small","Very Small"))
EndspbySize$categorybis <- factor(EndspbySize$categorybis, levels = c("L","M","S","VS"),
                                  labels = c("Large","Medium","Small","Very Small"))
EndspbySize$RCP <- factor(EndspbySize$RCP, levels = c("26","45","85"),
                  labels = c("RCP 2.6","RCP 4.5","RCP 8.5"))

#Drawing Plots: attempts with different color scales
#first method (quantiles based on data distribution)
ggplot(EndspbySize, aes(x=PerSucAdj, y=PerSucCor))+
  geom_abline(intercept = 0, slope = 1)+
  geom_count(aes(col=category))+
  facet_grid(Year~RCP)
#Second method (quantiles based on scale)
ggplot(EndspbySize, aes(x=PerSucAdj, y=PerSucCor))+
  geom_abline(intercept = 0, slope = 1)+
  geom_count(aes(col=categorybis))+
  facet_grid(Year~RCP)
# continuous scale 1
ggplot(EndspbySize, aes(x=PerSucAdj, y=PerSucCor, color=LogArea))+
  geom_abline(intercept = 0, slope = 1)+
  geom_count()+
  facet_grid(Year~RCP)
# continuous scale 2
ggplot(EndspbySize, aes(x=PerSucAdj, y=PerSucCor, color=Area))+
  geom_abline(intercept = 0, slope = 1)+
  geom_count()+
  facet_grid(Year~RCP)+
  scale_color_gradientn(colours = terrain.colors(4))

#triangle <- with(tab1, data.frame(x=c(0, 100, 100), y=c(0, 0, 100)))
#ggplot(tab1, aes(x=PerSucAdj, y=PerSucCor)) +
#  geom_polygon(data=triangle, aes(x, y), fill="#d8161688") +
#  geom_count()
# ggarrange(g1, g2, labels = c("RCP 2.6", "RCP 8.5"), common.legend = TRUE, legend = "right", hjust=-4)

#Final plot
ggplot(EndspbySize, aes(x=PerSucAdj, y=PerSucCor))+
  geom_abline(intercept = 0, slope = 1, size = 0.3)+
  geom_count(aes(col=categorybis))+
  theme(panel.background =element_rect(fill="white", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        strip.background = element_rect(colour="black"),
        legend.position = c(0.9,0.12),
        legend.direction = "vertical",
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.text =element_text(size=10),
        legend.title = element_text(size=11),
        legend.box="horizontal",
        legend.title.align = 0.5,
        axis.title.y = element_text(margin=margin(0,10,0,0), size=15),
        axis.title.x = element_text(margin=margin(10,0,0,0),size=15),
        axis.text = element_text(size=12))+
  scale_y_continuous(limits = c(0,100) ,breaks = seq(0,100,25))+
  labs(x="Percentage of endemic species achieving climate connectivity per PA \n Adjacency only",
       y="Percentage of endemic species achieving climate connectivity per PA \n Corridors",
       col="PA Size \nCategory")+
  scale_size(name="Number\nof PA", breaks = c(10,50,100,200))+
  facet_grid(Year~RCP)
  
#guides(col=guide_legend(ncol = 2))

ggsave("C:/Users/charlotp/Desktop/figures/figure5.png", width =30, height =20, units = "cm", dpi = 500)
# categories : 
# VS = very small = 0 to 0.1910261 (=exp(q1bis)) km2, corresponding to 928 PA
# S = small = 0.1910261 to 3.649096 (=exp(q2bis)) km2, corresponding to 649 PA
# M = medium = 3.649096 to 69.70724 (=exp(q3bis)) km2, corresponding to 211 PA
# L = large = >69.70724 km2 , corresponding to 61 PA

# useless to have 2 RCP ?


################################ true figures ##########################


### figure 5: PA size and improvements in endemic species due to corridors
load("C:/Users/charlotp/Desktop/figures/figure5.RDATA")
EndspbySize <- EndspbySize[-which(EndspbySize$PerSucAdj > EndspbySize$PerSucCor),]
ggplot(EndspbySize, aes(x=allspSucAdj, y=allspSucCor))+
  geom_abline(intercept = 0, slope = 1, size = 0.3)+
  geom_count(aes(col=categorybis))+
  theme(panel.background =element_rect(fill="white", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        strip.background = element_rect(colour="black"),
        legend.position = c(0.9,0.12),
        legend.direction = "vertical",
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.text =element_text(size=10),
        legend.title = element_text(size=11),
        legend.box="horizontal",
        legend.title.align = 0.5,
        strip.text = element_text(size = 20),
        axis.title.y = element_text(margin=margin(0,10,0,0), size=15),
        axis.title.x = element_text(margin=margin(10,0,0,0),size=15),
        axis.text = element_text(size=12))+
  labs(x="Number of endemic species achieving climate connectivity per PA \n Adjacency only",
       y="Number of endemic species achieving climate connectivity per PA \n Corridors",
       col="PA Size \nCategory")+
  scale_size(name="Number\nof PA", breaks = c(10,50,100,200))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  facet_grid(Year~RCP)

ggsave("C:/Users/charlotp/Desktop/figures/figure5_number.png", width =30, height =20, units = "cm", dpi=500)

scale_y_continuous(limits = c(0,100) ,breaks = seq(0,100,25))+
### figure 4: Improvements in endemic species due to corridors
load("C:/Users/charlotp/Desktop/figures/fig4.RDATA")
ggplot(ALL_Gain, aes(x=year, y=Gain, group=year,fill=scenario)) +
  geom_boxplot(notch=F,outlier.alpha = 0.1, outlier.size = 1)+
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'))+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        strip.text = element_text(size =15),
        panel.grid.minor=element_blank(), 
        axis.title.y = element_text(margin=margin(0,10,0,0), size = 15),
        axis.title.x = element_text(margin=margin(10,0,0,0), size = 15),
        legend.direction = "horizontal",
        panel.background =element_rect(fill="white", color="black"),
        strip.background = element_rect(colour="black"),
        axis.text.x = element_text(angle = 45,hjust = 1,size = 10)) + 
  scale_y_continuous(breaks = seq(0,50,10))+
  facet_grid(cols = vars(scenario))+  scale_x_discrete(limits=c(2030,2040,2050,2060,2070,2080))+
  labs( x="Year", y="Number of endemic species achieving climate connectivity per PA")

ggplot(ALL_Gain2, aes(x=year, y=Gain, group=year,fill=scenario)) +
  geom_boxplot(notch=F,outlier.alpha = 0.1, outlier.size = 1)+
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'))+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        strip.text = element_text(size =15),
        panel.grid.minor=element_blank(), 
        axis.title.y = element_text(margin=margin(0,10,0,0), size = 15),
        axis.title.x = element_text(margin=margin(10,0,0,0), size = 15),
        legend.direction = "horizontal",
        panel.background =element_rect(fill="white", color="black"),
        strip.background = element_rect(colour="black"),
        axis.text.x = element_text(angle = 45,hjust = 1,size = 10)) + 
  scale_y_continuous(breaks = seq(0,200,10))+
  facet_grid(cols = vars(scenario))+  scale_x_discrete(limits=c(2030,2040,2050,2060,2070,2080))+
  labs( x="Year", y="Percentage of endemic species \n achieving climate connectivity per PA")


### figure 2: Margin of success or failure
load(file="C:/Users/charlotp/Desktop/figures/data_figure2.RDATA")
ggplot(tentative,aes(x=HowFar_Area)) + 
  geom_histogram(aes(y = ..density..,  fill = RCP), color = "black",size=0.3,
                 alpha = 0.3, position = "identity",binwidth = 1) + 
  geom_density(aes(color = RCP), size =1, adjust=2)+
  facet_grid(condition~year,scales="free",space="free")+
  scale_fill_manual(values=c('skyblue','palegreen3','tomato'))+
  scale_colour_manual( values = c('skyblue','palegreen3','tomato'))+
  scale_x_continuous(limits=c(-6,12), breaks = seq(-6,12,1))+
  scale_y_continuous(labels=scales::percent)+
  labs(x= "Margin of success or failure (°C)", y = "Protected Area Network")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white", color="black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(margin=margin(10,0,0,0), size = 15),
        axis.title.y = element_text(margin=margin(0,10,0,0), size = 15),
        axis.ticks.x = element_blank(),
        legend.text =element_text(size=10),
        legend.title = element_text(size=11),
        strip.text = element_text(size = 20),
        strip.background = element_rect(colour="black"),
        legend.position=c(0.9,0.9),
        legend.direction="vertical")+
  geom_segment(data=ann_line,aes(x=xmid,xend=xmin,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show.legend=F, size = 1.5)+
  geom_segment(data=ann_line,aes(x=xmid,xend=xmax,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show.legend=F, size = 1.5)+
  geom_text(data=ann_text,aes(x=x,y=y,label=label),size=6,show.legend=F)+
  geom_segment(data=ann_lineCor, aes(x=x,y=y,xend=xend, yend=yend), linetype=2, size = 1.5,show.legend=F)+
  geom_segment(data=ann_lineAdj, aes(x=x,y=y,xend=xend, yend=yend), linetype=2, size = 1.5,show.legend=F)+
  geom_segment(aes(x = -6, y = 0, xend = 12, yend = 0), size = 1,show.legend=F)

