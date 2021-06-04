##############
# PA Metrics #
##############

library(data.table)

setwd("C:/Users/perle/OneDrive/Bureau/RDATA/Metrics")

# Load inputs
anx <- fread("Inputs/animals_cores.csv")
vgx <- fread("Inputs/plants_cores.csv")
#patches <- readOGR("D:/", "climatic_patches")
patches.dt <- fread("Inputs/core_PA.csv")
patches.dt <- patches.dt[,-c(3,4)]
corridor <- fread("C:/Users/perle/OneDrive/Bureau/RDATA/propagation/data_all_Austria/mean_500m/AllAUCorridors_subset.csv")#list corridor where source = hottest


# Remove species we're not confident in data relevance
# We keep all plants + Insects (except Apterygota, Psocoptera, Thysanoptera)
# Apterygota = Diplura + collembola + Protura + Archaeognatha + Zygentoma (Name_Tax2)
# Psocoptera = no species found
# Thysanoptera = no species found
tab_anx <- fread("Inputs/insect_species_todelete.csv")
sub <- tab_anx[which(tab_anx$Name_Tax1 == "Insecta"),]
sub <- sub[!which(sub$Name_Tax2 == "Diplura"),]
sub <- sub[!which(sub$Name_Tax2 == "Collembola"),]
sub <- sub[!which(sub$Name_Tax2 == "Protura"),]
sub <- sub[!which(sub$Name_Tax2 == "Archaeognatha"),]
sub <- sub[!which(sub$Name_Tax2 == "Zygentoma"),]
idsp_tokeep <- sub$ID

anx <- anx[which(anx$specid %in% idsp_tokeep),]

# # Statistics on endemic species
min_elev = as.numeric(sub$Höhe_min)
min_elev = na.omit(min_elev)
hist(min_elev, freq=F)
max_elev = as.numeric(sub$Höhe_max)
max_elev = na.omit(max_elev)
hist(max_elev, freq=F)
# median(min_elev) #1900 m
#
plantes <- fread("Inputs/insect_species_todelete.csv")
min_elev_p = as.numeric(plantes$Höhe_min)
min_elev_p = na.omit(min_elev_p)
hist(min_elev_p)
#
plantes <- fread("Inputs/insect_species_todelete.csv")
max_elev_p = as.numeric(plantes$Höhe_max)
max_elev_p = na.omit(max_elev_p)
hist(min_elev_p)
# median(min_elev_p)#1760 m
#
MIN_ELEV = c(min_elev,min_elev_p)
hist(MIN_ELEV)
MAX_ELEV = c(max_elev,max_elev_p)
hist(MAX_ELEV)
# median(MIN_ELEV)#1800m
# summary(MIN_ELEV)#1st qu = 1356, med = 1800, mean = 1736, 3rd qu = 2189, max = 3797

# How many patches by PA?
patchesbyPA <-aggregate(core_ID ~ PA_ID, patches.dt, function(x) length(unique(x)))

mean(patchesbyPA$core_ID)
sd(patchesbyPA$core_ID)
median(patchesbyPA$core_ID)
hist(patchesbyPA$core_ID)

###### CORRIDOR METRIC 1: how many sp by corridor ###### 
# We considered that all sp present in a PA that has a corridor
# are potentially able to take this corridor (if the PA is the hottest)
# even though the corridor has been built considering patches only

# Calculate how many different unique animal species in a PA
anxperpatch <- merge(anx, patches.dt, by.x = "core_id", by.y ="core_ID")
anxperPA <-aggregate(specid ~ PA_ID, anxperpatch, function(x) length(unique(x)))
anxperPA  <- merge(patches.dt, anxperPA , by="PA_ID", all=T)

#anxperPA[is.na(anxperPA)] <- 0
names(anxperPA) <- c("PA_ID","core_ID", "anx")

# Calculate how many different unique vegetal species in a PA
vgxperpatch <- merge(vgx, patches.dt, by.x = "core_id", by.y ="core_ID")
vgxperPA <-aggregate(specid ~ PA_ID, vgxperpatch, function(x) length(unique(x)))
vgxperPA <- merge(patches.dt, vgxperPA , by="PA_ID", all=T)
#vgxperPA[is.na(vgxperPA)] <- 0
names(vgxperPA) <- c("PA_ID","core_ID", "vgx")

# Merge animal and vegetal counts
sum_tot_patch <- merge(anxperPA, vgxperPA, by="core_ID", all=T)
sum_tot_patch$sp <- sum_tot_patch$anx + sum_tot_patch$vgx
sum_tot_patch <- sum_tot_patch[,-c(4)]

#
487
sum_tot_patch$core_ID[which(sum_tot_patch$PA_ID.x == 487)]

list_an = unique(anx$specid[which(anx$core_id == 2376)], anx$specid[which(anx$core_id == 2582)])
list_pl = unique(vgx$specid[which(vgx$core_id == 2376)], vgx$specid[which(vgx$core_id == 2582)])
list_pl
# Merge total count with corridor file
sppercorridor <- merge(corridor,sum_tot_patch, by.x = "Origin", by.y = "core_ID")
# we keep: origin core, length, cost, corridor_ID,PA_ID,sp counts  
sppercorridor<- sppercorridor[,c(1,7,9,10,14,15,16,17)]
names(sppercorridor) <- c("origin_patch","length","cost","corridor_ID","PA_ID","animal","vegetal","allsp")
write.csv(sppercorridor, file = "Outputs/metricCor1.csv")

###### CORRIDOR METRIC 2: rank corridor by their cost, length and sp ###### 
# Corridors are ranked considering those 3 parameters
# the top rank is attributed to the "best" corridor (=lowest cost, lowest length, highest sp)

# Rank by cost
sppercorridor <- sppercorridor[order(sppercorridor[,2]),]
sppercorridor$r_cost <- c(1:length(sppercorridor$cost))
# Rank by lenght
sppercorridor <- sppercorridor[order(sppercorridor[,3]),]
sppercorridor$r_length <- c(1:length(sppercorridor$length))
# Rank by number of species
sppercorridor <- sppercorridor[order(sppercorridor[,8], decreasing = T),]
sppercorridor$r_sp <- c(1:length(sppercorridor$allsp))

# Final rank, weight of the 3
sppercorridor$sum_r <- sppercorridor$r_cost + sppercorridor$r_length + sppercorridor$r_sp
sppercorridor <- sppercorridor[order(sppercorridor[,12]),]
sppercorridor$RANK <- c(1:length(sppercorridor$sum_r))

write.csv(sppercorridor, file = "Outputs/metricCor2_Rank.csv")

###### Loop for PA metrics ###########


for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    year = 2030
    scenario = 26
    
    setwd("C:/Users/charlotp/Documents/propagation/data_all_Austria/mean_500m/Output")
    hf <- fread(paste("AllData_rcp",scenario,"_",year,".csv", sep =""))
    hf2 <- fread(paste("AllData_rcp",scenario,"_",year,"_COR.csv", sep =""))
    
    # ### Check
    # #adjacency only
    # table(hf$HowFar_0)
    # length(which(hf$HowFar_0 >= 0))#3636 patches successful
    # #corridors
    # table(hf2$HowFar_100)
    # length(which(hf2$HowFar_100 >= 0)) #5080 patches successful
    
    ###### PA METRIC 1: how many species are successful within a PA WITHOUT corridors ###### 
    # sum up all species which are in climate successful patches
    # but be careful and keep only different ones
    
    # First with animal
    anxpersuccess <- merge(anx, patches.dt, by.x = "core_id", by.y ="core_ID")
    totanx <- anxpersuccess
    anxpersuccess <- merge(anxpersuccess, hf, by.x = "core_id", by.y ="Cores")
    anxpersuccess <- anxpersuccess[,c(1,2,3,20)]
    # Keep only successful patches
    anxpersuccess <- anxpersuccess[which(anxpersuccess$HowFar_0 >=0 ),]
    # Calculate unique different sp
    anxpersuccessperPA <-aggregate(specid ~ PA_ID, anxpersuccess, function(x) length(unique(x)))
    
    # Then with vegetal
    vgxpersuccess <- merge(vgx, patches.dt, by.x = "core_id", by.y ="core_ID")
    totvgx <- vgxpersuccess
    vgxpersuccess <- merge(vgxpersuccess, hf, by.x = "core_id", by.y ="Cores")
    vgxpersuccess <- vgxpersuccess[,c(1,2,3,20)]
    # Keep only successful patches: HowFar >= 0 <=> TcO >= TfD : SUCCESS
    vgxpersuccess <- vgxpersuccess[which(vgxpersuccess$HowFar_0 >=0 ),]
    # Calculate unique different sp
    vgxpersuccessperPA <-aggregate(specid ~ PA_ID, vgxpersuccess, function(x) length(unique(x)))
    
    # Merge animal and vegetal counts of successful patches
    sum_success <- merge(anxpersuccessperPA, vgxpersuccessperPA, by="PA_ID", all=T)
    sum_success[is.na(sum_success)] <- 0
    names(sum_success) <- c("PA_ID","animalSUC","vegetalSUC")
    
    # Get total counts to compute % of successful species by PA
    totanxperPA <-aggregate(specid ~ PA_ID, totanx, function(x) length(unique(x)))
    totvgxperPA <-aggregate(specid ~ PA_ID, totvgx, function(x) length(unique(x)))
    sum_tot <- merge(totanxperPA, totvgxperPA, by="PA_ID", all=T)
    sum_tot[is.na(sum_tot)] <- 0
    names(sum_tot) <- c("PA_ID","animalTOT","vegetalTOT")
    
    sum_success <- merge(sum_tot, sum_success, by ="PA_ID", all=T)
    sum_success[is.na(sum_success)] <- 0
    sum_success$allspTOT <- sum_success$animalTOT + sum_success$vegetalTOT
    sum_success$allspSUC <- sum_success$animalSUC + sum_success$vegetalSUC
    sum_success$percentSUC <- (sum_success$allspSUC / sum_success$allspTOT)*100
    sum_success$percentUNSUC <- 100 - sum_success$percentSUC

    
    # ### Check
    # hist(sum_success$percentSUC)
    
    #sum_success <- merge(unique(patches.dt[,c(2)]), sum_success, by = "PA_ID", all.x = T)
    
    name <- paste("C:/Users/charlotp/Documents/Metrics/Outputs/metricPA1_withoutCor_",scenario,"_",year,".csv", sep="")
    write.csv(sum_success, file = name)
    
    
    ###### PA METRIC 3: how many species are successful within a PA WITH corridors######
    # sum up all species which are in climate successful patches by PA and with corridors
    # but be careful and keep only different ones
    
    # First with animal
    anxpersuccess2 <- merge(totanx, hf2, by.x = "core_id", by.y ="Cores")
    anxpersuccess2 <- anxpersuccess2[,c(1,2,3,18)]
    # Keep only successful patches
    anxpersuccess2 <- anxpersuccess2[which(anxpersuccess2$HowFar_100 >= 0 ),]
    # Calculate unique different sp
    anxpersuccessperPA2 <-aggregate(specid ~ PA_ID, anxpersuccess2, function(x) length(unique(x)))
    
    # Then with vegetal
    vgxpersuccess2 <- merge(totvgx, hf2, by.x = "core_id", by.y ="Cores")
    vgxpersuccess2 <- vgxpersuccess2[,c(1,2,3,18)]
    # Keep only successful patches
    vgxpersuccess2 <- vgxpersuccess2[which(vgxpersuccess2$HowFar_100 >= 0 ),]
    # Calculate unique different sp
    vgxpersuccessperPA2 <-aggregate(specid ~ PA_ID, vgxpersuccess2, function(x) length(unique(x)))
    
    # Merge animal and vegetal counts of successful patches
    sum_success2 <- merge(anxpersuccessperPA2, vgxpersuccessperPA2, by="PA_ID", all=T)
    sum_success2[is.na(sum_success2)] <- 0
    names(sum_success2) <- c("PA_ID","animalSUC","vegetalSUC")
    
    # Get total counts to compute % of successful species by PA
    sum_success2 <- merge(sum_tot, sum_success2, by ="PA_ID", all=T)
    sum_success2[is.na(sum_success2)] <- 0
    sum_success2$allspTOT <- sum_success2$animalTOT + sum_success2$vegetalTOT
    sum_success2$allspSUC <- sum_success2$animalSUC + sum_success2$vegetalSUC
    sum_success2$percentSUC <- (sum_success2$allspSUC / sum_success2$allspTOT)*100
    sum_success2$percentUNSUC <- 100 - sum_success2$percentSUC
    
    # ### Check
    # hist(sum_success2$percentSUC)

    name2 <- paste("C:/Users/charlotp/Documents/Metrics/Outputs/metricPA3_withCor_",scenario,"_",year,".csv", sep="")
    write.csv(sum_success2, file = name2)

  }}






############## # Statistics
setwd("C:/Users/charlotp/Documents/Metrics/Outputs")

#import data
patches.dt <- fread("C:/Users/charlotp/Documents/Metrics/Inputs/core_PA.csv")
test <-aggregate(Shape_Area ~ PA_ID, patches.dt, function(x) sum(x))
test$Shape_Area <- test$Shape_Area / 1000000 #get in km2
sum(test$Shape_Area)
hist(test$Shape_Area)
summary(test$Shape_Area) #min 0,01 / max 1331

test$category <- c()
test[which(test$Shape_Area <= 0.05),3] <- "VS" #462 VERY SMALL
test[which(test$Shape_Area <= 0.1 & test$Shape_Area > 0.05),3] <- "S" # 200 SMALL
test[which(test$Shape_Area <= 1 & test$Shape_Area > 0.1),3] <- "M" # 623 MEDIUM
test[which(test$Shape_Area <= 100 & test$Shape_Area > 1),3] <- "L" # 451 LARGE
test[which(test$Shape_Area > 100),3] <- "VL" # 53 VERY LARGE
names(test)<-c("PA_ID","Area","Category")

# Corridors
mean_Cor <- data.frame()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    
    path <- paste("metricPA3_withCor_",scenario,"_",year,".csv", sep="")
    name <- paste("Cor_",scenario,"_",year,sep="") # "Cor_26_2030
    tab <- assign(name,fread(path))
    tab.m <- merge(test, tab, by="PA_ID")
    tab.m <- tab.m[,c(1:3,11)] #PA_ID, Area, Category, percentSUC
    tab.m$Category <-factor(tab.m$Category, levels=c("VS", "S", "M","L","VL"))
    
    mVS <- mean(tab.m$percentSUC[which(tab.m$Category == "VS")])
    mS <- mean(tab.m$percentSUC[which(tab.m$Category == "S")])
    mM <- mean(tab.m$percentSUC[which(tab.m$Category == "M")])
    mL <- mean(tab.m$percentSUC[which(tab.m$Category == "L")])
    mVL <- mean(tab.m$percentSUC[which(tab.m$Category == "VL")])
    mtot <- mean(tab.m$percentSUC)
    
    dt <- data.frame(c(rep(mtot,5)),c(mVS,mS,mM,mL,mVL),c("VS", "S", "M","L","VL"),c(rep(year,5)), c(rep(scenario,5)))
    names(dt) <- c("m%SUC_tot","m%SUC","Category","year","scenario")
    mean_Cor <- rbind(mean_Cor, dt)
  }}

# adjacency
mean_Adj <- data.frame()
for(scenario in c(26,45,85)){
  for(year in c(2030,2040,2050,2060,2070,2080)){
    path <- paste("metricPA1_withoutCor_",scenario,"_",year,".csv", sep="")
    name <- paste("Adj_",scenario,"_",year,sep="") # "Adj_26_2030
    tab <- assign(name,fread(path))
    tab.m <- merge(test, tab, by="PA_ID")
    tab.m <- tab.m[,c(1:3,11)] #PA_ID, Area, Category, percentSUC
    tab.m$Category <-factor(tab.m$Category, levels=c("VS", "S", "M","L","VL"))
    
    mVS <- mean(tab.m$percentSUC[which(tab.m$Category == "VS")])
    mS <- mean(tab.m$percentSUC[which(tab.m$Category == "S")])
    mM <- mean(tab.m$percentSUC[which(tab.m$Category == "M")])
    mL <- mean(tab.m$percentSUC[which(tab.m$Category == "L")])
    mVL <- mean(tab.m$percentSUC[which(tab.m$Category == "VL")])
    mtot <- mean(tab.m$percentSUC)
    
    dt <- data.frame(c(rep(mtot,5)),c(mVS,mS,mM,mL,mVL),c("VS", "S", "M","L","VL"),c(rep(year,5)), c(rep(scenario,5)))
    names(dt) <- c("m%SUC_tot","m%SUC","Category","year","scenario")
    mean_Adj <- rbind(mean_Adj, dt)
  }}

####### Margin of improvement ####
#Substraction corridors - adjacency to get improvement due to corridors
data <- mean_Adj
data$Improvement <- mean_Cor$`m%SUC` - mean_Adj$`m%SUC`
data$Improvement_tot <- mean_Cor$`m%SUC_tot` - mean_Adj$`m%SUC_tot`
data <- data[,-c(1,2)]
data$PercentImprovement <- (data$Improvement /100) * data$Improvement_tot

data$Category <- factor(data$Category, levels = c( "VL","L","M", "S","VS")) 


#ggplot(data, aes(x=as.factor(year), y=PercentImprovement, fill = as.factor(Category)))+
#geom_bar(stat="identity", position=position_dodge(), color = "black")+
#scale_fill_manual(values=c('skyblue','palegreen3','tomato'), labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"))+

ggplot(data, aes(x = year, y = PercentImprovement))+
  geom_col(aes(fill = Category)) +facet_wrap(~ scenario)+
  labs( x="Date", y="Average % of endemic species climatically successful",
       title="Improvements due to Corridors")










library(cowplot)

plot1 <- ggplot(ab, aes(x=percentSUC, y=Parea))+geom_count(aes(col=Category))+
  labs(y="% of total protected area network",
       x="% species that can find suitable climate", 
       title="2030 - RCP 2.6 - Adjacency")
plot2 <- ggplot(bc, aes(x=percentSUC, y=Parea))+geom_count(aes(col=Category))+
  labs(y="% of total protected area network",
       x="% species that can find suitable climate", 
       title="2030 - RCP 2.6 - Corridors")

plot3 <- ggplot(ab3, aes(x=percentSUC, y=Parea))+geom_count(aes(col=Category))+
  labs(y="% of total protected area network",
       x="% species that can find suitable climate", 
       title="2080 - RCP 2.6 - Adjacency")
plot4 <- ggplot(bc3, aes(x=percentSUC, y=Parea))+geom_count(aes(col=Category))+
  labs(y="% of total protected area network",
       x="% species that can find suitable climate", 
       title="2080 - RCP 2.6 - Corridors")+
        scale_size_area(max_size = 10)

ggplot(bc3, aes(x=percentSUC, y=Parea))+geom_count(aes(size=stat(prop), col = Category))+
  labs(y="% of total protected area network",
       x="% species that can find suitable climate", 
       title="2080 - RCP 2.6 - Corridors")+
  scale_size_area(max_size = 10)

plot_grid(plot1,plot2,plot3,plot4, labels = "AUTO")



# NO data for endeöic species
nd <- fread("C:/Users/charlotp/Documents/Metrics/nodataPA.csv")
(sum(nd$Shape_Area[which(is.na(nd$AllEndSp))]) / sum(nd$Shape_Area))*100
length(which(is.na(nd$AllEndSp)))





######### Methods ###############
data  %>%
  group_by(group) %>%
  summarise(n_distinct(elements_to_count))

dfsorted=df[order(df[,2]),]
