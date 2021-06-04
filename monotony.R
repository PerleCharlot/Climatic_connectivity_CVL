###################################
# Check for monotony in corridors #
###################################
library(readxl)
library(data.table)
library(rgdal)

setwd("C:/Users/charlotp/Documents/MONOTONY")#folder where are all MAT and neigh tables

#Load LCPs network

### Get the list of LCP
# open cmd
# cd C:\Users\charlotp\Documents\MONOTONY\MAT
# dir /b *.xls > mylist.txt
LCP_list <- fread("list_LCP.csv")#list of LCP, as they don't have a continous order

store_check <- c()
store_LCP <- c()
store_Trange <- c()
counter = 0

for(LCP in LCP_list$list){
  counter = counter + 1
  print(paste(counter, "/",length(LCP_list$list)))

  MAT <- tryCatch(read_excel(paste("MAT/", LCP, ".xls", sep="")),error=function(e) e)
  if (inherits(MAT, "error")){MAT <- read_excel(paste("MAT/", LCP, ".xlsx", sep=""))}
  neighbor <- tryCatch(read_excel(paste("Neigh/", LCP, ".xls", sep="")),error=function(e) e)
  if (inherits(neighbor, "error")){neighbor <- read_excel(paste("Neigh/", LCP, ".xlsx", sep=""))}
  
  #find out EXTREME pixels
  start_end_list <- neighbor$src_FID_fishnet_big_extent[!(duplicated(neighbor$src_FID_fishnet_big_extent)|duplicated(neighbor$src_FID_fishnet_big_extent, fromLast=TRUE))]
  
  #Find which one is START and END
  df = data.frame(start_end_list, MAT$MEAN[which(MAT$FID_fishnet_big_extent %in% start_end_list)])
  names(df) <- c("pixel","temperature")
  #end = df$pixel[which(df$temperature == min(df$temperature))]
  start = df$pixel[which(df$temperature == max(df$temperature))] #source = warmest
  
  #order pixel (from start pixel to the end)
  pixel_1 = start
  pixel_2 = neighbor$src_FID_fishnet_big_extent[which(neighbor$nbr_FID_fishnet_big_extent == pixel_1)]
  pixel_storage = c(pixel_1,pixel_2)
  for (i in 1:length(MAT$FID_fishnet_big_extent)){
    a = neighbor$src_FID_fishnet_big_extent[which(neighbor$nbr_FID_fishnet_big_extent == pixel_2)]
    pixel_3 = a[which(a[]!=pixel_1)] 
    if(length(pixel_3)>1){ #issue
      # If there is at least one pixel already processed
      if(any(pixel_3 %in% pixel_storage)){
        pixel_3 <- pixel_3[-which(pixel_3 %in% pixel_storage)]#the new pixel
      } else {index1 = which(neighbor$nbr_FID_fishnet_big_extent == pixel_3[1])  #means they are news
              index2 = which(neighbor$src_FID_fishnet_big_extent == pixel_2)
              dist1 <- neighbor$LENGTH[index1[which(index1 %in% index2)]]
              #select the adjacent neighbor
              index1 = which(neighbor$nbr_FID_fishnet_big_extent == pixel_3[2])
              index2 = which(neighbor$src_FID_fishnet_big_extent == pixel_2)
              dist2 <- neighbor$LENGTH[index1[which(index1 %in% index2)]]

              dt <- cbind(c(pixel_3[1],dist1),c(pixel_3[2],dist2))
              pixel_3 <- dt[1,which(max(dist1,dist2) == dt[2,])]}
    } else {pixel_3 <- pixel_3}
    
    pixel_storage = append(pixel_storage, pixel_3)
    pixel_1 = pixel_2
    pixel_2 = pixel_3
  }
  
  # Check for consistency
  length(pixel_storage) == length(MAT$COUNT)
  
  #add temperature to ordered pixels
  
  temperature = c()
  pixel = c()
  for(i in 1:length(pixel_storage)){
    temp  = MAT$MEAN[which(MAT$FID_fishnet_big_extent %in% pixel_storage[i])]
    temperature = append(temperature, temp)
    pixel = append(pixel, pixel_storage[i]) 
    }
  
  corridor_ordered <-data.frame(pixel, temperature)
  
  #save temperature ordered for each corridor if we need it later on
  write.csv(corridor_ordered, file = paste("Temperatures_Corridors/corr_ord_", LCP, ".csv", sep=""))
  
  #check if it's strictly decreasing (i.e. monotonic)
  check <- all(diff(corridor_ordered$temperature) <= 0)
  #check <- is.unsorted(corridor_ordered$temperature)
  if(check == T) {check <- "Monotonic" #TRUE <=> strictly decreasing <=> MONOTONIC
  
  #Temperature range assessment
  Trange <- range(corridor_ordered$temperature)
  #dfTrange <- data.frame(Trange[1], Trange[2],Trange[2] - Trange[1])
  #names(dfTrange)<- c("Minimum", "Maximum", "Temperature Range in degree")
  #write.csv(dfTrange, file = paste("Temperatures_Corridors/Trange_corr_", LCP, ".csv", sep=""))
  store_Trange <- append(store_Trange, Trange[2] - Trange[1])
  
  } else { check <- "Non-monotonic"}
  store_check <- append(store_check, check)
  store_LCP <- append(store_LCP, LCP)
}#20 min

monotony_summary <- data.frame(store_LCP, store_check)
names(monotony_summary) <- c("Corridor", "Status")
head(monotony_summary)
summary(monotony_summary)

#18 142 non-monotonic + 13 597 monotonic

write.csv(monotony_summary, file="monotony_summary.csv")


Trange_summary <- data.frame(store_LCP, store_Trange)
names(Trange_summary) <- c("Corridor", "Temperature Range")
head(Trange_summary)
write.csv(Trange_summary, file="Trange_summary.csv")
# then merge list corridors to keep with a file with corridor ID / link ID

