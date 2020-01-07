devtools::install_github("dkahle/ggmap")
devtools::install_github("rodazuero/gmapsdistance")

library(gmapsdistance)
library(ggplot2)
library(ggmap)
library(jsonlite)
library(data.table)

geo_key  # Using the personal geo_key API generated from google

# Taking raw data and converting it into useful format
data <- fromJSON("Takeout/Location History/Location_History.json")
loc <- as.data.table(data$locations)
loc$time = as.POSIXct(as.numeric(loc$timestampMs)/1000, origin = "1970-01-01")
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

# Subsetting data to only include everything inside Dallas
corner1 <- c(33.202865, -97.603125)
corner2 <- c(32.206715, -96.051447)
loc1 <- subset(loc, lon > corner1[2] & lon < corner2[2] & lat > corner2[1] & lat < corner1[1])
loc1[,1:3]<-NULL

lats <- c(33.402865,32.706715)
lons <- c(-97.603125,-96.051447)

# subsetting data for everything in Aug 2018
loc2 <- subset(loc,time >= "2018-08-01" & time <= "2018-08-31", select = c("time","lat","lon"))


register_google(key = geo_key)
# bbox <- make_bbox(loc$lon,loc$lat,f=0.5)
dallas <- get_map(location = c(lon = mean(lons), lat = mean(lats)), zoom = 11,
                  maptype = "roadmap", source = "google")

ggmap(dallas) + geom_point(data=loc1, aes(x=lon, y = lat), alpha=0.5)



# Second session
as.numeric(loc2$time[2] - loc2$time[1])
x <- as.numeric(diff(loc2$time))
hist(x[which(x > -100)])


# Third session

library(geosphere)

loc2$dist_prev <- NA     #Variable to store first difference distance
loc2$time_prev <- NA     #Variable to store first difference time
loc2$vel_prev <- NA     #Variable to store first difference velocity

system.time({

for (i in 1:10000){   #(nrow(loc2)-1)
  loc2$dist_prev[i] <- distm(c(loc2$lon[i],loc2$lat[i]), 
                             c(loc2$lon[i+1],loc2$lat[i+1]), 
                             fun = distHaversine)[1,1]    #Dist in meters
  loc2$time_prev[i] <- as.numeric(loc2$time[i]-loc2$time[i+1])   #Time in sec
}
  
})

loc2$vel_prev <- (loc2$dist_prev/loc2$time_prev)*2.236936 # Into MPH



loc3 <- loc2[1:10000,]
loc3$in_motion <- loc3$vel_prev >= 1.5

# Working to generate the moving averages
loc3$five_prev <- NA
loc3$five_next <- NA
loc3$three_each_side <- NA

for (i in 5:(nrow(loc3)-5)){
  loc3$five_prev[i] <- mean(loc3$in_motion[i:(i+4)])
  loc3$five_next[i] <- mean(loc3$in_motion[(i-4):(i)])
  loc3$three_each_side[i] <- mean(loc3$in_motion[(i-3):(i+3)])
}

# write.csv(loc3,"loc3.csv", row.names = FALSE)


# Fourth session

# loc3 <- read.csv("loc3.csv")

m_thresh = 0.5 #Establishing the boundary between moving and stationary
loc3$label <- NA

#The logic to label start and ends of a journey

for (i in 7:(nrow(loc3)-7)){
  if(loc3$three_each_side[i] < m_thresh & loc3$three_each_side[i-1] >= m_thresh) loc3$label[i] <- "Start"
  else if(loc3$three_each_side[i] >= m_thresh & loc3$three_each_side[i-1] < m_thresh) loc3$label[i] <- "End"
  else if(loc3$three_each_side[i] >= m_thresh) loc3$label[i] <- "M"
  else if(loc3$three_each_side[i] < m_thresh) loc3$label[i] <- "X"
  else loc3$label[i] <- "Exception"
}

plot(loc3[loc3$vel_prev <500,]$time, loc3[loc3$vel_prev <500,]$vel_prev)
plot(loc3$time,loc3$three_each_side, type = "l")

num_journeys <- sum(loc3$label == "Start", na.rm = TRUE)

loc4 <- data.frame(start_datetime = rep(NA,num_journeys))

loc4$start_datetime <- subset(loc3, label == "Start")$time
loc4$from_lat <- subset(loc3, label == "Start")$lat
loc4$from_lon <- subset(loc3, label == "Start")$lon

loc4$end_datetime <- subset(loc3, label == "End")$time
loc4$to_lat <- subset(loc3, label == "End")$lat
loc4$to_lon <- subset(loc3, label == "End")$lon

round_sig <-2

loc4[,c("round_flat","round_flon","round_tlat","round_tlon")] <- apply(loc4[,c("from_lat","from_lon","to_lat","to_lon")],2,function(x) round(x,round_sig))

dallas <- get_map(location = c(lon = mean(loc4$round_flon), lat = mean(loc4$round_flat)), zoom = 13,
                  maptype = "roadmap", source = "google")

ggmap(dallas) + geom_point(data=loc4, aes(x=round_flon, y = round_flat), alpha=0.5)




set.api.key(geo_key)
drive_data <- gmapsdistance(paste(loc4$from_lat,loc4$from_lon,sep=","),
                            paste(loc4$to_lat,loc4$to_lon,sep=","),
                            departure = round(as.numeric(Sys.time())/86400)*86400+64800,
                            combinations = "pairwise",mode="driving")
transit_data <- gmapsdistance(paste(loc4$from_lat,loc4$from_lon,sep=","),
                            paste(loc4$to_lat,loc4$to_lon,sep=","),
                            departure = round(as.numeric(Sys.time())/86400)*86400+64800,
                            combinations = "pairwise",mode="transit")

loc4$drive_time <- drive_data$Time$Time
loc4$transit_time <- transit_data$Time$Time
loc4$drive_dist <- drive_data$Distance$Distance
loc4$transit_dist <- transit_data$Distance$Distance

loc4$save_ratio <- (loc4$transit_time - loc4$drive_time)/loc4$transit_time

hist(loc4$save_ratio[loc4$save_ratio > 0])


loc4 <- as.data.table(loc4)

loc5 <- loc4[, list(Freq =.N), by=list(round_flon,round_flat,round_tlon,round_tlat)]

loc4 <- merge(loc4, loc5, by = c("round_flon","round_flat","round_tlon","round_tlat"))

dallas <- get_map(location = c(lon = mean(loc4$round_flon), lat = mean(loc4$round_flat)), zoom = 13,
                  maptype = "roadmap", source = "google")

ggmap(dallas) + geom_segment(data = subset(loc4, Freq >= 1 & Freq <= 26 & save_ratio >= 0) ,
                             aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat, size=Freq, color = save_ratio))

write.csv(loc4,"loc4.csv", row.names = FALSE)
