setwd("C:/Users/user/Desktop/R/Course 10")

#install.packages("rjson")

library("rjson")
library("jsonlite")
library("RODBC")
library("sqldf")

## https://www.kaggle.com/c/yelp-recsys-2013/forums/t/4465/reading-json-files-with-r-how-to
fnames <- c('business','checkin','review','tip','user')
jfile <- paste0(getwd(),'/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_',fnames,'.json')
business_data <- stream_in(file(jfile[1]))
checkin_data<- stream_in(file(jfile[2]))
#review_data <- stream_in(file(jfile[3]))
#tip_data<- stream_in(file(jfile[4]))
#user_data<- stream_in(file(jfile[5]))


allNames<- names(checkin_data[1, 1])

View(allNames)

allNames

allNamesDF <- data.frame(do.call('rbind', strsplit(as.character(allNames),'-',fixed=TRUE)))

names(allNamesDF)[1]<- "Time"
names(allNamesDF)[2]<- "Day"

allNamesDF<- cbind(allNamesDF, allNamesDF[2])

names(allNamesDF)[3]<- "Day_Full"

allNamesDF[3]<- gsub("0", "SUN", allNamesDF[,3])
allNamesDF[3]<- gsub("1", "MON", allNamesDF[,3])
allNamesDF[3]<- gsub("2", "TUE", allNamesDF[,3])
allNamesDF[3]<- gsub("3", "WED", allNamesDF[,3])
allNamesDF[3]<- gsub("4", "THUR", allNamesDF[,3])
allNamesDF[3]<- gsub("5", "FRI", allNamesDF[,3])
allNamesDF[3]<- gsub("6", "SAT", allNamesDF[,3])

nrow(checkin_data)
for(currentRow in 39003:45166)
{
  #print(checkin_data[currentRow,3])
  t_checkin<- t(checkin_data[currentRow,1])
  
  if(currentRow == 1)
  {
    tcheckin <- cbind(t_checkin, checkin_data[currentRow,3])
  }
  else 
  {
   tcheckin<- rbind(tcheckin, cbind(t_checkin, checkin_data[currentRow,3]))
  }
  #t_checkin
  #business_data$longitude
  #business_data$latitude
}

tcheckin<- cbind(row.names(tcheckin), tcheckin)

colnames(tcheckin)[1]<- "day_time"
colnames(tcheckin)[2]<- "checkin"
colnames(tcheckin)[3]<- "business_id"
tcheckin_final<- na.omit(tcheckin)

colnames(tcheckin)

tcheckin_final_DF<- as.data.frame(tcheckin_final)

View(business_data[,c(1,10,13)])
names(merge_data)

merge_data<- merge(x = tcheckin_final_DF, y = business_data[,c(1,10,13)], by = "business_id")
merge_data<- cbind(merge_data, data.frame(do.call('rbind', strsplit(as.character(merge_data$day_time),'-',fixed=TRUE))))

rm(test)
colnames(merge_data)[6]<- "time"
colnames(merge_data)[7]<- "day"
             
names(merge_data)

#Install&load packages
install.packages("animation")
library("animation")

install.packages("ggmap")
library("ggmap")

library(sqldf)
final<- sqldf("select time, day, longitude, latitude, sum(checkin) checkin from merge_data group by time, day, longitude, latitude")

write.csv(final, file = "checkins.csv", row.names = T)

final<- read.csv("checkins.csv", header = T)

final[final$checkin == max(final$checkin), ]

extent <- c(min(final$longitude), min(final$latitude), 
            max(final$longitude), max(final$latitude))

basemap <- get_map(location = extent, maptype = "watercolor", source = "stamen")
basemap <- ggmap(basemap, extent = "device")

basemap

ani.options(outdir = getwd())

#Construct the hourly labels
am <- paste(1:11, "AM", sep = "")
am <- append("12AM", am)
pm <- paste(1:11, "PM", sep = "")
pm <- append("12PM", pm)
hour_label <- append(am, pm)

summary(final$day)

path.to.convert <- paste0(shortPathName(
  "C:\\Program Files\\ImageMagick-6.9.2-Q16\\"), "convert.exe")
ani.options(convert=path.to.convert)

c<- saveGIF({
  #Set the first frame to the 0 (12am)
  hour<-0
  #Draw the check-in patterns for each hour using a while loop
  while(hour < 24) {
    #Subset the data for this hour
    this_hour <- final[final$time == hour, ]
    
    if(nrow(this_hour) != 0)
    {
      #Compose the map for this hour (basemap + points + time label)
        this_hour_map <- basemap +
        #Set long/lat and adjust the alpha value for each points to mitigate the overplotting problem
        geom_point(aes(x = longitude, y = latitude, alpha = checkin),
                   #Set the data to be plotted
                   data = this_hour, col = "purple", size = 0.8) +
        #Plot the black shadow for time label
        geom_text(data = NULL, x = -50 + 0.003, y = 50.9 - 0.003,
                  label = hour_label[hour + 1], color = "black",
                  face = "bold", cex = 10) +
        #Plot the time label
        geom_text(data = NULL,x = -50, y = 50.9  - 0.003,
                  label = hour_label[hour + 1], color = "#9A413B",
                  face = "bold", cex = 10) +
        #Remove axis, ticks, etc.
        theme(axis.line=element_blank(), axis.text.x=element_blank(),
              axis.text.y=element_blank(), axis.ticks=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.border = element_blank(),plot.background=element_blank(),
              legend.position="none")
      
      #Plot the map composed above
      plot(this_hour_map, xaxs = "i", yaxs = "i")
    }
    #Increment to next hour
    hour <- hour + 1
    
    #Keep track of the current progress
    print(hour)
  }
}, movie.name = "yelp_hourly.gif", interval = 0.6, ani.width = 600, ani.height = 600)
