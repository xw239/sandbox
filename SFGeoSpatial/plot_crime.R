library(ggmap)
library(data.table)
library(zipcode)

#load crime data
SF_training <- data.table(read.csv(unz('train.csv.zip', 'train.csv')))
SF_training[, Dates := as.POSIXct(Dates, format="%Y-%m-%d %H:%M:%S", 
                                  tz = "Etc/GMT-8")]
#get SF centroid coordinates
SF_center <- c(lon = -122.4394, lat = 37.7649)
SF_bound <- c(left = -122.52, bottom = 37.70, right = -122.35, top = 37.825)

#google map.less control over specific boundary.
#SF_map <- get_googlemap(center = SF_center,zoom = 12, maptype= "roadmap", 
 #                   crop=T, scale = 2)
#statmen map. Finer control over map range
SF_map_statmen <- get_stamenmap(SF_bound, zoom = 13)
##plot(SF_map)
#plot to see map 
plot(SF_map_statmen)

#get the top 10 crimes in
sort(table(SF_training$Category), decreasing =T)/nrow(SF_training)
top_crimes <- names(sort(table(SF_training$Category), decreasing =T)[1:12])

#plot top crimes by type. Generate png files for each crime type
for (cr in top_crimes) {
    plot_tmp <-  ggmap(SF_map_statmen)+
        geom_point(aes(x = X, y = Y), data = SF_training[Category== cr],
                   alpha = .1, color="darkred", size = 0.3) +  
        labs(x = 'Longitude', y = 'Latitude') + ggtitle(cr)
    
   ggsave(paste0(which(top_crimes==cr), '_',make.names(cr),'.png'), 
          plot_tmp, width =7, height =7)
}
