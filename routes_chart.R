
# Plot charts
# Author: Kevin Seow 

# Common Libraries 

library(tidyr)
library(plyr)
library(dplyr)
library(data.table)
library(scales)
library(RcppRoll)
library(lubridate)
library(ggplot2)
library(zoo)
library(corrplot)
library(broom)
library(forecast)
library(ggrepel)
library(raster)

wd <- "C:/Users/Kevin/Desktop/learning/git/20210402 michelin project/"
setwd(wd)

routes <- read.csv("michelin_routes.csv", header = F)
routes <- t(routes)
colnames(routes) <- paste0("ROUTE_",seq(1:5))

df_route <- gather(data.frame(routes), ROUTE, postal) %>% 
            mutate(postal = ifelse(nchar(postal) == 6, postal,paste0("0",postal)))

postcodes <- read.csv("Michelin_geolocations.csv", header = T, stringsAsFactors = F)[-1] %>% 
             mutate(postal = ifelse(nchar(postal) == 6, postal,paste0("0",postal)))

df_route <- left_join(df_route, postcodes, by = c("postal"))

df_route <- df_route %>% mutate(latend = lead(lat, n = 1L)) %>% mutate(longend = lead(long, n = 1L))

michelin <- read.csv("restaurant_postcodes.csv", header = T, stringsAsFactors = F) %>% 
            mutate(postal = ifelse(nchar(postcode) == 6, postcode,paste0("0",postcode)))

michelin <- left_join(michelin, postcodes, by = c("postal"))


sgp = getData("GADM", country = "SGP", level = 1) # get the coordinates of Singapore
sgp = fortify(sgp)

linecols <- c("dodgerblue3","darkorange1","slategray4","mediumorchid","darkturquoise")

p.route <- ggplot(sgp, aes(x = long, y = lat)) + 
  geom_map(data = sgp, map = sgp, aes( map_id = id), fill = NA, color = "black") + theme_bw() +
  geom_point(data = df_route, aes(x = long, y = lat, color = ROUTE), size = 2) + 
  geom_segment(data = df_route, aes(x = long, y = lat, xend = longend, yend = latend, group = ROUTE, color = ROUTE), size = 1.5) + 
  geom_point(data = df_route, aes(x = long, y = lat, fill = ROUTE), 
             alpha = 0.5, shape = 21, show.legend = FALSE) + 
  theme(legend.title = element_text(size = 10)) + 
  ylim(1.25,1.375) + xlim(103.7,103.95) + scale_color_manual(values = linecols) + scale_fill_manual(values = linecols)

ggsave(filename = "route_michelin_map.png", plot = p.route, dpi = 1200, width = 11, height = 6, limitsize = FALSE)

p.rest <- ggplot(sgp, aes(x = long, y = lat)) + 
  geom_map(data = sgp, map = sgp, aes( map_id = id), fill = NA, color = "black") + theme_bw() +
  geom_point(data = michelin, aes(x = long, y = lat), size = 1.5, color = "tomato", alpha = 0.7)

ggsave(filename = "michelin_map.png", plot = p.rest, dpi = 1200, width = 11, height = 6, limitsize = FALSE)

michelin$grade <- gsub("\\:.*", "", michelin$michelin_grade)

p.grade <- ggplot(sgp, aes(x = long, y = lat)) + 
  geom_map(data = sgp, map = sgp, aes( map_id = id), fill = NA, color = "black") + theme_bw() +
  geom_point(data = michelin, aes(x = long, y = lat, color = grade, fill = grade), size = 1.5, alpha = 0.9) + 
  scale_fill_manual(values = linecols) + scale_color_manual(values = linecols)

ggsave(filename = "michelin_map.png", plot = p.rest, dpi = 1200, width = 11, height = 6, limitsize = FALSE)
