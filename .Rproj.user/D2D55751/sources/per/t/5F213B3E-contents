#Author: Lachlan Bourke
#Crotalus map script
#2025-03-29

#### Get map file ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf
library(geodata)
# Download map data and plot other data over the map
#Use geodata package. 

#Country with province borders
USA<- geodata::gadm(country = "USA", level = 1, path = "Country_data", version = "latest", resolution = 1, type = "sf")

#Country with elevation data
USA.elevation<-elevation_30s("USA",path = "Altitude_map_data", mask = T)

library(sf)
#use below code to convert spatvector to simple features (sf) (can then map in tmap)
USA <- sf::st_as_sf(USA)

#Need to simplify as has polygons and multipolygons. If use above will get
#duplicate text values for the states and territories.
#https://stackoverflow.com/questions/69947457/tm-text-produces-duplicate-text-in-tmap
library(dplyr)
USA <- USA %>%  
  st_cast()

#### Sites #####
#venom site location data frame
dat <- read.csv("Crotalus_pyrrhus_localities.csv", header=TRUE)

#Convert data frame to simple features (sf).
sites <- sf::st_as_sf(dat, coords = c("Longitude", "Latitude"))


#### Make Map ####

#Fonts
#https://cran.r-project.org/web/packages/extrafont/readme/README.html
library(extrafont)
font_import() #import fonts installed on system
fonts() #vector of font names
loadfonts() #Register fonts

library(tmap)

bbox_USA <- st_bbox(c(xmin = -125.390631, xmax = -108.9, 
                      ymin = 31, ymax = 38), crs = st_crs(4326))

USA_map <- tm_shape(USA.elevation, bbox=bbox_USA) + 
  tm_raster(title = "Elevation (m)", breaks=c(-Inf,0,500,1000,2000,3000,4000,5000),
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1")) + #USA.elevation is raster.
  tm_shape(USA)+
  tm_polygons(fill_alpha=0)+ #what USA consists of 
  #tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+ hide as adding state names via tm_titles to manually choose position
  tm_shape(sites)+
  tm_symbols(size=1 ,col="red",fill="red",fill_alpha = 0.5)+
  tm_text("Locality", size = 0.5, fontfamily = "Times", angle=45)+
  tm_layout(text.fontfamily = "Times New Roman",legend.position = c(0.05,0.5),
            legend.title.size = 1.25, legend.text.size = 1, 
            legend.text.fontface = "plain",
            frame = FALSE) +
  tm_title(text = "California", size = 1, position = c(0.3,0.8))+ #Add multiple titles for states
  tm_title(text = "Arizona", size = 1, position = c(0.8,0.65))+
  tm_title(text = "Nevada", size =1, position = c(0.55,0.9))

USA_map

tmap_save(USA_map,filename = "USA_map1.png", dpi = 300)

tmap_save(USA_map,filename = "USA_map1.tiff", dpi = 600)

### Map 2 for photoshop ####

USA_map <- tm_shape(USA.elevation, bbox=bbox_USA) + 
  tm_raster(title = "Elevation (m)", breaks=c(-Inf,0,500,1000,2000,3000,4000,5000),
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1")) + #USA.elevation is raster.
  tm_shape(USA)+
  tm_polygons(fill_alpha=0)+ #what USA consists of 
  #tm_text("NAME_1", size = 1, alpha = 0.5, fontfamily = "Times")+ hide as adding state names via tm_titles to manually choose position
  tm_shape(sites)+
  tm_symbols(size=1 ,col="black",fill = "red",fill_alpha=1)+
  tm_layout(text.fontfamily = "Times New Roman",legend.position = c(0.05,0.5),
            legend.title.size = 1.25, legend.text.size = 1, 
            legend.text.fontface = "plain",
            frame = FALSE)

USA_map

tmap_save(USA_map,filename = "USA_map_black_red.tiff", dpi = 600)


