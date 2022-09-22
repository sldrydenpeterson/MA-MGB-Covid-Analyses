library(tidyverse)
library(readxl)
library(broom)
library(sf)
library(rgdal)
library(rgeos)

#useful code chunk
`%!in%` = Negate(`%in%`)

#read in shapefile of zipcodes from Mass GIS
zips<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/zipcodes_nt/ZIPCODES_NT_POLY.shp")
zips <- gBuffer(zips, byid=TRUE, width=0) #fixing errors related to junction errors

#convert from spatial polygon DF to sf
zips.sf<-st_as_sf(zips, coords = c("lon", "lat"), crs = 4326)

#read in Town names
MAtowns<-read_csv("MAtowns.csv")

t1<-left_join(zips.sf, MAtowns, by= c("POSTCODE" ="zipcode"))

#collapse zipcodes into towns (for town with multiple)
t2<-t1 %>%
  group_by(Town) %>%
  dplyr::summarise(geometry=st_union(geometry))

#convert back to spatial polygon df
t3 <- as_Spatial (t2, IDs=t2$Town)
#project
t4 <- spTransform(t3, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#make tidy for ggplot
t5<-tidy(t4, region = c("Town"))

ggplot()+
  geom_polygon(data = t5 ,
               aes(x = long, y = lat, group = group, fill="grey80"), 
               colour = "white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() 


#read in town shapefile from Mass GIS
towns<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/townssurvey_shp/TOWNSSURVEY_POLY.shp")
towns <- gBuffer(towns, byid=TRUE, width=0) #fixing errors related to junction errors

towns.sp <- spTransform(towns, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
towns.df<-tidy(towns.sp, region = c("TOWN"))


#read in BOS neighborohood shape, but admin districts rather the public health units, using to have improved plot for Brighton
cityofboston<-readOGR("~/Dropbox (Partners HealthCare)/R files/Boston nightlight/Boston_Neighborhoods_StatAreas+2017/Boston_Neighborhoods.shp")
cityofboston <- gBuffer(cityofboston, byid=TRUE, width=0) #fixing errors related to junction errors
cityofboston.sf<-st_as_sf(cityofboston, coords = c("lon", "lat"), crs = 4326) %>%
    filter(Name=="Allston" | Name=="Brighton" | Name== "West Roxbury") %>%
    mutate(Town=Name,
         Town=ifelse(Town=="Brighton" | Town== "Allston", "Allston Brighton", "West Roxbury")) %>%
        group_by(Town)%>%
  dplyr::summarise(geometry=st_union(geometry))

cityofboston.3 <- as_Spatial(cityofboston.sf, IDs=cityofboston.sf$Town)
cityofboston.4 <- spTransform(cityofboston.3 , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Allston.WestRox<-tidy(cityofboston.4, region = c("Town"))



#remove boston from town map file to permit replacing with neighborhoods
towns.noboston<- towns.df %>% filter(id != "BOSTON")

#taking zipcode maps and extracting boston neighbohoords
towns.onlyboston<- t5 %>% filter(
    id == "East Boston" |
    id == "Charlestown" |
    id == "Back Bay Downtown" |
    id == "South Boston" |
    id == "South End" |
    id == "Fenway" |
    id == "Roxbury" |
    id == "Dorchester Uphams" |
    id == "Dorchester Codman" |
    id == "Mattapan" |
    id == "Jamaica Plain" |
    id == "Roslindale" |
    id == "Hyde Park" |
    id == "West Roxbury" #|  #issues with zipcode geography around BC/brookline makes these inaccurate.
   # id == "Allston Brighton" |
)

#merging to create full boston data set (West Rox still missing chunk bordering brookline)
townsgeo_onlybostonneigh <-full_join(towns.onlyboston, Allston.WestRox %>% filter(id !="West Roxbury")) %>%#adding in allston/brighton, West Rox still partially inaccurate
  filter( piece== "1") #remove non-populated islands and Fenway chunk in the 'neck'
  write_csv(townsgeo_onlybostonneigh, "townsgeo_onlybostonneigh.csv")

  #MA towns with boston neigh
townsgeo_withbostonneigh <-full_join(towns.noboston,towns.onlyboston) %>%
                   mutate(Town=str_to_title(id, locale = "en"))
  write_csv(townsgeo_withbostonneigh, "townsgeo_withbostonneigh.csv")
  
  #MA towns with boston as whole city
townsgeo<-towns.df %>%
  mutate(Town=str_to_title(id, locale = "en"))
write_csv(townsgeo, "townsgeo.csv")


