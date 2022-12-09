library(tidyverse)
library(ggrepel)
library(viridis)
library(cowplot)
library(ggmap)
library(patchwork)
library(readxl)
library(ggsci)

setwd("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analysis")

### MA data
#read in .csv files of zipcodes and population
MAtowns <- read_csv("MAtowns_no_bosneighborhoods.csv")  #reading in with consolidated Boston
MAtownpoptable <- read_csv("MAtownpoptable.csv")
#mgb.facilities <- read_xlsx("MGBfacilities.xlsx", sheet = "Sheet1")

#read in updating (twice weekly) .csv files of cases/testing
# allcovidtowns <- read_csv("allcovidtowns.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26")) %>% # MDPH town testing data avial from 2020-05-27
#   mutate(date=(lubridate::ymd(date)))

allcovidtowns <- read_csv("allcovidtowns_no_bosneighborhoods.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26")) %>% # MDPH town testing data avaiable from 2020-05-27
  mutate(date=(lubridate::ymd(date))) 

#Adding town_population to allcovidtowns
covidtowns1<-left_join(allcovidtowns, MAtownpoptable, by="Town")

covidtowns2<-left_join(covidtowns1, MAtowns%>% dplyr::select(-town_population), by="Town") %>% distinct(Town, date, .keep_all = TRUE)

#sorting by town and date, and incident cases in past week
covidtowns<-covidtowns2 %>% 
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% 
  mutate(Rate=(Count/town_population)*100000,
         Count1wk=Count-(lag(Count,1)),
         Count1wk=if_else(Count1wk<0, 0, Count1wk),
         incidence7day=Rate-lag(Rate,1), 
         incidence7day=if_else(incidence7day<0, 0, incidence7day), #some instances where cases removed in subsequent weeks (?changed patient residence info, errors)
         incidence1day=incidence7day/7,
         Tests1wk=Tests-(lag(Tests,1)),
         Tests2wk=Tests-(lag(Tests,2)),
         Count2wk=Count-(lag(Count,2)),
         Count2wk=if_else(Count2wk<0, 0, Count2wk),
         positivity1wk=ifelse(Tests1wk>0, Count1wk/Tests1wk, NA), #preventing NaN when Tests1wk are zero
         testingrate=100000*Tests1wk/town_population) %>%
  filter(date > as.Date("2020-05-27") & Town != "Boston")  #first row is NA for all as calculations lagged 1 week
#allcovidtowns.long<-covidtowns #make a dataset with all observations


covidtowns %>%
  filter(date== max(date)) %>%
  distinct(Town, .keep_all = TRUE) %>%
  group_by() %>%
  filter(!is.na(incidence1day))%>%
  summarise(incidence1day = quantile(incidence1day, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))

### BOS data


boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

#read in .csv files of zipcodes and population
MAtownsbos <- read_csv("MAtowns.csv") %>% filter(Town %in% boston)

MAtownbospoptable <- read_csv("MAtownpoptable.csv")  %>% filter(Town %in% boston)

#read in updating (twice weekly) .csv files of cases/testing
allcovidtownsbos <- read_csv("allcovidtowns.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26") & Town %in% boston) %>% # MDPH town testing data available from 2020-05-27
  mutate(date=(lubridate::ymd(date)))



#Adding town_population to allcovidtownsbos
covidtownsbos1<-left_join(allcovidtownsbos, MAtownbospoptable, by="Town")

covidtownsbos2<-left_join(covidtownsbos1, MAtownsbos%>% dplyr::select(-town_population), by="Town") %>% distinct(Town, date, .keep_all = TRUE)

#sorting by town and date, and incident cases in past week
covidtownsbos<-covidtownsbos2 %>%
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% 
  mutate(Rate=(Count/town_population)*100000,
         Count1wk=Count-(lag(Count,1)),
         Count1wk=if_else(Count1wk<0, 0, Count1wk),
         incidence7day=Rate-lag(Rate,1), 
         incidence7day=if_else(incidence7day<0, 0, incidence7day), #some instances where cases removed in subsequent weeks (?changed patient residence info, errors)
         incidence1day=incidence7day/7,
         Tests1wk=Tests-(lag(Tests,1)),
         Tests2wk=Tests-(lag(Tests,2)),
         Count2wk=Count-(lag(Count,2)),
         Count2wk=if_else(Count2wk<0, 0, Count2wk),
         positivity1wk=ifelse(Tests1wk>0, Count1wk/Tests1wk, NA), #preventing NaN when Tests1wk are zero
         testingrate=100000*Tests1wk/town_population,
         date = case_when(
           date < as.Date("2020-12-10") ~ date -1, # aligning dates of BPHC and MDPH
            TRUE ~ date)) %>%
  filter(date > as.Date("2020-05-27"))  #first row is NA for all as calculations lagged 1 week
#allcovidtownsbos.long<-covidtownsbos #make a dataset with all observations

'%!in%' <- Negate('%in%')

combined<-rbind(covidtowns %>% filter(Town %!in% boston), covidtownsbos %>% mutate(zipcode = as.character(zipcode))) %>%
  group_by(Town) %>%
  arrange(date) %>%
  mutate(incidence2wkchange=incidence1day-lag(incidence1day,2),
         incidence1wkchange=incidence1day-lag(incidence1day,1),
         incidence2wkchange.pct=100*incidence2wkchange/lag(incidence1day,2))

MAtownSESbos<- read_csv("MAtownSES.csv") %>%
  filter(Town %in% boston) %>%
  dplyr::rename(SVI_SES=RPL_THEME1.town, SVI_ages_disability=RPL_THEME2.town,
         SVI_minority=RPL_THEME3.town, SVI_house=RPL_THEME4.town,
         SVI_overall=RPL_THEMES.town)  %>%
  # Socioeconomic – RPL_THEME1
  # Household Composition & Disability – RPL_THEME2
  # Minority Status & Language – RPL_THEME3
  # Housing Type & Transportation – RPL_THEME4
  # Overall tract rankings:  RPL_THEMES.
  mutate(
    quartile.SVI_SES=cut(SVI_SES, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_ages_disability=cut(SVI_ages_disability, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_minority=cut(SVI_minority, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_house=cut(SVI_house, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_overall=cut(SVI_overall, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE)
  ) 

`%!in%` = Negate(`%in%`)
MAtownSES<- read_csv("MAtownSES.csv") %>%
  filter(Town %!in% boston) %>%
  dplyr::rename(SVI_SES=RPL_THEME1.town, SVI_ages_disability=RPL_THEME2.town,
         SVI_minority=RPL_THEME3.town, SVI_house=RPL_THEME4.town,
         SVI_overall=RPL_THEMES.town)  %>%
  # Socioeconomic – RPL_THEME1
  # Household Composition & Disability – RPL_THEME2
  # Minority Status & Language – RPL_THEME3
  # Housing Type & Transportation – RPL_THEME4
  # Overall tract rankings:  RPL_THEMES.
  mutate(
    quartile.SVI_SES=cut(SVI_SES, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_ages_disability=cut(SVI_ages_disability, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_minority=cut(SVI_minority, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_house=cut(SVI_house, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_overall=cut(SVI_overall, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE)
  ) 

SES<- rbind(MAtownSES, MAtownSESbos)

combined<-left_join(combined, SES, by="Town") 



# towns.by.SVI<- combined %>%
#   filter(date==last(date)) %>%
#   distinct(Town, .keep_all = TRUE) %>%
#   arrange(SVI_overall) %>%
#   dplyr::select(date, Town,quartile.SVI_overall, town_population, Count2wk, SVI_overall, eohhs_name) 
# write_csv(towns.by.SVI, "towns.by.SVI.csv")
# 
# towns.by.SVI %>%
#   group_by(quartile.SVI_overall) %>%
#   summarise(Cases2wks=sum(Count2wk))

metrotowns<-c("Chelsea", "Allston Brighton", "Charlestown","Back Bay Downtown", "East Boston", "Dorchester Uphams", "Dorchester Codman", "Fenway", "Hyde Park",
              "Jamaica Plain", "Mattapan", "Roslindale", "Roxbury", "South Boston", "South End", "West Roxbury", "Brookline", "Newton", "Watertown",
              "Cambridge", "Somerville", "Medford", "Everett", "Winthrop", "Revere", "Weston", "Wellesley", "Waltham", "Belmont", "Arlington", "Malden",
              "Needham", "Dover", "Dedham", "Milton", "Quincy", "Nahant", "Lynn", "Saugus", "Melrose", "Woburn", "Lexington", "Lincoln", "Concord",
              "Lincoln", "Sudbury", "Wayland", "Natick", "Sherborn", "Westwood", "Norwood", "Canton", "Randolph",
              "Weymouth", "Hingham", "Hull", "Braintree", "Winchester", "Wakefield", "Stoneham", "Salem", "Swampscott",
              "Marblehead", "Framingham", "Bedford", "Burlington", "Hanscom Afb", "Medfield", "Sherborn", "Walpole")


townsSVI<-combined %>%
  filter(Town %in% metrotowns) %>% 
  distinct(Town, .keep_all = TRUE)

metroSVIoverall<-combined %>%
  filter(Town %in% metrotowns) %>%
  group_by(date, quartile.SVI_SES) %>%
  dplyr::summarise(pop=sum(town_population),
            Cases=sum(Count1wk)
  ) %>%
  mutate(incidence1day=((Cases/pop)*100000)/7,
         example=ifelse(quartile.SVI_SES==1, "Newton/Brookline",
                        ifelse(quartile.SVI_SES==2, "Cambridge/Somerville",
                               ifelse(quartile.SVI_SES==3, "Quincy/Allston",         
                                      ifelse(quartile.SVI_SES==4, "Lynn/Dorchester", "Blank"))))
  )




metroSVIplot<- metroSVIoverall %>% 
  ggplot( aes(x=date, y=incidence1day, group=quartile.SVI_SES, color= as.factor(quartile.SVI_SES))) +
  theme_classic() +  theme( plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.2, se=FALSE) +
  labs(x="Date", y="Daily Incidence, per 100,000",
       title="Incidence of Covid-19 by SES Vulnerabilty in Metro Boston",
       subtitle = paste0("Last data available: ", max(metroSVIoverall$date)),
       caption="Source: Massachusetts Department of Public Health, Boston Public Health Commission, CDC Social Vulnerability Index, US Census" )+
  scale_color_jama(palette = c("default"))+
  geom_label_repel(data=metroSVIoverall  %>% filter(date == last(metroSVIoverall$date)), 
                   aes(label=paste0("SVI SES Q", quartile.SVI_SES,"\n",round(incidence1day,1),"/100,000\n","(eg, ",example, ")"),  x = date, y = incidence1day, 
                       color=as.factor(quartile.SVI_SES)),min.segment.length = 0, 
                   xlim=c(Sys.Date()+40, Sys.Date()+200), max.overlaps = 50) +
  guides(color="none") +
  coord_cartesian( xlim=c(as.Date(NA), Sys.Date()+200), ylim = c(0,300))+
  scale_x_date(breaks = scales::pretty_breaks(10),  labels=scales::label_date_short())+
  scale_y_continuous(breaks = scales::pretty_breaks(10))

metroSVIplot
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/SVI incidence.pdf", width = 16, height=10)



townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
townsgeo<-read.csv("townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(townsgeo, townsgeobos)


CombinedGeo <-  left_join(matownsgeo, combined %>% filter(date==last(date)), by = "Town")

# #get centroids
# library(data.table)
# library(geosphere)
# cent<- CombinedGeo %>% 
#   dplyr::select(long, lat, Town)
# setDT(cent)
# 
# #make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
# findCentroid <- function(long, lat, ...){
#   centroid(cbind(long, lat), ...)
# }
# 
# centroids<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
# centroids<-centroids %>%
#   distinct(Town, xname, yname) %>%
#   mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
#          yname= ifelse(Town=="Nantucket", yname-0.02, yname),
#          yname= ifelse(Town=="Chilmark", yname+0.02, yname),
#          yname= ifelse(Town=="Boston", yname-0.02, yname),
#          xname= ifelse(Town=="Boston", xname+0.04, xname),
#          xname= ifelse(Town=="Dorchester Uphams", xname-0.04, xname),
#          xname= ifelse(Town=="South Boston", xname-0.03, xname),
#          xname= ifelse(Town=="Hull", xname-0.05, xname),
#          xname= ifelse(Town=="Salem", xname-0.02, xname),
#          xname= ifelse(Town=="Westport", xname-0.02, xname),
#          yname= ifelse(Town=="Provincetown", yname+0.01, yname),
#          yname= ifelse(Town=="South Boston", yname-0.01, yname))


#read_csv("town.neighborhood.labels.csv")

CombinedGeo<-left_join(CombinedGeo, read_csv("town.neighborhood.labels.csv"), by="Town")


incid.metroboston<-CombinedGeo #%>% filter(Town %in% metrotowns)

mgb.facilities<- read_excel("MGBfacilities.xlsx")

#metro boston plot
metroboston<- ggplot()+
  geom_polygon(data = incid.metroboston, 
               aes(x = long, y = lat, group = group, fill=incidence1day), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = incid.metroboston %>%
               distinct(Town, .keep_all = TRUE),
    size=2.0, color= "black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(incidence1day,0), ")", sep="")))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0,41),breaks = c(0, 10, 20, 30, 40),
                       # limits=c(0,80),breaks = c(0, 20, 40, 60, 80),
                       #limits=c(0,210),breaks = c( 50, 100, 150, 200),
                       #limits=c(0,25),breaks = c(0, 5, 10, 15, 20, 25),
                       name="Average Daily Covid-19\nincidence in past week\nper 100,000") +
  labs(title="Recent Covid-19 Diagnoses in Metro Boston",
       subtitle = paste0("Cases diagnosed from ",max(incid.metroboston$date)-12, " to ", max(incid.metroboston$date)-5),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
metroboston
ggsave("metroboston.pdf", height = 8, width =12)
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/metroboston incidence.jpg", plot = last_plot(), device = "jpeg",
#        scale = 1,  units = c("in"), height = 8.67, width = 8.67,
#        dpi = 300, limitsize = TRUE)

# #metro boston plot
# metroboston.svi<- ggplot()+
#   geom_polygon(data = incid.metroboston, 
#                aes(x = long, y = lat, group = group, fill=SVI_SES*100), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_text( data = incid.metroboston %>%
#                distinct(Town, .keep_all = TRUE),
#              size=2.0, color= "black",
#              aes(x=xname, y=yname,  label=paste(Town,"\n(", round(SVI_SES*100,0), ")", sep="")))+
#   coord_quickmap() + theme_void() + 
#   # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
#   #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
#   #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
#   
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        #limits=c(0,41),breaks = c(0, 10, 20, 30, 40),
#                        # limits=c(0,80),breaks = c(0, 20, 40, 60, 80),
#                        limits=c(0,90),breaks = c(0, 25, 50, 75 ),
#                        #limits=c(0,25),breaks = c(0, 5, 10, 15, 20, 25),
#                        name="Social Vulnerability\nIndex, (percentile)") +
#   labs(title="Socioeconomic Vulnerabilty in Metro Boston",
#        subtitle = "Social Vulnerability Index, SES Domain",
#        caption="Source: Centers from Disease Control and Prevention, 2018. Averaged over commmunity.") +
#   theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
#         legend.direction = "vertical", aspect.ratio = 0.75) +
#   coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
# metroboston.svi
# metro.combined.svi<-metroboston + metroboston.svi + plot_layout(ncol=2)
# ggsave("metroboston.svi.pdf", height = 8, width =24)
# 
# 
# #metro boston count plot
# metroboston.count<- ggplot()+
#   geom_polygon(data = incid.metroboston, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_text( data = incid.metroboston %>%
#                distinct(Town, .keep_all = TRUE),
#              size=1.8, color= "black",
#              aes(x=xname, y=yname,  label=paste(Town,"\n(", round(Count2wk,0), ")", sep="")))+
#   coord_quickmap() + theme_void() + 
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000, 1500, 2000),
# 
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="Recent Covid-19 Diagnoses in Metro Boston",
#        subtitle = paste0("Cases diagnosed from ",max(incid.metroboston$date)-19, " to ", max(incid.metroboston$date)-5),
#        caption="Source: BPHC and MDPH") +
#   theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
#         legend.direction = "vertical", aspect.ratio = 0.75) +
#   coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
# metroboston.count
# ggsave("metroboston.count.pdf", height =10,  width =20)
# 
# 
# 
# metroboston.count.mgb<- ggplot()+
#   geom_polygon(data = incid.metroboston, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% filter(
#     str_detect(Facility.short, "SRH") == FALSE & str_detect(Facility.short, "Spaulding") == FALSE &
#       long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62), aes(x=long, y=lat), shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000, 1500, 2000),
#                        
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Outpatient Locations in Metro Boston",
#        subtitle = paste0("Cases diagnosed from ",max(incid.metroboston$date)-19, " to ", max(incid.metroboston$date)-5),
#        caption="Source: BPHC and MDPH") +
#   geom_label_repel(data = mgb.facilities %>% filter(
#     str_detect(Facility.short, "SRH") == FALSE & str_detect(Facility.short, "Spaulding") == FALSE &
#       long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62) , size=2.2, min.segment.length = 0, max.overlaps = 200,
#     aes(x=long, y=lat,  label=Facility.short))+
#   theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
#         legend.direction = "vertical", aspect.ratio = 0.75) +
#   coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
# metroboston.count.mgb
# ggsave("metroboston.count.mgb.pdf", height =10,  width =20)
# 
# metroboston.count.pharm<- ggplot()+
#   geom_polygon(data = incid.metroboston, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% filter(Pharmacy == 1 &
#       long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62), aes(x=long, y=lat), shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000, 1500, 2000),
#                        
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Outpatient Pharmacies in Metro Boston",
#        subtitle = paste0("Cases diagnosed from ",max(incid.metroboston$date)-19, " to ", max(incid.metroboston$date)-5),
#        caption="Source: BPHC and MDPH") +
#   geom_label_repel(data = mgb.facilities %>% filter(Pharmacy == 1 &
#       long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62) , size=2.2, min.segment.length = 0, max.overlaps = 200,
#     aes(x=long, y=lat,  label=Facility.short))+
#   theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
#         legend.direction = "vertical", aspect.ratio = 0.75) +
#   coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
# metroboston.count.pharm
# ggsave("metroboston.count.mgb.pdf", height =10,  width =20)

#change metro boston plot
metroboston.change<- ggplot()+
  geom_polygon(data = incid.metroboston %>%
               mutate(incidence1wkchange=if_else(incidence1wkchange< -15, -15, incidence1wkchange)), 
               aes(x = long, y = lat, group = group, fill=incidence1wkchange), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = incid.metroboston %>%
               distinct(Town, .keep_all = TRUE),
             size=2, color = "black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(incidence1wkchange,0), ")", sep="")))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='RdYlGn', direction=-1, na.value = "#a50026",
                      limits=c(-15,15),breaks = c(-15, 0, 15),
                       name="One-week change\nin Covid-19 incidence\nper 100,000") +
  labs(title="Change in Covid-19 Incidence in Metro Boston",
       subtitle = paste0("Cases diagnosed from ",max(incid.metroboston$date)-12, " to ", max(incid.metroboston$date)-5),
       caption="Source: BPHC and MDPH")+
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical",aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
  #coord_cartesian(xlim=c(-71.8, -70.7), ylim = c(42.15, 42.65))
metroboston.change
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/metroboston change.pdf", width=16, height=9)



# ggplot()+
#   geom_polygon(data = incid.metroboston, 
#                aes(x = long, y = lat, group = group, fill=testingrate), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_text( data = incid.metroboston %>%
#                distinct(Town, .keep_all = TRUE), 
#              size=1.8,
#              aes(x=xname, y=yname,  label=paste(Town,"\n(", round(incidence1day,1), ")", sep="")))+
#   coord_quickmap() + theme_void() + 
#   # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
#   #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
#   #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
#   
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                       # limits=c(0,80),breaks = c(0, 25, 50, 75),
#                        name="Average Daily Covid-19 incidence over\nin past week per 100,000 population") +
#   labs(title="Recent Covid-19 Diagnoses in Metro Boston",
#        subtitle = paste0("Cases for week preceeding ",max(incid.metroboston$date)),
#        caption="Source: BPHC and MDPH")
# ggsave("metrobostontesting.pdf", width=18, height=12)

#All MA plot
allma<- ggplot()+
  geom_polygon(data = CombinedGeo, 
               aes(x = long, y = lat, group = group, fill=incidence1day), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
  geom_label_repel(data = CombinedGeo %>% distinct(Town, xname, yname, incidence1day, Count1wk) %>%
                     filter(Count1wk>100),
                 size=2.2, min.segment.length = 0,
                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1, max.overlaps = 60) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0,41),breaks = c(0, 10, 20, 30, 40),
                       #limits=c(0,80),breaks = c(0, 20, 40, 60, 80),
                       #limits=c(0,400),breaks = c(0, 100, 200, 300, 400),
                       #limits=c(0,210),breaks = c( 50, 100, 150, 200),
                       #limits=c(0,25),breaks = c(0, 5, 10, 15, 20, 25),
                       name="Average Daily Covid-19 incidence\nin past week per 100,000") +
  labs(title="Recent Covid-19 Diagnoses in Massachusetts",
       subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-12, " to ", max(CombinedGeo$date, na.rm = TRUE)-5, 
                         " | Communities with >100 cases labelled"),
       caption="Source: BPHC and MDPH")
allma
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.pdf", width=16, height=9)


# allma<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=incidence1day), 
#                colour = "white", alpha = 0.9, size = .25) +
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = CombinedGeo %>% distinct(Town, xname, yname, incidence1day, Count1wk) %>% 
#                      filter(Count1wk>200 & incidence1day>300), 
#                    size=2.2, min.segment.length = 0,
#                    aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1, max.overlaps = 60) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        #limits=c(0,41),breaks = c(0, 10, 20, 30, 40),
#                        #limits=c(0,80),breaks = c(0, 20, 40, 60, 80),
#                        limits=c(0,400),breaks = c(0, 100, 200, 300, 400),
#                        #limits=c(0,25),breaks = c(0, 5, 10, 15, 20, 25),
#                        name="Average Daily Covid-19 incidence\nin past week per 100,000") +
#   labs(title="Recent Covid-19 Diagnoses in Massachusetts",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-12, " to ", max(CombinedGeo$date, na.rm = TRUE)-5, 
#                          " | Communities with >200 cases and incidence >300 labelled"),
#        caption="Source: BPHC and MDPH")
# allma
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.pdf", width=16, height=9)
# 
# 
#change mass plot
allma.change<- ggplot()+
  geom_polygon(data = CombinedGeo %>%
                 mutate(incidence1wkchange=if_else(incidence2wkchange< -15, -15, incidence1wkchange)),
               aes(x = long, y = lat, group = group, fill=incidence1wkchange),
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
  geom_label_repel(data = CombinedGeo %>% distinct(Town, xname, yname, incidence1wkchange, Count1wk) %>%
                     filter(incidence1wkchange>10 & Count1wk>50),
                   size=2.2, min.segment.length = 0,
                   aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1, max.overlaps = 60) +
  scale_fill_distiller(palette='RdYlGn', direction=-1, na.value = "#a50026",
                       limits=c(-15,15),breaks = c(-15, 0, 15),
                       name="One-week change\nin Covid-19 incidence\nper 100,000") +
  labs(title="Change in Covid-19 Incidence in Massachusetts Towns",
       subtitle = paste0("Most recent data: ",max(CombinedGeo$date)-5, " | Towns with >10 per 100,000 increase and >50 weekly cases labeled"),
       caption="Source: BPHC and MDPH")
allma.change
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MA change.pdf", width=16, height=9)

# 
# allma.count<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = CombinedGeo %>% distinct(Town, xname, yname, Count2wk) %>%
#                      filter(Count2wk>1500 ),
#                    size=2.2, min.segment.length = 0,
#                    aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count2wk,0), " cases", sep="")), nudge_x=0.1, max.overlaps = 60) +
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000,  2000),
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="Recent Covid-19 Diagnoses in Massachusetts",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-19, " to ", max(CombinedGeo$date, na.rm = TRUE)-5, 
#                          " | Communities with >1500 cases labeled"),
#        caption="Source: BPHC and MDPH")
# allma.count
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.count.pdf", width=16, height=9)
# 
# allma.count.mgb<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% mutate(metro = if_else(long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62, 1, 0)) %>% filter(
#     str_detect(Facility.short, "SRH") == FALSE & str_detect(Facility.short, "Spaulding") == FALSE & str_detect(Facility.short, "Wentworth") == FALSE &
#       metro == 0), aes(x=long, y=lat), shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = mgb.facilities %>% mutate(metro = if_else(long < -70.6  & long > -71.4 & lat > 42.18 & lat < 42.62, 1, 0)) %>% filter(
#     str_detect(Facility.short, "SRH") == FALSE & str_detect(Facility.short, "Spaulding") == FALSE & str_detect(Facility.short, "Wentworth") == FALSE &
#       metro == 0) , size=2.2, min.segment.length = 0, max.overlaps = 200,
#     aes(x=long, y=lat,  label=Facility.short))+
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000,  2000),
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Locations outside of Boston and Covid-19 Diagnoses in Massachusetts",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-19, " to ", max(CombinedGeo$date, na.rm = TRUE)-5, 
#                          " | Communities with >1500 cases labeled"),
#        caption="Source: BPHC and MDPH")
# allma.count.mgb
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.count.mgb.pdf", width=16, height=9)
# 
# allma.count.pharm<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% filter(Pharmacy == 1), aes(x=long, y=lat), shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = mgb.facilities %>% filter(Pharmacy == 1), size=2.2, min.segment.length = 0, max.overlaps = 200,
#     aes(x=long, y=lat,  label=Facility.short))+
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000,  2000),
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Outpatient Pharmacy Locations",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-19, " to ", max(CombinedGeo$date, na.rm = TRUE)-5, 
#                          " | Communities with >1500 cases labeled"),
#        caption="Source: BPHC and MDPH")
# allma.count.pharm
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.count.mgb.pdf", width=16, height=9)
# 
# 
# 
# allma.count.IV<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% filter(IV == 1), aes(x=long, y=lat), size=3, shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = mgb.facilities %>% filter(IV == 1) , size=2.2, min.segment.length = 0, max.overlaps = 200,
#     aes(x=long, y=lat,  label=Facility.short))+
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000,  2000),
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Outpatient Covid-19 IV Treatment Locations",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-19, " to ", max(CombinedGeo$date, na.rm = TRUE)-5),
#        caption="Source: BPHC and MDPH")
# allma.count.IV
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.count.IV.pdf", width=16, height=9)
# 
# allma.count.oral<- ggplot()+
#   geom_polygon(data = CombinedGeo, 
#                aes(x = long, y = lat, group = group, fill=Count2wk), 
#                colour = "white", alpha = 0.9, size = .25) +
#   geom_point(data= mgb.facilities %>% filter(IV == 1) %>% filter(Facility.short == "Massachusetts General Hospital"), aes(x=long, y=lat), size=3, shape=21, fill= "#4292c6")+
#   coord_quickmap() + theme_void() + 
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
#   geom_label_repel(data = mgb.facilities %>% filter(IV == 1) %>% filter(Facility.short == "Massachusetts General Hospital") , size=2.2, min.segment.length = 0, max.overlaps = 200,
#                    aes(x=long, y=lat,  label=Facility.short))+
#   scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
#                        limits=c(0,2001),breaks = c(0, 500, 1000,  2000),
#                        name="Confirmed Cases\nin Past 2 weeks") +
#   labs(title="MGB Outpatient Covid-19 IV Treatment Locations",
#        subtitle = paste0("Cases diagnosed from ", max(CombinedGeo$date, na.rm = TRUE)-19, " to ", max(CombinedGeo$date, na.rm = TRUE)-5),
#        caption="Source: BPHC and MDPH")
# allma.count.oral
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MAincidence.count.oral.pdf", width=16, height=9)



library(officer)
library(patchwork)
slides <- read_pptx("wide template.pptx")

# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = metroSVIplot,
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = metroboston.count, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = metroboston.count.mgb, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = metroboston.count.pharm, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = allma.count, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = allma.count.mgb, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = allma.count.pharm, 
#                  location = ph_location_fullsize() )

slides <- add_slide(slides)
slides<- ph_with(x = slides, value = allma.count.IV, 
                 location = ph_location_fullsize() )

slides <- add_slide(slides)
slides<- ph_with(x = slides, value = allma.count.oral, 
                 location = ph_location_fullsize() )


print(slides, target = "20220113 covid treatment locations.pptx")

