library(tidyverse)
library(ggrepel)
library(viridis)
library(ggsci)
library(cowplot)
library(ggmap)
library(patchwork)
library(readxl)

MAtowns <- read_csv("MAtowns.csv")  #reading in with consolidated Boston
#MAtownpoptable <- read_csv("MAtownpoptable.csv")

MAtownSES<- read_csv("MAtownSES.csv") %>%
  rename(SVI_SES=RPL_THEME1.town, SVI_ages_disability=RPL_THEME2.town,
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

MAtownsbos<-left_join(
  MAtowns %>%
    distinct(Town, .keep_all = TRUE),
  MAtownSES, by="Town")%>%
  distinct(Town, .keep_all = TRUE) %>%
  dplyr::select(-zipcode)

BPHC <-left_join(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="BPHC current positivity", guess_max = 10000) %>%
                   distinct(Town, date, .keep_all = TRUE) %>%
                   dplyr::select(-zipcode),
                 MAtownsbos, by="Town")%>%
  mutate(first.testdate.included = (lubridate::ymd(first.testdate.included)),
          last.testdate.included = (lubridate::ymd(last.testdate.included)),
          report.date = (lubridate::ymd(report.date)),
         date = (lubridate::ymd(date)),
         cases = round(currenttests*currentpositivity,0),
         testingrate = currenttests*100000/town_population
  )

BPHCmatch<-BPHC%>%
  distinct(Town, date, .keep_all = TRUE) %>%
  group_by(date) %>%
  mutate(
    rankpositivity= dense_rank(desc(currentpositivity)),
    ranktesting= dense_rank(desc(testingrate))
    ) 


testing.targetBPHC<-inner_join(BPHCmatch %>%
                                dplyr::select(date, Town, rankpositivity, ranktesting, testingrate) %>%
                                filter(!is.na(rankpositivity) & !is.na(testingrate)), 
                               BPHCmatch %>% 
                                filter(!is.na(rankpositivity) & !is.na(testingrate)) %>%
                                rename(testingrate.target=testingrate) %>%
                                dplyr::select(date, testingrate.target, ranktesting),
                              by=c("date" ="date" , "rankpositivity"="ranktesting")
) %>%
  group_by()


BPHCmatch<- left_join(BPHCmatch, testing.targetBPHC  %>% 
                              dplyr::select(date, Town, testingrate.target), by= c("date", "Town")) %>%
  mutate(
    testingrate.target.cap=if_else(testingrate.target>10000, 10000, testingrate.target), #capping at 10% of population  (10000/100000) to reduce distortion of collegetown testing
    testingrate.delta=testingrate.target.cap-testingrate,
    testingrate.gap=if_else(testingrate.delta<0, 0, testingrate.delta), #excess testing set to 0, no benefit from earlier/later testing
    testingrate.excess=if_else(testingrate.delta<0, abs(testingrate.delta), 0),
    testingrate.excess=if_else(testingrate.excess>10000, 10000, testingrate.excess), #capping at 10% of population  (10000/100000) to reduce distortion of collegetown testing
    testing.gap=testingrate.gap*(town_population/100000),
    testing.excess=testingrate.excess*(town_population/100000),
    week=lubridate::week(date))

neighborhood.gap<-BPHCmatch %>%
 # filter(town_population >36000) %>%
  ggplot( aes(x=date, y=testingrate.gap, group=Town, color=Town) )+
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  scale_color_igv()+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.1, se=FALSE) +
  labs(x="Date", y="Testing Gap (per 100,000)",
       title="Relative Testing Gap, City of Boston, 2020", 
       subtitle = paste0("Last available data: ", last(BPHCmatch$date)))+
  guides(color=FALSE) +
  coord_cartesian(xlim=c(as.Date("2020-11-11"), Sys.Date()+35), ylim=c(0,NA))+
  geom_label_repel(data=BPHCmatch %>%  ungroup() %>% dplyr::filter(date==max(date)), 
                   aes(label=paste0(Town),  x = date, y = testingrate.gap, color=Town),
                   min.segment.length = 0,  xlim=c(Sys.Date(), as.Date(NA)))+
  scale_x_date(breaks = scales::pretty_breaks(10)) 
neighborhood.gap
ggsave("BOSgap.pdf", units = "in", width = 12, height=8)

neighborhood.positivity<-BPHCmatch %>%
  # filter(town_population >36000) %>%
  ggplot( aes(x=date, y=currentpositivity*100, group=Town, color=Town) )+
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  scale_color_igv()+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.1, se=FALSE) +
  labs(x="Date", y="Positivity (percent)",
       title="Current Positivity, City of Boston, 2020", 
       subtitle = paste0("Last available data: ", last(BPHCmatch$date)))+
  guides(color=FALSE) +
  coord_cartesian(xlim=c(as.Date("2020-11-11"), Sys.Date()+35), ylim=c(0,NA))+
  geom_label_repel(data=BPHCmatch %>%  ungroup() %>% dplyr::filter(date==max(date)), 
                   aes(label=paste0(Town),  x = date, y = currentpositivity*100, color=Town),
                   min.segment.length = 0,  xlim=c(Sys.Date(), as.Date(NA)))+
  scale_x_date(breaks = scales::pretty_breaks(10)) 
neighborhood.positivity
ggsave("BOSpositivity.pdf", units = "in", width = 12, height=8)

neighborhood.testing<-BPHCmatch %>%
  # filter(town_population >36000) %>%
  ggplot( aes(x=date, y=testingrate, group=Town, color=Town) )+
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  scale_color_igv()+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.1, se=FALSE) +
  labs(x="Date", y="Testing (per 100,000)",
       title="Testing Intensity, City of Boston, 2020", 
       subtitle = paste0("Last available data: ", last(BPHCmatch$date)))+
  guides(color=FALSE) +
  coord_cartesian(xlim=c(as.Date("2020-11-11"), Sys.Date()+35), ylim=c(0,NA))+
  # geom_label_repel(data=BPHCmatch %>%  ungroup() %>% dplyr::filter(date==max(date)), 
  #                  aes(label=paste0(Town),  x = date, y = testingrate, color=Town),
  #                  min.segment.length = 0) +
  geom_label_repel(data=t1<-BPHCmatch %>%  ungroup() %>% dplyr::filter(date==max(date)), 
                   aes(label=paste0(Town),  x = date, y = testingrate, color=Town),
                   min.segment.length = 0,  xlim=c(Sys.Date(), as.Date(NA))) +
  scale_x_date(breaks = scales::pretty_breaks(10))
neighborhood.testing
ggsave("BOStesting.pdf", units = "in", width = 12, height=8)

library(patchwork)
neighborhood.positivity + neighborhood.testing + neighborhood.gap + plot_layout(ncol=2)
ggsave("BOSplots.pdf", units = "in", width = 14, height=14)



#MAPS

townsgeo<-read.csv("townsgeo_onlybostonneigh.csv")


boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

townsgeobos<- townsgeo %>% filter(Town %in% boston)

EquitytownbosGeo <-  left_join(townsgeobos, BPHCmatch %>% ungroup() %>% 
                                 filter(!is.na(testingrate)) %>% 
                                 filter(date==max(date)), by = "Town") 

#maps



library(data.table)
library(geosphere)
cent<- EquitytownbosGeo %>% 
  dplyr::select(long, lat, Town)
setDT(cent)

#make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
findCentroid <- function(long, lat, ...){
  centroid(cbind(long, lat), ...)
}

centroidsbos<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
centroidsbos<-centroidsbos %>%
  distinct(Town, xname, yname) %>%
  mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
         yname= ifelse(Town=="Nantucket", yname-0.02, yname),
         yname= ifelse(Town=="Chilmark", yname+0.02, yname),
         yname= ifelse(Town=="Boston", yname-0.02, yname),
         xname= ifelse(Town=="Boston", xname+0.04, xname),
         xname= ifelse(Town=="Salem", xname-0.02, xname),
         xname= ifelse(Town=="Westport", xname-0.02, xname),
         yname= ifelse(Town=="Provincetown", yname+0.01, yname),
         yname= ifelse(Town=="East Boston", yname-0.01, yname),
         xname= ifelse(Town=="Dorchester Uphams", xname-0.045, xname),
         yname= ifelse(Town=="South Boston", yname-0.005, yname),
         xname= ifelse(Town=="South Boston", xname-0.02, xname),
  )

colors<-c(  "#80796BFF","#DF8F44FF", "#B24745FF", "#B24745FF")

EquitytownbosGeo<-left_join(EquitytownbosGeo, centroidsbos, by="Town") 

bos1<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=currentpositivity*100), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme( legend.position =  "none",
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0,15),breaks = c(0, 5, 10),
                       name="Test Positivity\nin past week") +
  geom_text( data = EquitytownbosGeo %>%
               distinct(Town, .keep_all = TRUE), 
             size=2.3, color="black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(currentpositivity*100,1), "%)", sep="")))+
  labs(title="Test Positivity in City of Boston", 
       subtitle = paste0("Percent of Tests Done | Week preceeding: ", last(EquitytownbosGeo$date)))

bos2<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=testingrate), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme( legend.position =  "none",
         panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(2000,8000),#breaks = c(0, 0.05, 0.1),
                       name="Testing Rate\nper 100,000\nin past week") +
  geom_text( data = EquitytownbosGeo %>%
               distinct(Town, .keep_all = TRUE), 
             size=2.3, color="black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(testingrate,0), ")", sep="")))+
  labs(title="Testing Intensity in City of Boston", 
       subtitle = paste0("Tests per 100,000 population | Week preceeding: ", last(EquitytownbosGeo$date)))

bos3<-ggplot()+
  geom_polygon(data = EquitytownbosGeo,
               aes(x = long, y = lat, group = group, fill=testingrate.gap), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme( legend.position =  "none",
         panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0, NA),#breaks = c(0, 0.05, 0.1),
                       name="Testing Gap\nper 100,000\nin past week") +
  geom_text( data = EquitytownbosGeo %>%
               distinct(Town, .keep_all = TRUE), 
             size=2.3, color="black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(testingrate.gap,0), ")", sep="")))+
  labs(title="Relative Testing Gap in City of Boston", 
       subtitle = paste0("Tests per 100,000 population to meet positivity ranking | Week preceeding: ", last(EquitytownbosGeo$date)))


library(patchwork)
bos1 + bos2 + bos3 + plot_layout(ncol=2)
ggsave("BOSgap.pdf", units = "in", width = 14, height=14)




#get cumulative gap and tests
# testingmatch %>%
#   #select(Town, Tests1wk, testing.gap)
#   filter(Town=="Lawrence") %>%
#   group_by(Town) %>%
#   summarise(gap=sum(testing.gap),
#       tests=sum(Tests1wk))
# 

metrotowns<-c("Chelsea", "Allston Brighton", "Charlestown","Back Bay Downtown", "East Boston", "Dorchester Uphams", "Dorchester Codman", "Fenway", "Hyde Park",
              "Jamaica Plain", "Mattapan", "Roslindale", "Roxbury", "South Boston", "South End", "West Roxbury", "Brookline", "Newton", "Watertown",
              "Cambridge", "Somerville", "Medford", "Everett", "Winthrop", "Revere", "Weston", "Wellesley", "Waltham", "Belmont", "Arlington", "Malden",
              "Needham", "Dover", "Dedham", "Milton", "Quincy", "Nahant", "Lynn", "Saugus", "Melrose", "Woburn", "Lexington", "Lincoln", "Concord",
              "Lincoln", "Sudbury", "Wayland", "Natick", "Sherborn", "Westwood", "Norwood", "Canton", "Randolph",
              "Weymouth", "Hingham", "Hull", "Braintree", "Winchester", "Wakefield", "Stoneham", "Salem", "Swampscott",
              "Marblehead", "Framingham", "Bedford", "Burlington", "Hanscom Afb", "Medfield", "Sherborn", "Walpole")

metroSVIoverall<-combined %>%
  filter(Town %in% metrotowns) %>%
  group_by(date, quartile.SVI_SES) %>%
  summarise(pop=sum(town_population),
            Cases=sum(Count1wk)
  ) %>%
  mutate(incidence1day1wk=((Cases/pop)*100000)/7,
         example=ifelse(quartile.SVI_SES==1, "Newton/Brookline",
                        ifelse(quartile.SVI_SES==2, "Cambridge/Somerville",
                               ifelse(quartile.SVI_SES==3, "Quincy/Allston",         
                                      ifelse(quartile.SVI_SES==4, "Lynn/Dorchester", "Blank"))))
  )

townsSVI<-combined %>%
  filter(Town %in% metrotowns) %>% 
  distinct(Town, .keep_all = TRUE)
library(ggsci)
metroSVIoverall %>% 
  ggplot( aes(x=date, y=incidence1day1wk, group=quartile.SVI_SES, color= as.factor(quartile.SVI_SES))) +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.2, se=FALSE) +
  labs(x="Date", y="Daily Incidence, per 100,000",
       title="Incidence of Covid-19 by SES Vulnerabilty in Metro Boston",
       subtitle = paste0("Last data available: ", max(metroSVIoverall$date)),
       caption="Source: Massachusetts Department of Public Health, CDC Social Vulnerability Index, US Census" )+
  scale_color_jama(palette = c("default"))+
  geom_label_repel(data=metroSVIoverall  %>% filter(date == last(metroSVIoverall$date)), 
                   aes(label=paste0("SVI SES Q", quartile.SVI_SES,"\n",round(incidence1day1wk,1),"/100,000\n","(e.g. ",example, ")"),  x = date, y = incidence1day1wk, 
                       color=as.factor(quartile.SVI_SES)),min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA))) +
  guides(color=FALSE) +
  coord_cartesian( xlim=c(min(metroSVIoverall$date), Sys.Date()+100))+
  scale_x_date(breaks = scales::breaks_pretty(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(10))

ggsave("~/Dropbox (Partners HealthCare)/R files/covid/SVI incidence.pdf", width = 10, height=10)
