library(tidyverse)
library(ggrepel)
library(viridis)
library(cowplot)
library(ggmap)
library(patchwork)
library(readxl)
library(ggsci)
fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}
#setwd("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing")

### MA data
#read in .csv files of zipcodes and population
MAtowns <- read_csv("MAtowns.csv")  #reading in with consolidated Boston

outpatient.treatment.wt<-
  left_join(
      left_join(enclave.total %>% mutate(zipcode = fixzip(zip)), MAtowns %>% mutate(zipcode = fixzip(zipcode)), 
                by = "zipcode"),
    weights, # IPW weights of outpatient diagnosis
                by = c("MASS.cat", "age.cat", "vaxstatus.collapsed", "race.eth.collapsed", "highADI")) %>%

  mutate(high_risk = case_when(
      MASS.cat == "MASS 6 or greater" ~ 1, 
      severe.immunosuppression == 1 ~ 1, 
      MASS.cat == "MASS 4 and 5" & vaxstatus.collapsed == "Not fully vaccinated" ~ 1, 
      TRUE ~0)) %>%
  filter(outpatient.coviddx ==1) %>%
  select(outpt.wts, Town, zipcode, MASS.cat, age.cat, vaxstatus.collapsed, race.eth.collapsed, 
         highADI, covid_treat, high_risk)



## all patients
treatment.coverage<-
  outpatient.treatment.wt %>%
  group_by(Town) %>%
  summarise(
    cases = sum(outpt.wts, na.rm = TRUE),
    treatment = sum(covid_treat, na.rm = TRUE)
  ) %>%
  mutate(pct.treat = treatment/cases, 
         pct.treat = if_else(cases <200, as.numeric(NA), pct.treat))


townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
townsgeo<-read.csv("townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(townsgeo, townsgeobos)


CombinedGeo <-  left_join(matownsgeo, treatment.coverage, by = "Town")
CombinedGeo<-left_join(CombinedGeo, read_csv("town.neighborhood.labels.csv"), by="Town")


metroboston.treat.all <- ggplot()+
  geom_polygon(data = CombinedGeo, 
               aes(x = long, y = lat, group = group, fill=pct.treat*100), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = CombinedGeo %>%
               distinct(Town, .keep_all = TRUE),
             size=2.0, color= "black",
             aes(x=xname, y=yname, 
          label = if_else(is.na(pct.treat), Town, paste(Town,"\n(", round(pct.treat*100,0), "%)", sep=""))))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='BuGn', direction=1,
                       limits=c(5,20),breaks = c(5, 10, 15, 20),
                       name="Percent recieving\noutpatient treatment") +
  labs(title="Recent Covid-19 Diagnoses and Treatment in Metro Boston, MGB",
       subtitle = "MGB Covid Cases, April to October 2022 | Capture-recapture methods to estimate complete COVID+ population",
       caption="Source: MGB COVID Enclave") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.6, -70.6), ylim = c(42.18, 42.62))
metroboston.treat.all
ggsave("metroboston.treat.all.pdf", width =11.7, height = 9)

## high risk patients
treatment.coverage.hr<-
  outpatient.treatment.wt %>%
  filter(high_risk ==1) %>%
  group_by(Town) %>%
  summarise(
    cases = sum(outpt.wts, na.rm = TRUE),
    treatment = sum(covid_treat, na.rm = TRUE)
  ) %>%
  mutate(pct.treat = treatment/cases, 
         pct.treat = if_else(cases <100, as.numeric(NA), pct.treat))



CombinedGeo.hr <-  left_join(matownsgeo, treatment.coverage.hr, by = "Town")
CombinedGeo.hr<-left_join(CombinedGeo.hr, read_csv("town.neighborhood.labels.csv"), by="Town")


metroboston.treat.hr<- ggplot()+
  geom_polygon(data = CombinedGeo.hr, 
               aes(x = long, y = lat, group = group, fill=pct.treat*100), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = CombinedGeo.hr %>%
               distinct(Town, .keep_all = TRUE),
             size=2.0, color= "black",
             aes(x=xname, y=yname, 
                 label = if_else(is.na(pct.treat), Town, paste(Town,"\n(", round(pct.treat*100,0), "%)", sep=""))))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='BuGn', direction=1,
                       limits=c(5,20),breaks = c(5, 10, 15, 20),
                       name="Percent recieving\noutpatient treatment") +
  labs(title="High Risk Covid-19 Diagnoses and Treatment in Metro Boston, MGB",
       subtitle = "High Risk MGB Covid Cases (MASS 4+), April to October 2022 | Capture-recapture methods to estimate complete COVID+ population",
       caption="Source: MGB COVID Enclave") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
metroboston.treat.hr

ggsave("metroboston.treat.hr.pdf", width =11.7, height = 9)


ma.treat.hr<- ggplot()+
  geom_polygon(data = CombinedGeo.hr, 
               aes(x = long, y = lat, group = group, fill=pct.treat*100), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = CombinedGeo.hr %>%
               distinct(Town, .keep_all = TRUE),
             size=2.0, color= "black",
             aes(x=xname, y=yname, 
                 label = if_else(is.na(pct.treat), Town, paste(Town,"\n(", round(pct.treat*100,0), "%)", sep=""))))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='BuGn', direction=1,
                       limits=c(5,20),breaks = c(5, 10, 15, 20),
                       name="Percent recieving\noutpatient treatment") +
  labs(title="High Risk Covid-19 Diagnoses and Treatment in Metro Boston, MGB",
       subtitle = "High Risk MGB Covid Cases (MASS 4+), April to October 2022 | Capture-recapture methods to estimate complete COVID+ population",
       caption="Source: MGB COVID Enclave") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) 
ma.treat.hr

ggsave("ma.treat.hr.pdf", width =20, height = 12)


