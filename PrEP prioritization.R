library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(tidycensus)
fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}

## Developing SVI resources for mAb program

#read in CDC SVI file for NE census districts
US2018_SVI <- read_csv("SVI2018_US.csv")

library(tidycensus)
##get zcta5 and census tract file for US
zcta5<- read_csv(url("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt")) %>% rename(FIPS = GEOID) %>% dplyr::select(FIPS, STATE, ZCTA5)  #new_name = old_name

# getting zips for NY, CT, RI, VT, NH, ME, and MA
NE.state.codes <-c("36", "09", "44","50","33","23","25")

NE.zips <- (zcta5  %>% filter(STATE %in% NE.state.codes))$ZCTA5

mazips<- read_csv("MAtowns.csv") %>%
  mutate(zipcode = fixzip(zipcode))

# #read in file of ZCTA5 to census tract
# ma_zcta5 <- read_csv("ma_zcta5.csv")  %>% rename(FIPS = GEOID) %>% dplyr::select(FIPS, ZCTA5)  #new_name = old_name

US_SVI<- full_join(US2018_SVI, zcta5, by = "FIPS") %>%
  filter(!is.na(ZCTA5)) %>%
  filter(RPL_THEMES > 0) %>%
  dplyr::select(E_TOTPOP,RPL_THEME1,RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES, ZCTA5)%>%
  rename(zipcode = ZCTA5)

US_SVI.zip<- US_SVI %>%
  group_by(zipcode) %>%
  dplyr::summarise(
    E_TOTPOP.zip=sum(E_TOTPOP),
    RPL_THEME1.zip=(sum(RPL_THEME1*E_TOTPOP)/E_TOTPOP.zip),
    RPL_THEME2.zip=(sum(RPL_THEME2*E_TOTPOP)/E_TOTPOP.zip),
    RPL_THEME3.zip=(sum(RPL_THEME3*E_TOTPOP)/E_TOTPOP.zip),
    RPL_THEME4.zip=(sum(RPL_THEME4*E_TOTPOP)/E_TOTPOP.zip),
    RPL_THEMES.zip=(sum(RPL_THEMES*E_TOTPOP)/E_TOTPOP.zip))%>%
  rename(SVI_SES=RPL_THEME1.zip,
         SVI_household.disability=RPL_THEME2.zip,
         SVI_minority.language=RPL_THEME3.zip,
         SVI_house.transport=RPL_THEME4.zip,
         SVI_overall=RPL_THEMES.zip) %>%
  distinct(zipcode, .keep_all = TRUE)

write_csv(US_SVI.zip %>% 
            mutate(
          SVI_SES_quartile = cut(SVI_SES, c(-1, 0.25, 0.5, 0.75, 1.1), labels = c("1", "2", "3", "4"))) %>%
            select(zipcode, SVI_SES, SVI_SES_quartile),
           "SES SVI US zip codes 2018.csv")



# identifyNE zipcodes for priority calling due to SES SVI
# MA SVI 75%ile ~ 58%ile for US standardized, using as cut-off 
highSES_SVI.NE<- left_join(US_SVI.zip %>%
  distinct(zipcode, .keep_all = TRUE) %>%
  mutate(highSES_SVI = if_else(SVI_SES >= 0.58, 1, 0)) %>%
  select(zipcode, highSES_SVI)%>% filter(zipcode %in% NE.zips),
  zcta5 %>% rename(zipcode = ZCTA5) %>%
    mutate(state = case_when(
      STATE == "36" ~ "NY",
      STATE == "09" ~ "CT",
      STATE == "44" ~ "RI",
      STATE == "50" ~ "VT",
      STATE == "33" ~ "NH",
      STATE == "23" ~ "ME",
      STATE == "25" ~ "MA")) %>%
    distinct(zipcode, state),
  by = "zipcode")
  
write_csv(highSES_SVI.NE, "highSES_SVI.NE.csv")

t1<-left_join(mazips %>%
                distinct(zipcode, .keep_all = TRUE), US_SVI.zip, by = "zipcode")
t2<- combined %>% distinct(zipcode)

t3<-left_join(read_csv("mAb_SVI.csv") %>% distinct(zipcode, .keep_all = TRUE), US_SVI.zip, by = "zipcode")

# #  Assessing correlation of SVI and longitudinal incidence, needs to run metro covid incidence plot.r
# t1 <- left_join(mazips %>% distinct(zipcode, .keep_all = TRUE), US_SVI.zip, by="zipcode") %>% 
#   group_by(Town) %>%
#   summarise(E_TOTPOP.town=sum(E_TOTPOP.zip, na.rm = TRUE),
#             SVI_SES.town = (sum(SVI_SES*E_TOTPOP.zip, na.rm = TRUE)/E_TOTPOP.town)) %>%
#   ungroup() %>%
#   mutate(quartile.SVI_SES.us = cut(SVI_SES.town, c(-1, 0.3, 0.5, 0.75, 1.1), labels = c("1", "2", "3", "4")))
# 
# metroSVIoverall<-left_join(combined, t1, by = "Town") %>%
#  # filter(Town %in% metrotowns) %>%
#   group_by(date, quartile.SVI_SES.us) %>%
#   dplyr::summarise(pop=sum(town_population),
#                    Cases=sum(Count1wk)
#   ) %>%
#   mutate(incidence1day=((Cases/pop)*100000)/7,
#          example=ifelse(quartile.SVI_SES.us==1, "Newton/Brookline",
#                         ifelse(quartile.SVI_SES.us==2, "Cambridge/Somerville",
#                                ifelse(quartile.SVI_SES.us==3, "Quincy/Allston",         
#                                       ifelse(quartile.SVI_SES.us==4, "Lynn/Dorchester", "Blank"))))
#   )
# 
#     
# metroSVIplot.us<- metroSVIoverall %>% filter(date < as.Date("2022-01-10")) %>%
#   ggplot( aes(x=date, y=incidence1day, group=quartile.SVI_SES.us, color= as.factor(quartile.SVI_SES.us))) +
#   theme_classic() +  theme( plot.title = element_text(size = rel(1.5)),
#                             axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_line(size=1.5)+
#   #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.2, se=FALSE) +
#   labs(x="Date", y="Daily Incidence, per 100,000",
#        title="Incidence of Covid-19 by SES Vulnerabilty in Massachusetts",
#        subtitle = paste0("Last data available: ", max(metroSVIoverall$date)),
#        caption="Source: Massachusetts Department of Public Health, CDC Social Vulnerability Index, US Census" )+
#   scale_color_jama(palette = c("default")) +
#   geom_label_repel(data=metroSVIoverall  %>% filter(date == as.Date("2022-01-06")) , 
#                    aes(label=paste0("SVI SES Q", quartile.SVI_SES.us,"\n",round(incidence1day,1),"/100,000\n","(eg, ",example, ")"),  x = date, y = incidence1day, 
#                        color=as.factor(quartile.SVI_SES.us)),min.segment.length = 0, 
#                    xlim=c(Sys.Date()+40, Sys.Date()+200)) +
#   guides(color="none") +
#   coord_cartesian( xlim=c(as.Date(NA), Sys.Date()+200))+
#   scale_x_date(breaks = scales::pretty_breaks(10),  labels=scales::label_date_short())+
#   scale_y_continuous(breaks = scales::pretty_breaks(10))
# 
# metroSVIplot.us
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/SVI incidence.us.pdf", width = 16, height=10)

antiCD20 <- read_excel("Ritux and Ocrev Patients BWH and BWF Past 3 months.xlsx", sheet = "Sheet1") %>%
  clean_names() %>%
  rename(zipcode= zip_code) %>%
  mutate(date = as.Date(ymd(date))) 

ritux_provider <- read_csv("ritux provider.csv")

CD20priority<-left_join( 
  left_join(antiCD20 %>%
  group_by(mrn) %>%
  arrange(desc(date))%>%
  distinct(mrn, .keep_all= TRUE) %>%
  ungroup() %>%
  mutate(zipcode = str_sub(zipcode, 1, 5)),
  US_SVI.zip, by = "zipcode") %>%
  mutate(SVI_quartile  = as.numeric(cut(SVI_SES,
      breaks = c(-1, .25, .5, .75, 1.1), labels = c(1, 2, 3, 4)))) %>%
  rowwise()%>%
  mutate(random1 = runif(1),
         random2 = runif(1),
         random3 = runif(1),
         random4 = runif(1)) %>%
  mutate(priority = case_when(
    SVI_quartile == 1 ~ max(c(random1, random2)),
    SVI_quartile == 2 ~ max(c(random1, random2)),
    SVI_quartile == 3 ~ max(c(random1, random2, random3, random3)),
    SVI_quartile == 4 ~ max(c(random1, random2, random3, random4)) )),
  ritux_provider, by = "referring_prov") %>%
  select(priority, dep, department, patient, mrn, referring_prov) %>%
  arrange(desc(priority))



CD20priority %>%
  count(dep)

write_csv(CD20priority %>% filter(dep == "rheum" & mrn !="15056617" & mrn !="11078284" & mrn !="09765900" & 
                                     mrn !="14271308" & mrn !="12027348"), "20220107 tix_cil rheum priority.csv")
write_csv(CD20priority %>% filter(dep == "neuro"), "20220107 tix_cil neuro priority.csv")

write_csv(CD20priority %>% filter(dep == "derm"), "20220107 tix_cil derm priority.csv")

write_csv(CD20priority %>% filter(dep == "renal"), "20220120 tix_cil renal priority.csv")


change<- mAb_SVI %>%
  distinct(zipcode, .keep_all = TRUE) %>%
  mutate(
    SVI_overall_quartile  = as.numeric(cut(SVI_overall, breaks = c(-1, .25, .5, .75, 1.1), labels = c(1, 2, 3, 4))),
    SVI_SES_quartile = as.numeric(cut(SVI_SES, breaks = c(-1, .25, .5, .75, 1.1), labels = c(1, 2, 3, 4))),
    priority_change = case_when(
      SVI_overall_quartile < SVI_SES_quartile ~ "SES gives higher priority",
      SVI_overall_quartile > SVI_SES_quartile ~ "Overall gives higher priority",
      SVI_overall_quartile == SVI_SES_quartile ~ "No Change")) %>%
  rename(eohhs_region = eohhs_name,
         population = E_TOTPOP.zip) %>%
  select(Town, zipcode, population, SVI_overall_quartile:priority_change) %>%
  arrange(desc(population))

change %>%
  count(priority_change)

change %>%
  group_by(priority_change) %>%
  tally(population)

write_csv(change, "Comparison of SVI metrics.csv")

    
  
############################
#MGH lists

fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}
MGH <- read_excel("20220109MGHMRNList1.xlsx", sheet = "Sheet1") %>%
  clean_names() %>%
  rename(zipcode= zip_code)%>%
  mutate(zipcode = fixzip(zipcode))

MGHMRN20220109List1.priority<-left_join(MGH %>%
              group_by(mrn) %>%
              distinct(mrn, .keep_all= TRUE) %>%
              ungroup() %>%
              mutate(zipcode = str_sub(zipcode, 1, 5)),
            US_SVI.zip, by = "zipcode") %>%
    mutate(SVI_quartile  = as.numeric(cut(SVI_SES,
                                          breaks = c(-1, .25, .5, .75, 1.1), labels = c(1, 2, 3, 4))),
           SVI_quartile = replace_na(SVI_quartile, as.factor(2))) %>%  # PO boxes, international, missing
    rowwise()%>%
    mutate(random1 = runif(1),
           random2 = runif(1),
           random3 = runif(1),
           random4 = runif(1)) %>%
    mutate(priority = case_when(
      SVI_quartile == 1 ~ max(c(random1, random2)),
      SVI_quartile == 2 ~ max(c(random1, random2, random3)),
      SVI_quartile == 3 ~ max(c(random1, random2, random3)),
      SVI_quartile == 4 ~ max(c(random1, random2, random3, random4)) )) %>%
    ungroup()%>%
  select(priority, random1:random4, mrn, referring_provider_last_first, SVI_quartile) %>%
  arrange(desc(priority))
    

write_csv(MGHMRN20220109List1.priority, "MGHMRN20220109List1.priority.csv")  
    