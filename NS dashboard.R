library(tidyverse)
library(patchwork)
library(ggmap)
library(readxl)
library(ggrepel)
library(ggsci)
library(showtext)
library(lubridate)
library(zoo)
library(readxl)
library(ggsci)


showtext_auto()
# townsgeobos<-read.csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/townsgeo_onlybostonneigh.csv")
# townsgeo<-read.csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(read.csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/townsgeo_onlybostonneigh.csv"), 
                  read.csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/townsgeo.csv") %>% filter(Town !="Boston"))
# 
# NSstudents <-read_excel("NSaddresses2021.xlsx", sheet="StudentsTeachers")
# gps.students<- ggmap::geocode(NSstudents$Address)
# students<- bind_cols(NSstudents, gps.students)
# write.csv(gps.students,"NSstudentsteachers2021.csv", row.names = FALSE)
NSstudentsteachers<-read.csv(file="NSstudentsteachers2021.csv")

load("vaccine.town.Rdata")
load("vaccine.sex.Rdata")

MAtowns <- read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/MAtowns.csv") %>%
  mutate(zipcode =  sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}}))

boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

vaccine.towns<- rbind(vaccine.town %>%
                     filter(Age_group=="Total" & Town !="Boston") %>%
                     mutate(date=reportdate)%>%
                     select(date, Town, fullvax, pop),
                   
                   left_join(vaccine.sex, MAtowns %>% select(zipcode, Town, town_population), by="zipcode") %>%
                     filter(Town %in% boston) %>%
                     mutate(date=reportdate)%>%
                     group_by(date, Town) %>%
                     summarize(fullvax= sum(fullvax.total),
                               pop = mean(town_population)) %>%
                     ungroup())

allcovidtowns.long<- left_join(
  left_join(
  read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/MAtowns.csv") %>% distinct(Town, .keep_all=TRUE),
           read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/allcovidtowns.csv", guess_max = 100000), by= "Town"),
  vaccine.towns, by= c("date", "Town") ) %>%
    mutate(Rate = Count*100000/town_population,
           incidence7day=Rate-(lag(Rate,1)), 
           incidence1day=(Rate-lag(Rate,1))/7,
           incidence14day=Rate-lag(Rate,2), #creates issues with negative incidence for first date for each town, as subtracts prior zipcode
           incidence1day2wk=(Rate-lag(Rate,2))/14,
           Tests2wk=Tests-(lag(Tests,2)),
           Count2wk=Count-(lag(Count,2)),
           positivity2wk=Count2wk/Tests2wk, 
           town_population=(Count/Rate)*100000,
           testingrate=100000*Tests2wk/ town_population,
           vaccine_prev = fullvax/town_population,
           zipcode =  sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}}))


allcovidtowns<-allcovidtowns.long %>%
  filter(date == as.Date(max(date)))  #date of last MDPH/BPHC town data
TownCovidGeo <-  right_join(allcovidtowns %>% distinct(Town, .keep_all = TRUE), matownsgeo, by = "Town")

NS.towns <- tribble(
  ~Town, ~count,
  "Jamaica Plain", 45,
  "Roslindale", 9, 
  "Dorchester Uphams", 3,
  "Fenway", 1, 
  "Milton", 1, 
  "West Roxbury", 4, 
  "Stoughton", 2, 
  "Dorchester Codman", 4, 
  "Hyde Park", 1, 
  "Mattapan", 1, 
  "Brookline", 1, 
  "Dedham", 1, 
  "Cambridge", 2, 
  "Malden", 1, 
  "Quincy", 1) %>%
  mutate(weight = count/sum(count))

ns<-left_join(allcovidtowns.long %>%
  filter(Town %in% NS.towns$Town) %>% 
  distinct(Town, date, .keep_all = TRUE),
  NS.towns, by="Town")  %>%
  mutate(Count.wt=(Count*weight),
         Tests.wt=(Tests*weight),
         Positivity.wt=(positivity2wk*weight),
         vaccine_prev.wt=(vaccine_prev*weight),
         population.wt=town_population*weight)

nscommunity<- ns %>% 
  filter(date> as.Date("2020-05-06")) %>%
  dplyr::group_by(date) %>% 
  dplyr::summarise(Count=sum(Count),
                   Count.weighted=sum(Count.wt),
                   Tests=sum(Tests),
                   Tests.weighted=sum(Tests.wt),
                   population=sum(town_population),
                   population.weighted=sum(population.wt, na.rm = TRUE),
                   positivity.weighted=sum(Positivity.wt, na.rm = TRUE),
                   vaccine_prev.weighted=sum(vaccine_prev.wt, na.rm = TRUE),
                   weight.check=sum(weight)) %>%
  dplyr::select(date, Count.weighted, Tests.weighted, population.weighted,positivity.weighted, vaccine_prev.weighted) %>% 
  arrange(date)   %>% 
  mutate(incidence14day=((Count.weighted-lag(Count.weighted,2))/population.weighted)*100000,
         incidence1day=incidence14day/14
  )


positivity.cat<-tribble(
  ~y, ~x, ~label,
  10.1, as.Date("2021-01-28"), "Very high\n(Testing insufficient)",
  5.1, as.Date("2021-01-28"), "High",
  2.6, as.Date("2021-01-28"), "Moderate",
  0.1, as.Date("2021-01-28"), "Low\n(Testing sufficient)" 
)

ns.positivity<- nscommunity %>%
  # mutate(positivity.weighted=replace(positivity.weighted,is.na(positivity.weighted), .06)) %>% 
  # mutate(positivity.weighted = ifelse(date > as.Date("2020-09-10") & date < as.Date("2020-11-11"), NA, positivity.weighted)) %>%
  # # filter(!is.na(positivity.weighted)) %>%
  add_row(date=as.Date(Sys.Date()+90), positivity.weighted=NA) %>% #add row with xmax so that ribbbon plots to max date
  ggplot( aes(x=date)) +
  geom_ribbon(aes(ymin=10, ymax=14),color="#969696", fill="#bd0026")+ #' very high, 10%'
  geom_ribbon(aes(ymin=5, ymax=10),color="#969696", fill="#e31a1c")+ #' high, 5%'
  geom_ribbon(aes(ymin=2.5, ymax=5),color="#969696", fill="#feb24c")+ #' moderate, 2.5%'
  geom_ribbon(aes(ymin=0, ymax=2.5),color="#969696", fill="#74c476")+ #' low, 0.5%'
  geom_line(aes(y=positivity.weighted*100), alpha=1, size=1.5, color="#525252") +
  # geom_line(data= nscommunity %>%
  #             mutate(positivity.weighted=replace(positivity.weighted,is.na(positivity.weighted), .06)) %>% 
  #             mutate(positivity.weighted = ifelse(date > as.Date("2020-09-10") & date < as.Date("2020-11-11"), NA, positivity.weighted)) %>% 
  #             filter(!is.na(positivity.weighted)) ,
  #           aes(y=positivity.weighted*100, x=date),
  #           alpha=1, size=1, color="#525252", linetype = "dotted")+
  theme_classic() + theme(plot.title = element_text(size = rel(1.0)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  geom_label_repel(data=nscommunity %>%
                     filter(date == last(date)), aes(label= paste0("NS Community\nAverage: ", round(positivity.weighted*100,1),"%"),
                                                     x = date ,  y = positivity.weighted*100),
                   hjust=0,  nudge_y = 0.5, nudge_x = -25,
                   xlim=c(Sys.Date(), as.Date(NA))) +
  geom_text(data=positivity.cat, aes(label=label, x=x, y=y), colour="#d9d9d9", size=3, hjust=0.5, vjust=0)+
  #guides(scale=FALSE) +
  coord_cartesian(ylim=c(0,12), xlim=c(as.Date("2021-03-20"), Sys.Date()+90))+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  # scale_fill_viridis(discrete = TRUE, end=0.85, option = "D")+
  labs(x="Date", y="Percent Tests Positive (weighted)",
       title="Test Positivity in Neighborhood School Communities",
       subtitle = "Two-week running average")

ns.positivity


risk.cat<-tribble(
  ~y, ~x, ~label,
  20, as.Date(Sys.Date()+100), "CDC 'High Transmission'",
  10.5, as.Date(Sys.Date()+100), "CDC 'Substantial Transmission'",
  4.1, as.Date(Sys.Date()+100), "CDC 'Moderate Transmission'",
  0.5, as.Date(Sys.Date()+100), "CDC 'Low Transmission'"
)
labels<-nscommunity %>% filter(date == last(date))
ns.incidence<- nscommunity %>%
  mutate(incidence1day=replace(incidence1day,is.na(incidence1day), 33)) %>% #overwrite peak average (~23) to fit in plot
  add_row(date=as.Date(Sys.Date()+200), incidence1day=NA) %>% #add row with xmax so that ribbbon plots to max date
  ggplot( aes(x=date)) +
  geom_ribbon(aes(ymin=14.14286, ymax=1000),color="#969696", fill="#E64B35B2")+ #' high, 100+'
  geom_ribbon(aes(ymin=7, ymax=14.14286),color="#969696", fill="#8491B4B2")+ #' substantial, 50-99'
  geom_ribbon(aes(ymin=1.285714, ymax=7),color="#969696", fill="#4DBBD5B2")+ #'  CDC moderate 10-49'
  geom_ribbon(aes(ymin=0, ymax=1.285714),color="#969696", fill="#00A087B2")+ #' CDC Low,0-9 per week
  geom_line(aes(y=incidence1day),   size=1.5, color="#525252") +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+

  geom_label_repel(data=labels, aes(label= paste0("NS Community\nAverage: ", round(incidence1day,1)),
                                                     x = date ,  y = incidence1day),
                   hjust=0, min.segment.length = 0, nudge_y = 15 , nudge_x = 4, 
                   xlim=c(Sys.Date(), as.Date(NA)))+
  geom_text(data=risk.cat, aes(label=label, x=x, y=y), colour="#7E6148FF", size=3.4, hjust=1, vjust=0)+
  #guides(color=FALSE) +
  coord_cartesian(ylim=c(0,80), xlim=c(as.Date("2021-07-20"), Sys.Date()+110))+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  labs(x="Date", y="Daily Cases, per 100,000",
       title="What is the Covid-19 transmission risk in our community?",
       subtitle = paste0("Two-week weighted average in NS communities | Risk by CDC criteria | Most recent data: ", 
                         max(nscommunity$date)))
ns.incidence


##Maps
townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
townsgeo<-read.csv("townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(townsgeo, townsgeobos)


CombinedGeo <-  left_join(
  left_join(matownsgeo, allcovidtowns.long %>% filter(date==last(date)), by = "Town"),
  read_csv("town.neighborhood.labels.csv"), by="Town")

NS.incidencemap <- ggplot()+
  geom_polygon(data = CombinedGeo, 
               aes(x = long, y = lat, group = group, fill=incidence1day), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_point(data=NSstudentsteachers, aes(x=lon, y=lat), shape=21, fill="dark grey", size=2)+
  geom_text( data = CombinedGeo %>%
               distinct(Town, .keep_all = TRUE),
             size=3, color= "black",
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(incidence1day,0), ")", sep="")))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(0,25),breaks = c(0, 5, 10, 15, 20, 25),
                       name="Covid-19\nincidence\nper 100,000") +
  labs(title="What is the risk where I live, work, or shop?",
       subtitle = paste0("Approximate location of NS Households | Cases for week preceeding ",max(CombinedGeo$date)))+
  theme(legend.position = c(.95, .67), legend.justification = c(1, 1), plot.title = element_text(size = rel(1.5)),
        legend.direction = "vertical", #aspect.ratio = 1.25,
        legend.background = element_rect(fill="white", color="white")) +
  coord_cartesian(xlim=c(-71.21, -70.95), ylim = c(42.13, 42.425))
NS.incidencemap

load("kids0to19.Rdata")
jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")

kids<- kids0to19 %>%
  ggplot(aes(x=date, y=incidence1day))+
  geom_line(aes(color=agecat), size=2) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  coord_cartesian(ylim=c(0,150), xlim=c(as.Date("2021-04-07"), Sys.Date()+90))+
  geom_hline(yintercept=0, linetype="dotted")+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  scale_color_npg(name="Age", alpha = 0.7)+
  labs(x="Date", y="Daily Cases, per 100,000",
       title="How many children are being diagnosed with Covid?",
       subtitle = paste0("Two-week average incidence in Massachusetts | Most recent data: ", 
                         max(kids0to19$date))) +
  guides(color="none")+
  geom_label_repel(data=kids0to19  %>%
                     filter(date == last(date)), aes(label= paste0(agecat, "\n ", round(incidence1day,1), " per 100,000"),
                                                     x = date ,  y = incidence1day, color=agecat),
                   hjust=0, min.segment.length = 0,
                   xlim=c(Sys.Date()+20, as.Date(NA)))
kids
# mypal = pal_npg("nrc", alpha = 0.7)(9)
# mypal
# library("scales")
# show_col(mypal)

load("vaccine.schoolage.Rdata")
library(ggpmisc)
ns.vaccine<- vaccine.schoolage %>%
  filter(reportdate == max(reportdate)) %>%
  filter(Town == "Jamaica Plain" | Town == "Roslindale" | Town =="Dorchester Uphams"
         | Town=="Dorchester Codman" |Town == "Roxbury") %>%
  filter(Age_group == "5-11 Years") %>%
  rowwise() %>%
  mutate("Vaccination among Children of\n5 to 11 Years, Two Doses" = paste0(Town, " ", round(fullvax.pct*100,0), "%")) %>%
  select("Vaccination among Children of\n5 to 11 Years, Two Doses")

vaccine.ns <- nscommunity %>% filter(date !=as.Date("2021-05-20")) %>% #spuriously dips in prev
  ggplot(aes(x=date, y=vaccine_prev.weighted*100))+
  geom_line(size=2, color="#00A087B2") +
  geom_hline(yintercept=0, linetype="dotted")+
  theme_classic() + theme( plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  coord_cartesian(ylim=c(0,100), xlim=c(as.Date("2021-03-20"), Sys.Date()+90))+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  # scale_color_jama(palette = c("default"))+
  labs(x="Date", y="Percent",
       title="What is the vaccination rate in our community?",
       subtitle = paste0("Complete vaccination, population-weighted  | Most recent data: ", 
                         max(nscommunity$date))) +
  geom_label_repel(data=nscommunity  %>%
                     filter(date == last(date)), aes(label= paste0("Complete\nvaccination:\n", 
                                                                   round(vaccine_prev.weighted*100,0), "%"),
                                                     x = date ,  y = vaccine_prev.weighted*100),
                   hjust=0.5, min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA))) +
  annotate("table", x= as.Date("2021-03-15"), y= 105, label = list(cbind(ns.vaccine)), size=4)
vaccine.ns 


load("mdph_testingbydate.Rdata")
load("vaccine.town.Rdata")
mdphfile<- "mdphcase_recent.xlsx"

#hospitalization and deaths from MDPH
hosp.and.deaths <-left_join(
  read_excel(mdphfile, sheet="Hospitalization from Hospitals") %>%
    janitor::clean_names() %>%
    mutate(date = ymd(date)) %>%
    arrange(date) %>%
    mutate(hosp = if_else(is.na(new_covid_19_hospitalizations), 0, new_covid_19_hospitalizations),
           hospitalizations=cumsum(hosp)) %>%
    filter(date <= max(date-2))%>%  # to remove delayed reporting bias
    select(date, hospitalizations),
  
  read_excel(mdphfile, sheet="DateofDeath")  %>%
    janitor::clean_names() %>%
    rename(deaths= confirmed_total) %>%
    mutate(date = ymd(date_of_death)) %>%
    filter(date <= max(date-2))%>%  # to remove delayed reporting bias
    select(date,deaths),
  by= "date")


maxdate<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough")  %>%  
  mutate(date = ymd(date)) %>%
  filter(date == max(date))
mindate<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough")  %>%  
  mutate(date = ymd(date)) %>%
  filter(date != as.Date("2021-01-01")) %>%
  filter(date == min(date))


mdph_bt_cases <-left_join(
  left_join(
    left_join(
      mdph_testingbydate %>%  ##total MPHD cases
        mutate(newpositive = if_else(is.na(newpositive), 0, newpositive),
               cases = cumsum(newpositive)) %>%
        select(date, cases),
      hosp.and.deaths, by="date") %>%
      filter(date> as.Date("2020-12-31") & date<= max(date-1)),
   read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough")  %>%  ##breakthrough cases reported
      mutate(date = ymd(date)) %>%
      mutate(bt_cases = cases,
             days= as.numeric(date-lag(date)),
             bt_cases7day = (bt_cases-lag(bt_cases))*7/days,
             bt_hospitalizations = hospitalization,
             bt_hosp7day = (bt_hospitalizations-lag(bt_hospitalizations))*7/days,
             bt_deaths = death,
             bt_deaths7day = (bt_deaths-lag(bt_deaths))*7/days) %>%
      select(date, bt_cases, bt_hospitalizations, bt_deaths, bt_cases7day, bt_hosp7day, bt_deaths7day),
    by = "date") %>%
    mutate(    cases7day = cases-lag(cases,7),
               pct.bt.cases = bt_cases7day/cases7day,
               pct.bt.cases = na.approx(pct.bt.cases, maxgap = 200, rule = 2),
               hosp7day = hospitalizations-lag(hospitalizations,7),
               pct.bt.hosp = bt_hosp7day/hosp7day,
               pct.bt.hosp = na.approx(pct.bt.hosp, maxgap = 200, rule = 2),
               deaths7day = deaths-lag(deaths,7),
               pct.bt.deaths = bt_deaths7day/deaths7day,
               pct.bt.deaths = na.approx(pct.bt.deaths, maxgap = 200, rule = 2)) %>%
    select(date, pct.bt.cases, cases7day, pct.bt.deaths, deaths7day, pct.bt.hosp, hosp7day),
  vaccine.town %>%       ## imputed longitudinal vaccination
    filter(Age_group == "Total") %>%
    group_by(reportdate) %>%
    rename(date = reportdate) %>%
    summarise(total.fullvax = sum(fullvax, na.rm = TRUE)) %>%
    ungroup()%>%
    add_row(date= as.Date("2021-01-01"), total.fullvax =0) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    mutate(total.fullvax = if_else(total.fullvax== 0 & date != as.Date("2021-01-01"), as.numeric(NA), total.fullvax)) %>%
    mutate(total.fullvax = na.approx(total.fullvax, maxgap = 200, rule = 2),
           MApop =7029917,
           vax.MApop = total.fullvax,
           unvax.MApop = MApop-vax.MApop,
           pct.vax = vax.MApop/MApop) ,
  by = "date") %>%
  mutate(
    vax.MApop = na.approx(vax.MApop, maxgap = 200, rule = 2) ,
    unvax.MApop = na.approx(unvax.MApop, maxgap = 200, rule = 2) ,
    MApop = na.approx(MApop, maxgap = 200, rule = 2) ,
    pct.vax = na.approx(MApop, maxgap = 200, rule = 2) ,
    total.fullvax = na.approx( pct.vax, maxgap = 200, rule = 2) ,
    bt_cases = pct.bt.cases*cases7day,
    unvax_cases = (1-pct.bt.cases)*cases7day,
    all.incidence1day = (cases7day*100000/MApop)/7,
    vax.incidence1day=(bt_cases*100000/vax.MApop)/7,
    unvax.incidence1day=(unvax_cases*100000/unvax.MApop)/7,
    VE.infection = 1- (vax.incidence1day/unvax.incidence1day),
    bt_hosp = pct.bt.hosp*hosp7day,
    unvax_hosp = (1-pct.bt.hosp)*hosp7day,
    all.hosp1day = (hosp7day*100000/MApop)/7,
    vax.hosp1day=(bt_hosp*100000/vax.MApop)/7,
    unvax.hosp1day=(unvax_hosp*100000/unvax.MApop)/7,
    VE.hosp = 1- (vax.hosp1day/unvax.hosp1day),
    bt_deaths = pct.bt.deaths*deaths7day,
    unvax_deaths = (1-pct.bt.deaths)*deaths7day,
    all.deaths1day = (deaths7day*100000/MApop)/7,
    vax.deaths1day=(bt_deaths*100000/vax.MApop)/7,
    unvax.deaths1day=(unvax_deaths*100000/unvax.MApop)/7,
    VE.death = 1- (vax.deaths1day/unvax.deaths1day),
  ) %>%
  filter(date < as.Date(maxdate$date+7) & date>as.Date("2021-06-18"))

VE.recent<- mdph_bt_cases %>% filter(date==max(as.Date(date))) %>% select(VE.infection)

bt_long <- mdph_bt_cases %>%
  select(date, vax.incidence1day, unvax.incidence1day, vax.hosp1day, unvax.hosp1day, vax.deaths1day, unvax.deaths1day, ) %>%
  #rename(Vaccinated = vax.incidence1day, Unvaccinated = unvax.incidence1day) %>%
  pivot_longer(!date, names_to = "status", values_to = "incidence1day") %>%
  mutate(label = status,
         label = fct_collapse(label, Vaccinated = c("vax.incidence1day", "vax.hosp1day", "vax.deaths1day"),
                              Unvaccinated = c("unvax.incidence1day", "unvax.hosp1day", "unvax.deaths1day"))) %>%
  filter(date != max(date))



# MAincidence_byvaccination<-bt_long %>% 
#   filter(status == "unvax.incidence1day" | status == "vax.incidence1day") %>%
#   ggplot() +
#   geom_hline(yintercept=0, linetype="dotted")+
#   geom_line(aes(x=date, y=incidence1day, group=label, color=label), size=2, alpha=0.7) +
#   theme_classic() + theme( plot.title = element_text(size = rel(1.5)), 
#                           axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
#   labs(x="Date",
#        y="Daily Cases, per 100,000",
#        title="What is the impact of vaccination on risk?",
#        subtitle = paste0("All Massachusetts residents")) +
#   coord_cartesian(xlim=c(as.Date("2021-06-19"), as.Date(max(bt_long$date+90)) ), ylim=c(0,150) ) +
#   scale_colour_npg() +
#   geom_label_repel(data = bt_long %>% filter(date == max(bt_long$date)) %>%
#                      filter(status == "unvax.incidence1day" | status == "vax.incidence1day") ,
#                    aes(x = date, y = incidence1day, label = paste0(label, "\n", round(incidence1day, 0), "/100,000"), color= label),
#                    hjust=0.5, 
#                    xlim=c(max(bt_long$date), as.Date(NA)))+
#   guides(color="none") +
# scale_y_continuous(breaks = scales::pretty_breaks(5)) +
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
# MAincidence_byvaccination


load("mwra.wastewater.covid.Rdata")

sewage$index <- 1:nrow(sewage)  # create index variable

wastewater.smooth <-
  rbind( 
    cbind(sewage %>%
            filter(year == "Current") %>%
            mutate(mean.log10 = log10(mean)) %>%
            summarize(smoothed =  predict(loess(mean ~ index, span=0.04), na.rm = TRUE),
                      smoothed.log10 =  predict(loess(mean.log10 ~ index, span=0.04), na.rm = TRUE)),
          sewage %>% filter(year == "Current" & !is.na(mean)) %>% select(date)) %>%
      mutate(year = "Current"),
    
    cbind(sewage %>%
            filter(year == "Last Year") %>%
            mutate(mean.log10 = log10(mean)) %>%
            summarize(smoothed =  predict(loess(mean ~ index, span=0.04), na.rm = TRUE),
                      smoothed.log10 =  predict(loess(mean.log10 ~ index, span=0.04), na.rm = TRUE)),
          sewage %>% filter(year == "Last Year" & !is.na(mean)) %>% select(date)) %>%
      mutate(year = "Last Year")) %>%
  filter(smoothed >0) %>%
  mutate(year = fct_relevel(year, "Current", "Last Year" ))


#log transformed
wastewater.recent.log<-
  ggplot() +
    theme_classic() + theme( plot.title = element_text(size = rel(1.5)),
                            axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  geom_point(data= sewage %>% filter(year == "Current"), aes(x=date, y=log10(mean)), alpha=.5, size=1.5, color= "#8A9197FF") +
  geom_line(data= wastewater.smooth, aes(x=date, y=smoothed.log10, color=year, group=year ), 
            alpha=.7, size=2.5) +
  labs(x="Date (current year)", y="Log10 SARS-CoV-2 Load (copies/mL)",
       title="How intense is Covid epidemic now compared to last year?",
       subtitle = paste0("Measured by viral particles flushed into sewer | Last measurement: ", as.Date(last(sewage$date))),
       caption="Source: Massachusetts Water Resources Authority" )+
  scale_colour_npg() +
  guides(color="none") +
  coord_cartesian(xlim=c(as.Date("2021-06-19"), as.Date(max(bt_long$date+90)) ), ylim=c(1,4.5))+
  scale_x_date(breaks = scales::pretty_breaks(12), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  geom_hline(yintercept = 0, linetype ="dotted") +
  geom_label_repel(data = wastewater.smooth %>% filter(date == Sys.Date()-5),
                   aes(x = date, y = smoothed.log10, label = paste0(year), color= year),
                   hjust=0.5, min.segment.length =0 , nudge_x = 90, nudge_y = .2)

wastewater.recent.log




layout_letters <- "
AABB
CCDD
CCEE
"


ns.incidence  + kids + NS.incidencemap + vaccine.ns +  wastewater.recent.log + plot_layout(design = layout_letters)
ggsave("NScovidepi.pdf", width=16, height=16)