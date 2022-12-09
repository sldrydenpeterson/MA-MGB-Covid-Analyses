library(ggsci)
library(tidyverse)
library(readxl)
library(ggrepel)
#import inpatient and MDPH numbers from csv, fix date, and make long dataset
MGBcensus <-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MGB census", guess_max = 10000)  %>% filter(!is.na(Brigham))
MGBcensus$date <-(lubridate::ymd(MGBcensus$date))

#MGB acute care hospital beds
hospitalname<- c("Mass General", "Brigham", "NSMC-Salem", "Newton-Wellesley",
                 "Wentworth-Douglass", "Brigham\nFaulkner", "Cooley-Dickinson", 
                 "Martha's Vineyard", "Nantucket Cottage", "Brigham CoV-Risk", "Influenza\ninpatients", "All MGB Acute\nHospitals", "MGB Vaccinated\nInpatients", "MGB Unvaccinated\nInpatients")
beds<-c(1057, 793, 395, 273, 178, 162, 140, 25, 19, NA, NA,  3042, NA, NA)
Hospital<-c("Mass.General", "Brigham", "NSMC.Salem","Newton.Wellesley","Wentworth.Douglass", 
            "Brigham.Faulkner", "Cooley.Dickinson", "Marthas.Vineyard", "Nantucket.Cottage",
            "BWH.PUI", "flu.mgb", "All.MGB.Acute", "All.MGB.Acute.vax", "All.MGB.Acute.unvax")

#make combined date frame
InpatientBeds<- data.frame(hospitalname,Hospital, beds)

#Create long datasets, separately for hospitals
MGBcensuslong <- left_join(MGBcensus %>%
                             pivot_longer(!date, names_to = "Hospital", values_to = "Cases"),
                           InpatientBeds, by = "Hospital") %>%
  mutate(PercentCovid = Cases/beds) %>%
  filter(!is.na(Cases))


all<- read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/wsMGB_Daily_IP_Census_data.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         totalcensus.all = c_census_exclude_covid_status) %>%
  filter(!is.na(totalcensus.all)) %>% select(date, totalcensus.all)

mgb_inpatient <-
  rbind(
      all %>%
        mutate(year = "Current",
               avg7day = (totalcensus.all + lag(totalcensus.all, 1) + lag(totalcensus.all, 2) + lag(totalcensus.all, 3) + 
                            lag(totalcensus.all, 4) + lag(totalcensus.all, 5) +  lag(totalcensus.all, 6))/7) %>%
        add_row(year = "Current", avg7day = 162, date = as.Date("2022-12-04")),
      all %>%
        mutate(date = date +365,
               year = "Last Year",
               avg7day = (totalcensus.all + lag(totalcensus.all, 1) + lag(totalcensus.all, 2) + lag(totalcensus.all, 3) + 
                            lag(totalcensus.all, 4) + lag(totalcensus.all, 5) +  lag(totalcensus.all, 6))/7) %>%
        add_row(year = "Last Year", avg7day = 162, date = as.Date("2022-12-04")+365),
      all %>%
        mutate(date = date +365,
               year = "Current coverage (20%)",
               avg7day = (1-0.2*0.5)*(totalcensus.all + lag(totalcensus.all, 1) + lag(totalcensus.all, 2) + lag(totalcensus.all, 3) + 
                            lag(totalcensus.all, 4) + lag(totalcensus.all, 5) +  lag(totalcensus.all, 6))/7) %>%
        add_row(year = "Current coverage (20%)", avg7day = (1-0.2*0.5)*162, date = as.Date("2022-12-04")+365) %>%
        filter(date > max.inpatient.date +7),
      all %>%
        mutate(date = date +365,
               year = "Improved coverage (50%)",
               avg7day = (1-0.5*0.5)*(totalcensus.all + lag(totalcensus.all, 1) + lag(totalcensus.all, 2) + lag(totalcensus.all, 3) + 
                                        lag(totalcensus.all, 4) + lag(totalcensus.all, 5) +  lag(totalcensus.all, 6))/7) %>%
        add_row(year = "Improved coverage (50%)", avg7day = (1-0.4*0.5)*162, date = as.Date("2022-12-04")+365) %>%
        filter(date > max.inpatient.date +7)
  )

mgb_inpatient %>%
  filter(date >as.Date("2022-12-11") & date <as.Date("2023-03-01")) %>%
  pivot_wider(names_from = year)
  

max.inpatient.date<- (mgb_inpatient %>% filter(year == "Current") %>% filter(date == max(date)))$date

mgbcensus.proj<- mgb_inpatient %>%
  filter(!is.na(avg7day)) %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_hline(yintercept = 250, linetype = "dashed") +
  theme_classic() + theme(aspect.ratio = 0.75, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=avg7day, x=date, group = year, color = year), alpha=.7, size=1.5) +
  scale_color_jama(palette = c("default"), alpha = .8) +
  coord_cartesian(xlim=c(Sys.Date()+90 -340, Sys.Date()+90), ylim=c(0,600)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  guides(color="none", size = "none") +
  geom_label_repel(data= mgb_inpatient %>%
                     group_by(year)%>%
                     filter(date == max.inpatient.date),
                   aes(x=date, y= avg7day, label=paste0(year, "\nCensus:", round(abs(avg7day),0)), color = year ),
                   min.segment.length = 0, hjust=0.5,  nudge_x = 90, nudge_y = .2) +
  geom_label_repel(data= mgb_inpatient %>%
                     group_by(year)%>%
                     filter(date == as.Date("2023-01-10")),
                   aes(x=date, y= avg7day, label=paste0(year, "\nPeak: ", round(avg7day,0)), color = year) ,
                   min.segment.length = 0, hjust=0.5,  nudge_x = -90, nudge_y = .2) +
  labs(x="Date",
       y="Number",
       title="MGB Covid-19 Inpatient Census Projections",
       subtitle = max.inpatient.date,
       caption="Source: Daily inpatient hospital censuses (MGB Tableau)" )
mgbcensus.proj

ggsave("mgbcensus.proj.pdf", width =12, height = 8)
  
  
  geom_label_repel(data=bwhonly[!is.na(bwhonly$Cases),] %>%
                     filter(date == max(date)), aes(label= paste0(hospitalname, "\nn=",Cases),
                                                    x = date ,  y = Cases),
                   hjust=0.5,
                   xlim=c(Sys.Date()+20, as.Date(NA)))+
  coord_cartesian(xlim=c(as.Date("2021-08-01"), Sys.Date()+90), ylim=c(0,650))+
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short()) +
  labs(x="Date",
       y="Number (@ 6am)",
       title="MGB Covid-19 Inpatients",
       subtitle = max(bwhonly$date),
       caption="Source: Daily inpatient hospital censuses (Covid-19 Epic flag)" )+
  guides(color="none")
mgbcensus
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MGBcensus.jpg", plot = last_plot(), device = "jpeg",
#        scale = 1,  units = c("in"),height = 8.67, width = 8.67,
#        dpi = 160, limitsize = TRUE)
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MGBcensus.pdf")
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/MGBcensus.png")



mgbflucensus<- MGBcensuslong %>%
  filter(!is.na(Cases)) %>%
  filter(Hospital == "flu.mgb") %>%
  ggplot( aes(x=date, group=hospitalname, color=hospitalname)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_hline(yintercept = 250, linetype = "dashed") +
  theme_classic() + theme(aspect.ratio = 0.75, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=Cases), alpha=.7, size=1.5) +
  scale_color_jama(palette = c("default"), alpha = .8) +
  # scale_color_viridis(discrete = TRUE, end=0.5, option = "D")+
  geom_label_repel(data=MGBcensuslong %>%
                     filter(!is.na(Cases)) %>%
                     filter(Hospital == "flu.mgb") %>%
                     filter(date == max(date)), aes(label= paste0(hospitalname, "\nn=",Cases),
                                                    x = date ,  y = Cases),
                   hjust=0.5,
                   xlim=c(Sys.Date()+20, as.Date(NA)))+
  coord_cartesian(xlim=c(as.Date("2021-08-01"), Sys.Date()+90), ylim=c(0,650))+
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short()) +
  labs(x="Date",
       y="Number (@ 6am)",
       title="MGB Influenza Inpatients",
       subtitle = max(bwhonly$date),
       caption="Source: Daily inpatient hospital censuses (Influenza Epic flag)" )+
  guides(color="none")
mgbflucensus
