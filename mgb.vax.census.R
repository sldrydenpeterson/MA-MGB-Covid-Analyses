library(tidyverse)
library(janitor)
library(lubridate)
library(ggsci)
library(ggrepel)

all<- read_csv("wsMGB_Daily_IP_Census_data-all.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         totalcensus.all = c_census_exclude_covid_status) %>%
  filter(!is.na(totalcensus.all)) %>% select(date, totalcensus.all)

covid<- read_csv("wsMGB_Daily_IP_Census_data-all.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         covidcensus.all = c_census) %>%
  filter(!is.na(covidcensus.all) & c_select_trend == "COVID-19") %>% select(date, covidcensus.all)

all.unvax<- read_csv("wsMGB_Daily_IP_Census_data-unvax.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         totalcensus.unvax = c_census_exclude_covid_status) %>%
  filter(!is.na(totalcensus.unvax)) %>% select(date, totalcensus.unvax)

covid.unvax<- read_csv("wsMGB_Daily_IP_Census_data-unvax.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         covidcensus.unvax = c_census) %>%
  filter(!is.na(covidcensus.unvax) & c_select_trend == "COVID-19") %>% select(date, covidcensus.unvax)

all.fullvax<- read_csv("wsMGB_Daily_IP_Census_data-fullvax.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         totalcensus.fullvax = c_census_exclude_covid_status) %>%
  filter(!is.na(totalcensus.fullvax)) %>% select(date, totalcensus.fullvax)

covid.fullvax<- read_csv("wsMGB_Daily_IP_Census_data-fullvax.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         covidcensus.fullvax = c_census) %>%
  filter(!is.na(covidcensus.fullvax) & c_select_trend == "COVID-19") %>% select(date, covidcensus.fullvax)

all.boost<- read_csv("wsMGB_Daily_IP_Census_data-boost.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         totalcensus.boost = c_census_exclude_covid_status) %>%
  filter(!is.na(totalcensus.boost)) %>% select(date, totalcensus.boost)

covid.boost<- read_csv("wsMGB_Daily_IP_Census_data-boost.csv") %>%
  clean_names() %>%
  mutate(date = mdy(day_of_c_calendar_date),
         covidcensus.boost = c_census) %>%
  filter(!is.na(covidcensus.boost) & c_select_trend == "COVID-19") %>% select(date, covidcensus.boost)


mgb.vaxstatus <- 
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              left_join(
                left_join(
                    all, covid, by="date"),
                all.unvax, by= "date"),
              covid.unvax, by = "date"),
            all.fullvax, by = "date"),
          covid.fullvax,  by = "date"),
        all.boost, by = "date"),
      covid.boost, by = "date") %>%
   replace(is.na(.), 0) #%>%
  # mutate(week = cut(date, "week")) %>%
  # group_by(week) %>%
  # summarise(totalcensus.all = sum(totalcensus.all),
  #           covidcensus.all = sum(covidcensus.all),
  #           totalcensus.unvax = sum(totalcensus.unvax),
  #           covidcensus.unvax = sum(covidcensus.unvax),
  #           totalcensus.fullvax = sum(totalcensus.fullvax),
  #           covidcensus.fullvax = sum(covidcensus.fullvax),
  #           totalcensus.boost = sum(totalcensus.boost),
  #           covidcensus.boost = sum(covidcensus.boost)
  # ) %>% ungroup()
    

PR<-bt_long %>%
  filter(status == "vax.incidence1day" | status == "unvax.incidence1day") %>%
  pivot_wider(id_cols ="date", names_from = "status", values_from = "incidence1day") %>%
  arrange(date) %>%
  mutate (prev.vax = (vax.incidence1day*7*4)/100000,
          prev.unvax = (unvax.incidence1day*7*4)/100000,
          PR = (1-prev.vax)/(1-prev.unvax),
          PR.lag = lag(PR, 14)) %>%
  select(date,PR, PR.lag)
  
      
VE<-left_join(mgb.vaxstatus, PR, by = "date") %>%
  mutate(PR = na.approx(PR, maxgap = 200, rule = 2, na.rm = FALSE),
         PR.lag = na.approx(PR.lag, maxgap = 200, rule = 2, na.rm = FALSE)) %>%
  arrange(date) %>%
  mutate(AE.fullvax = covidcensus.fullvax/covidcensus.unvax,
         BF.fullvax = (totalcensus.fullvax-covidcensus.fullvax)/(totalcensus.unvax-covidcensus.unvax),
         AE.boost = covidcensus.boost/covidcensus.unvax,
         BF.boost = (totalcensus.boost-covidcensus.boost)/(totalcensus.unvax-covidcensus.unvax),
         VE.fullvax = 1-((AE.fullvax*PR.lag)/BF.fullvax),  #section that permits adjustment for community prev
         VE.boost = 1-((AE.boost*PR.lag)/BF.boost),
         # VE.fullvax = 1-((AE.fullvax)/BF.fullvax),
         # VE.boost = 1-((AE.boost)/BF.boost),
         VE.fullvax.7d = (VE.fullvax + lag(VE.fullvax, 1) + lag(VE.fullvax, 2) + lag(VE.fullvax, 3)
                          + lag(VE.fullvax, 4) + lag(VE.fullvax, 5) + lag(VE.fullvax, 6))/7,
         VE.boost.7d = (VE.boost + lag(VE.boost, 1) + lag(VE.boost, 2) + lag(VE.boost, 3)
                          + lag(VE.boost, 4) + lag(VE.boost, 5) + lag(VE.boost, 6))/7 ,
         VE.fullvax.14d = (VE.fullvax + lag(VE.fullvax, 1) + lag(VE.fullvax, 2) + lag(VE.fullvax, 3)
                          + lag(VE.fullvax, 4) + lag(VE.fullvax, 5) + lag(VE.fullvax, 6)
                          + lag(VE.fullvax, 7) + lag(VE.fullvax, 8) + lag(VE.fullvax, 9)
                          + lag(VE.fullvax, 10) + lag(VE.fullvax, 11) + lag(VE.fullvax, 12) + lag(VE.fullvax, 13)) /14,
         VE.boost.14d = (VE.boost + lag(VE.boost, 1) + lag(VE.boost, 2) + lag(VE.boost, 3)
                          + lag(VE.boost, 4) + lag(VE.boost, 5) + lag(VE.boost, 6)
                          + lag(VE.boost, 7) + lag(VE.boost, 8) + lag(VE.boost, 9)
                          + lag(VE.boost, 10) + lag(VE.boost, 11) + lag(VE.boost, 12)
                          + lag(VE.boost, 13))/14 
         )%>%
        select(date, VE.fullvax.7d, VE.boost.7d, VE.fullvax.14d, VE.boost.14d) %>%
        pivot_longer(!date, names_to = "vax", values_to = "VE")


MGB.VE<- VE %>%  filter(vax == "VE.fullvax.14d" | vax == "VE.boost.14d") %>%
  ggplot() +
  geom_line(aes(x=date, y= VE*100, group= vax, color=vax), size=2)+
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="",
       y="Vaccine Effectiveness (%)",
       title="Vaccine Effectiveness on Hospital Days, MGB",
       subtitle = paste0("Adapted test-negative design | 14-day running average", " | Last data available: ", as.Date(max(VE$date))),
       caption="Note: Inpatient census at MGB acute care hospitals from date Covid-19 census > 100 patients (11 Nov 2021)\nIncidental SARS-CoV-2 positivity estimated as relative prevalence in MA vaccinated and unvaccinated populations with 14-day lag") +
  geom_hline(yintercept=0, linetype="dotted")+
  scale_color_npg(alpha =0.8) +
  coord_cartesian(xlim=c(as.Date("2021-11-14"), as.Date(max(VE$date)+20)), ylim = c(0,100))+
  geom_label_repel(data = VE %>% filter(date == as.Date(max(date))) %>% filter(vax == "VE.fullvax.14d" | vax == "VE.boost.14d") ,
                   aes(x = date, y = VE*100, label = paste0(if_else(vax == "VE.boost.14d", "Fully Vaccinated\nwith Booster",
                                                                    "Fully Vaccinated\nand no Booster"), "\n", round(VE*100, 0), "%"), color= vax),
                   hjust=0.5, nudge_y = 15,
                   xlim=c(max(VE$date)+5, as.Date(NA))) +
  guides(color = "none")+
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
MGB.VE

  ggsave("MGB vaccine effectiveness by boosting.pdf", width =12, height = 8)

  
      
      