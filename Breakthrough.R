library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(ggsci)
library(ggrepel)
library(patchwork)

load("mdph_testingbydate.Rdata")
load("vaccine.town.Rdata")
mdphfile<- "mdphcase_recent.xlsx"

#hospitalization and deaths from MDPH
hosp.and.deaths <-left_join(
  read_excel(mdphfile, sheet="DateofDeath")  %>%
    janitor::clean_names() %>%
    rename(deaths= confirmed_total) %>%
    mutate(date = ymd(date_of_death)) %>%
    filter(date <= max(date-3))%>%  # to remove delayed reporting bias
    select(date,deaths),
  read_excel(mdphfile, sheet="Hospitalization from Hospitals") %>%
    janitor::clean_names() %>%
    mutate(date = ymd(date)) %>%
    arrange(date) %>%
    mutate(hosp = if_else(is.na(new_covid_19_hospitalizations), 0, new_covid_19_hospitalizations),
           hospitalizations=cumsum(hosp)) %>%
    filter(!is.na(new_covid_19_hospitalizations)) %>%  # to remove delayed reporting bias
    select(date, hospitalizations),
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
      mdph_testingbydate %>%  ##total MDPH cases
      mutate(newpositive = if_else(is.na(newpositive), 0, newpositive),
        cases = cumsum(newpositive)) %>%
     # filter(date <= max(date)) %>%  
      select(date, cases),
      hosp.and.deaths, by="date") %>%
      filter(date> as.Date("2020-12-31")),
    read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough")  %>%  ##breakthrough cases reported
      mutate(date = ymd(date)) %>%
      mutate(bt_cases = cases,
             days= as.numeric(date-lag(date)),
             bt_cases7day = (bt_cases-lag(bt_cases))*7/days,
             bt_hospitalizations = hospitalization,
             bt_hosp7day = (bt_hospitalizations-lag(bt_hospitalizations))*7/days,
             
             # starting 15 March 2022, definition of covid death updated by DPH, 
             # approximately 12.5% of deaths no longer classified as covid death
             bt_deaths = case_when(
               date <= as.Date("2022-03-07") ~ round(death*0.875, 0), 
               TRUE ~ death),
             
             # DPH data cleaning recognized 524 additional deaths on May 10 in vaccinated
             # individuals, all deaths prior to Dec 2021 assuming that missingness was constant
             
             bt_deaths = case_when(
               date < as.Date("2021-12-01") ~ round(bt_deaths*1.894197952,0),
               date >= as.Date("2021-12-01") & date < as.Date("2022-05-07") ~ round(bt_deaths + 524,0),
               TRUE ~ bt_deaths
             ),
       
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
         pct.vax = vax.MApop/MApop),
  by = "date") %>%
  mutate(
    vax.MApop = na.approx(vax.MApop, maxgap = 200, rule = 2) ,
    unvax.MApop = na.approx(unvax.MApop, maxgap = 200, rule = 2) ,
    MApop = na.approx(MApop, maxgap = 200, rule = 2) ,
    pct.vax = na.approx(MApop, maxgap = 200, rule = 2) ,
    #total.fullvax = na.approx( pct.vax, maxgap = 200, rule = 2) ,
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

VE.recent<- mdph_bt_cases %>% filter(date == max(date)-1) %>%
  summarise(VE.infection = last(VE.infection),
            VE.hosp = last(VE.hosp),
            VE.death = last(VE.death))
  

bt_long <- mdph_bt_cases %>%
  select(date, vax.incidence1day, unvax.incidence1day, vax.hosp1day, unvax.hosp1day, vax.deaths1day, unvax.deaths1day, ) %>%
  #rename(Vaccinated = vax.incidence1day, Unvaccinated = unvax.incidence1day) %>%
  pivot_longer(!date, names_to = "status", values_to = "incidence1day") %>%
  mutate(label = status,
         label = fct_collapse(label, Vaccinated = c("vax.incidence1day", "vax.hosp1day", "vax.deaths1day"),
         Unvaccinated = c("unvax.incidence1day", "unvax.hosp1day", "unvax.deaths1day"))) %>%
  filter(date != max(date))



MAincidence_byvaccination<-bt_long %>% 
  filter(status == "unvax.incidence1day" | status == "vax.incidence1day") %>%
ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=incidence1day, group=label, color=label), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)), 
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Confirmed Daily Covid-19, per 100,000",
       title="Covid-19 Incidence by Vaccination Status, Massachusetts",
       subtitle = paste0("Estimated from overall incidence and weekly reported case numbers in vaccinated residents | Data through ", as.Date(max(bt_long$date)))) +
  coord_cartesian(xlim=c(as.Date("2021-08-01"), as.Date(max(bt_long$date+20)) ), ylim=c(0,250) ) +
  scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = bt_long %>% filter(date == max(bt_long$date)) %>%
                     filter(status == "unvax.incidence1day" | status == "vax.incidence1day") ,
                     aes(x = date, y = incidence1day, label = paste0(label, "\n", round(incidence1day, 1), "/100,000"), color= label),
                   hjust=0.5, 
                   xlim=c(max(bt_long$date), as.Date(NA)))+
  guides(color="none") +
 # annotate("text", x=as.Date(max(bt_long$date-20)), y=30,
 # label=paste0("Crude relative protection of\nvaccinated vs unvaccinated: ", round(100*VE.recent$VE.infection,1), "%"), hjust=0, color= "#B24745ff")+
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
MAincidence_byvaccination
ggsave("MAincidence_byvaccination.pdf", width=16, height=12)




MAhosp_byvaccination<-bt_long %>% 
  filter(status == "unvax.hosp1day" | status == "vax.hosp1day") %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=incidence1day, group=label, color=label), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)), 
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Covid-19 Hospitalizations, per 100,000",
       title="Covid-19 Hospitalization by Vaccination, Massachusetts",
       subtitle = paste0("Estimated from overall and weekly-reported hospitalizations\nin vaccinated residents | Data through ", 
                         as.Date(max((as.data.frame(bt_long[which(bt_long$status=="vax.hosp1day" & !is.na(bt_long$incidence1day)),][,"date"]))$date)))) +
  coord_cartesian(xlim=c(as.Date("2021-08-01"), as.Date(max(bt_long$date+40)) ), ylim=c(0,20) ) +
  scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = bt_long %>% filter(status == "unvax.hosp1day" | status == "vax.hosp1day") %>% 
                     filter(!is.na(incidence1day)) %>%
                   filter(date == max(date)) ,
                   aes(x = date, y = incidence1day, label = paste0(label, "\n", round(incidence1day, 1), "/100,000"), color= label),
                   hjust=0.5, 
                   xlim=c(max(bt_long$date), as.Date(NA)))+
  guides(color="none") +
 # annotate("text", x=as.Date(max(bt_long$date-25)), y=1.2, label=paste0("Crude relative protection of\nvaccinated vs unvaccinated: ", round(100*VE.recent$VE.hosp,1), "%"), hjust=0, color= "#B24745ff")+
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
MAhosp_byvaccination
ggsave("MAhosp_byvaccination.pdf", width=16, height=12)

MAdeaths_byvaccination<-bt_long %>% 
  filter(status == "unvax.deaths1day" | status == "vax.deaths1day") %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=incidence1day*10, group=label, color=label), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)), 
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Daily Covid-19 Deaths, per 1,000,000",
       title="Covid-19 Deaths by Vaccination, Massachusetts",
       subtitle = paste0("Estimated from overall incidence and weekly reported deaths\nin vaccinated residents | Data through ", as.Date(max((as.data.frame(bt_long[which(bt_long$status=="vax.deaths1day" & !is.na(bt_long$incidence1day)),][,"date"]))$date)))) +
  coord_cartesian(xlim=c(as.Date("2021-08-01"), as.Date(max(bt_long$date+40)) ), ylim=c(0,17) ) +
  scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = bt_long %>% filter(status == "unvax.deaths1day" | status == "vax.deaths1day") %>% 
                     filter(!is.na(incidence1day)) %>%
                     filter(date == max(date)),
                   aes(x = date, y = incidence1day*10, label = paste0(label, "\n", round(incidence1day*10, 1), "/1,000,000"), color= label),
                   hjust=0.5, 
                   xlim=c(max(bt_long$date), as.Date(NA)))+
  guides(color="none") +
 # annotate("text", x=as.Date(max(bt_long$date-20)), y=2, label=paste0("Crude relative protection of\nvaccinated vs unvaccinated: ", round(100*VE.recent$VE.death,1), "%"), hjust=0, color= "#B24745ff")+
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
MAdeaths_byvaccination
ggsave("MAhosp_byvaccination.pdf", width=16, height=12)

MAincidence_byvaccination + (MAhosp_byvaccination + MAdeaths_byvaccination) + plot_layout(ncol=1)
ggsave("MA_byvaccination.pdf", width=16, height=16)

# dates of available weeks with data
dates_avail <- as.Date((read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough"))$date)
  


mdph_bt_cases %>%
  select(date, VE.infection, VE.hosp, VE.death) %>% 
  filter(date > as.Date("2021-08-01")) %>%
  filter(date %in% dates_avail) %>%
  rename("Confirmed Infection" = VE.infection,
         "Hospitialization" = VE.hosp, 
         "Death" = VE.death) %>%
  pivot_longer(!date, names_to = "endpoint", values_to = "VE") %>%
  mutate(endpoint = fct_relevel(as.factor(endpoint), c("Confirmed Infection", "Hospitialization", "Death"))) %>%
  ggplot() +
  geom_smooth(aes(x=date, y=VE, group=endpoint, color=endpoint), se = FALSE, size=2) +
  coord_cartesian(ylim=c(0,1)) + 
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)), 
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5))) +
  labs(x="Date",
       y="Population Vaccination Effectiveness",
       title="Covid-19 Vaccination Effectiveness, Massachusetts",
       legend = "",
       subtitle = paste0("Estimated from overall incidence and weekly reported deaths\nin vaccinated residents | Data through ", as.Date(max(bt_long$date)))) +
  coord_cartesian( ylim=c(0,1) ) +
  scale_colour_jama(palette = c("default"), alpha = 1, name = "Outcome") 
ggsave("vaccine effectiveness.pdf", width=8, height=6)



## revised
library(tidyverse)
library(readxl)
library(lubridate)

load("mdph_testingbydate.Rdata")
load("vaccine.town.Rdata")
mdphfile<- "mdphcase_recent.xlsx"


bt.tally<-left_join(
  left_join(
    mdph_testingbydate %>%  ##total MPHD cases
      mutate(newpositive = if_else(is.na(newpositive), 0, newpositive),
             cases = cumsum(newpositive),
             date= date+5) %>%
      select(date, cases) %>%
      rename(total.cases = cases) %>%
      filter(date %in% dates_avail),
    
    read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Breakthrough")  %>%  ##breakthrough cases reported
      mutate(date = ymd(date)) %>%
      select(date, cases, hospitalization, death) %>%
      rename(bt.cases = cases,
             bt.hosp = hospitalization,
             bt.deaths = death) %>%
      mutate(
             
             # starting 15 March 2022, definition of covid death updated by DPH, 
             # approximately 12.5% of deaths no longer classified as covid death
             bt.deaths = case_when(
               date <= as.Date("2022-03-07") ~ round(bt.deaths*0.875, 0), 
               TRUE ~ bt.deaths)) %>%
      filter(date %in% dates_avail), by = "date"), 
  hosp.and.deaths %>% mutate(date = date+5), by = "date") %>%
  mutate(unvax.cases= total.cases-bt.cases,
         unvax.hosp = hospitalizations-bt.hosp,
         unvax.deaths = deaths-bt.deaths) %>%
  select(date, bt.cases, unvax.cases, bt.hosp, unvax.hosp, bt.deaths, unvax.deaths) %>%
  mutate(days = as.numeric(date-lag(date,1)), 
         unvax.cases = (unvax.cases-lag(unvax.cases, 1)),
         unvax.hosp = (unvax.hosp-lag(unvax.hosp, 1)),
         unvax.deaths = (unvax.deaths-lag(unvax.deaths, 1)),
         bt.cases = (bt.cases-lag(bt.cases, 1)),
         bt.hosp = (bt.hosp-lag(bt.hosp, 1)),
         bt.deaths = (bt.deaths-lag(bt.deaths, 1)))


bt.tally %>% #filter(date>as.Date("2021-12-15")) %>%
  summarise( days = sum(days, na.rm = TRUE),
             unvax.cases  = sum(unvax.cases , na.rm = TRUE) ,
             unvax.hosp  = sum(unvax.hosp , na.rm = TRUE) ,
             unvax.deaths  = sum(unvax.deaths , na.rm = TRUE) ,
             bt.cases  = sum(bt.cases , na.rm = TRUE) ,
             bt.hosp  = sum(bt.hosp , na.rm = TRUE) ,
             bt.deaths  = sum(bt.deaths , na.rm = TRUE) ) %>%
  mutate( pct.bt.hosp = bt.hosp *100/bt.cases ,
          pct.unvax.hosp = unvax.hosp *100/unvax.cases ,
          pct.bt.deaths = bt.deaths *100/bt.cases ,
          pct.unvax.deaths = unvax.deaths *100/unvax.cases ,
  )  %>%
  select(-days)

outcomes.ma<- bt.tally %>%
  mutate(pct.unvax.hosp = unvax.hosp *100/lag(unvax.cases,0),
         pct.bt.hosp = bt.hosp *100/lag(bt.cases,0),
         pct.bt.deaths = bt.deaths *100/lag(bt.cases,0) ,
         pct.unvax.deaths = unvax.deaths *100/lag(unvax.cases,0) ) %>%
  select(date, pct.unvax.hosp, pct.bt.hosp, pct.bt.deaths, pct.unvax.deaths ) %>%
  pivot_longer(!date, names_to = "type", values_to = "percent") %>%
        mutate(status = if_else(type== "pct.unvax.hosp" | type== "pct.unvax.deaths",
                                "unvaccinated", "fully vaccinated"),
               endpoint = if_else(type== "pct.unvax.hosp" | type== "pct.bt.hosp",
                                "hospitalized", "died")) 

bt.outcomes<-  outcomes.ma %>%
  ggplot()+
  geom_line(aes(x=date, y=percent, group=type, color=paste0(status, type)),size=2, alpha=1)+
  geom_hline(yintercept=0, linetype="dotted")+
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)), 
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Percent of Confirmed Covid-19 Cases",
       title="Hospitalization and Death among Confirmed Covid-19 Cases, Massachusetts",
       subtitle = paste0("Weekly percentages without assuming lag between diagnosis and outcome")) +
  coord_cartesian(xlim=c(as.Date("2021-07-26"), as.Date(max(bt_long$date+30)) ), ylim=c(0,10) ) +
  scale_colour_manual(values = c("#3C5488E5",  "#00A087E5", "#8491B4E5","#91D1C2E5")) +     # 1,3 deaths  2,4 hosp
  geom_label_repel(data = outcomes.ma %>% filter(date == max(outcomes.ma$date)),
                   aes(x = date, y = percent, label = paste0(round(percent, 1), ("% of\n"), status, " cases \n", endpoint), color= paste0(status, type)),
                   hjust=0.5, min.segment.length = 0,
                   xlim=c(max(outcomes.ma$date), as.Date(NA)))+
  guides(color="none") +  
    scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
bt.outcomes


junejuly.baselineVE<- mdph_bt_cases %>% filter(date < as.Date("2021-07-19")) %>%
  summarise(VE.infection.baseline = mean(VE.infection), 
            VE.hosp.baseline = mean(VE.hosp), 
            VE.death.baseline = mean(VE.death) )

VE.rel<- mdph_bt_cases %>%
  mutate("Infection" = VE.infection/junejuly.baselineVE$VE.infection.baseline,
         "Hospitalization" = VE.hosp/junejuly.baselineVE$VE.hosp.baseline,
         "Death" = VE.death/junejuly.baselineVE$VE.death.baseline,) %>% select(date, "Infection",  "Hospitalization",  "Death") %>%
  pivot_longer(!date, names_to = "Outcome", values_to = "Rel") %>%
  mutate(Outcome = fct_relevel(Outcome, "Infection", "Hospitalization", "Death"))

VE.comparison <- VE.rel %>%
  ggplot(aes(x=date, y= Rel, group=Outcome, color=Outcome, fill=Outcome))+
  geom_hline(yintercept = 1)+
  geom_smooth(span = 0.4, size=2) +
  theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  coord_cartesian(ylim = c(0.3, 1.3), xlim = c(as.Date("2021-08-19"), as.Date(NA)))+
  scale_x_date(date_breaks = "week", labels = scales::label_date_short()) +
  scale_color_jama(palette = c("default"), name="Outcome") +
  scale_fill_jama(palette = c("default"), name="Outcome") +
  labs(x="Date", y="Ratio (Observed / Baseline)",
       title="Relative Vaccine Effectiveness",
       subtitle = paste0("Compared with baseline period (19 June to 18 August 2021) | Most recent data: ", 
                         max(VE.rel$date)))
VE.comparison

