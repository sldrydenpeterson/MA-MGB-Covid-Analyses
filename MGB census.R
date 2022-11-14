library(ggsci)
library(tidyverse)
library(readxl)
library(ggrepel)
#import inpatient and MDPH numbers from csv, fix date, and make long dataset
MGBcensus <-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MGB census")  %>% filter(!is.na(Brigham))
MGBcensus$date <-(lubridate::ymd(MGBcensus$date))

#MGB acute care hospital beds
hospitalname<- c("Mass General", "Brigham", "NSMC-Salem", "Newton-Wellesley",
                 "Wentworth-Douglass", "Brigham\nFaulkner", "Cooley-Dickinson", 
                 "Martha's Vineyard", "Nantucket Cottage", "Brigham CoV-Risk", "All MGB Acute\nHospitals", "MGB Vaccinated\nInpatients", "MGB Unvaccinated\nInpatients")
beds<-c(1057, 793, 395, 273, 178, 162, 140, 25, 19, NA, 3042, NA, NA)
Hospital<-c("Mass.General", "Brigham", "NSMC.Salem","Newton.Wellesley","Wentworth.Douglass", 
            "Brigham.Faulkner", "Cooley.Dickinson", "Marthas.Vineyard", "Nantucket.Cottage",
            "BWH.PUI", "All.MGB.Acute", "All.MGB.Acute.vax", "All.MGB.Acute.unvax")

#make combined date frame
InpatientBeds<- data.frame(hospitalname,Hospital, beds)

#Create long datasets, separately for hospitals
MGBcensuslong <- left_join(MGBcensus %>%
  pivot_longer(!date, names_to = "Hospital", values_to = "Cases"),
  InpatientBeds, by = "Hospital") %>%
  mutate(PercentCovid = Cases/beds) %>%
  filter(!is.na(Cases))



# BWH census
#limit to BWH, BWFH, and all combined
bwhonly<- MGBcensuslong %>%
  filter(Hospital == "Brigham" | Hospital == "Brigham.Faulkner" | Hospital == "Mass.General" | Hospital == "All.MGB.Acute")
  

mgbcensus<- bwhonly%>%
  filter(!is.na(Cases)) %>%
  ggplot( aes(x=date, group=hospitalname, color=hospitalname)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_hline(yintercept = 250, linetype = "dashed") +
  theme_classic() + theme(aspect.ratio = 0.75, plot.title = element_text(size = rel(1.5)),
                         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=Cases), alpha=.7, size=1.5) +
  scale_color_jama(palette = c("default"), alpha = .8) +
  # scale_color_viridis(discrete = TRUE, end=0.5, option = "D")+
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



##Vaccine and inpatients


# inpatient.vax<- MGBcensuslong %>%
#   filter(Hospital == "All.MGB.Acute.vax" | Hospital == "All.MGB.Acute.unvax")
# 
# load("vaccine.town.Rdata")
# 
# adults.fullvax <- vaccine.town %>%
#   filter(reportdate == max(reportdate) & Age_group != "12-15 Years" & Age_group != "15-19 Years" & Age_group != "Total") %>%
#   summarise(total.fullvax = sum(fullvax, na.rm = TRUE))
# adults.fullvax <- adults.fullvax$total.fullvax
# adults.nofullvax <- (7029917*(1-0.196)) - adults.fullvax
# 
# 
# inpatient.calc <- inpatient.vax %>%
#   filter(date > max(date)-7) %>% #limit to past week
#   mutate(total.days = sum(Cases)) %>%
#   group_by(hospitalname) %>% #vaccination status
#   mutate(hosp.days=sum(Cases)) %>%
#   distinct(hospitalname, .keep_all = TRUE) %>%
#   select(Hospital, hospitalname, total.days, hosp.days) %>%
#   mutate(p = hosp.days/total.days)
# 
# vax.inpatient.days<- inpatient.calc[inpatient.calc$Hospital=="All.MGB.Acute.vax", "hosp.days"]
# unvax.inpatient.days<- inpatient.calc[inpatient.calc$Hospital=="All.MGB.Acute.unvax", "hosp.days"]
# VE.inpatient <- 1- ((vax.inpatient.days/adults.fullvax)/(unvax.inpatient.days/adults.nofullvax))
# VE.inpatient<-VE.inpatient$hosp.days
# 
# inpatient.vax.plot<- inpatient.vax %>%
#   filter(!is.na(Cases)) %>%
#   ggplot( aes(x=date, group=hospitalname, color=hospitalname)) +
#   geom_hline(yintercept=0, linetype="dotted") +
#   theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_line(aes(y=Cases), alpha=.7, size=1.5) +
#   scale_color_jama(palette = c("default"), alpha = .8) +
#   # scale_color_viridis(discrete = TRUE, end=0.5, option = "D")+
#   geom_label_repel(data= inpatient.vax%>%
#                      filter(date == max(date)) %>% mutate(pct = Cases/sum(Cases)), aes(label= paste0(hospitalname, "\nn=",Cases, " (", round(pct*100,0), "%)"),
#                                                     x = date ,  y = Cases),
#                    hjust=0.5,
#                    xlim=c(Sys.Date(), as.Date(NA)), min.segment.length = 0)+
#  # annotate("text", x=as.Date("2021-06-21"), y=35, label=paste0("Crude (population-adjusted only)\nvaccine effectiveness\nfor hospitalization days: ", round(100*VE.inpatient,0), "%"), hjust=0, color= "#B24745ff")+
#   coord_cartesian(xlim=c(as.Date("2021-06-01"), Sys.Date()+90), ylim=c(0,NA))+
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short()) +
#   labs(x="Date",
#        y="Number (@ 6am)",
#        title="MGB Covid-19 Inpatients, by Vaccination Status",
#        subtitle = max(bwhonly$date),
#        caption="Source: Daily inpatient hospital censuses (Covid-19 Epic flag)" )+
#   guides(color="none")
# 
# inpatient.vax.plot




## DPH inpatient numbers and percent


load(file="~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/mdph_hosp.Rdata") 

mdph.inpatient <-rbind(
  mdph_hosp %>%
    filter(date > as.Date("2020-03-01")) %>%
    rename(Unvaccinated=inpatient.unvax,
         Vaccinated=inpatient.vax,
         Intubated = intubated) %>%
    pivot_longer(!date, names_to = "group", values_to = "census") %>%
      mutate(date = ymd(as.Date(date)),
             year = "Current"),
  
  mdph_hosp %>%
    filter(date > as.Date("2020-03-01")) %>%
    rename(Unvaccinated=inpatient.unvax,
           Vaccinated=inpatient.vax,
           Intubated = intubated) %>%
    pivot_longer(!date, names_to = "group", values_to = "census") %>%
    mutate(date = ymd(as.Date(date)),
           date = date+365,
           year = "Last Year") ) %>%
  mutate(year = fct_relevel(year, "Last Year", "Current"))

max.inpatient.date <- (mdph.inpatient %>% filter(year == "Current" & group == "inpatient.total") %>% filter(date == max(date, na.rm = TRUE)))$date

mdph.inpatient.plot <- mdph.inpatient %>%
  filter(group == "inpatient.total") %>%
ggplot( aes(x=date, group=year, color=year)) +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=census), alpha=.7, size=2.5) +
  scale_color_jama(palette = c("default"), alpha = .8) +
  # scale_color_viridis(discrete = TRUE, end=0.5, option = "D")+
  geom_label_repel(data= mdph.inpatient %>%
                     filter(group == "inpatient.total") %>%
                     group_by(group) %>%
                     filter(date == max.inpatient.date), 
                   aes(x = date ,  y = census, 
                       label=paste0(year, "\n", census, " patients")),
                   hjust=0.5,
                   xlim=c(Sys.Date(), as.Date(NA)), min.segment.length = 0) +
  coord_cartesian(xlim=c(Sys.Date()-365+90, Sys.Date()+90), ylim=c(0,NA)) +
  scale_x_date(breaks = scales::pretty_breaks(12), labels=scales::label_date_short()) +
  labs(x="Date",
       y="Number of Inpatients",
       title="Covid-19 Inpatients in Massachusetts",
       subtitle = paste0("7-Day running average for current and prior year | Last data: ", max.inpatient.date),
       caption="Source: Collected by MDPH from Massahcusetts hospitals." )+
  guides(color="none")
  mdph.inpatient.plot
 
  ggsave("mdph.inpatient.plot.pdf", width = 12, height = 8) 


 
 pct.mdph.inpatient.plot<- 
   mdph.inpatient %>%
   filter(group == "pct.covid" & !is.na(census)) %>%
   ggplot() +
   annotate("rect", ymin=2, ymax=4, xmin=as.Date("2020-01-01"), xmax=as.Date("2025-01-01"), fill="#00A1D599")+
   annotate("text", y=3, x=Sys.Date()-365+90, label= "Range of Peak Influenza\nInpatient Percentage (2017-2020)", color="#374E55FF", hjust=0)+ 
   geom_hline(yintercept=0, linetype="dotted") +
   theme_classic() + 
   theme(plot.title = element_text(size = rel(1.5)),
         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
   geom_line(aes(x=date, y=census, group=year, color = year), alpha=.7, size=2.5) +
   scale_color_jama(palette = c("default"), alpha = .8) +
    guides(color="none") +
   coord_cartesian(xlim=c(Sys.Date()-365+90, Sys.Date()+90), ylim=c(0,30)) +
    geom_label_repel(data= mdph.inpatient %>%
                       filter(group == "pct.covid") %>%
                       group_by(group) %>%
                       filter(date == max.inpatient.date), 
                     aes(label= paste0(year, "\n", round(census, 1), "%"),
                         x = date ,  y = census, color=year),
                     hjust=0.5, xlim=c( max.inpatient.date, as.Date(NA)), min.segment.length = 0) +
   scale_x_date(breaks = scales::pretty_breaks(12), labels=scales::label_date_short()) +
   scale_y_continuous(breaks = scales::pretty_breaks(10)) +
   labs(x="Date",
        y="Percent of inpatients with Covid-19",
        title="Inpatient Covid-19, Massachusetts",
        subtitle = paste0("7-Day running average for current and prior year | Last data: ", max.inpatient.date),
        caption="Source: Collected by MDPH from Massachusetts hospitals." ) +
   guides(color="none")
 
 pct.mdph.inpatient.plot

 ggsave("pct.mdph.inpatient.plot.pdf", width = 12, height = 8)
 
 



 
load(file="mdph.deaths.Rdata") 
 deaths.year<-
   rbind(
     mdph.deaths %>% mutate(year = "Current"),
     mdph.deaths%>%
       filter(date > as.Date("2020-03-01")) %>%
       mutate(date = date+365, 
              year = "Last Year") %>% 
       select(date, confirmed_deaths, deaths7d.avg, year))%>%
   mutate(year = fct_relevel(year, "Last Year", "Current"))
 
 
 mdph.deaths.plot <-
   ggplot() +
   geom_hline(yintercept=0, linetype="dotted") +
   theme_classic() + 
   theme(plot.title = element_text(size = rel(1.5)),
         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
   geom_hline(yintercept = 34,linetype="dotted") +
   annotate(geom = "text", label = "Expected daily\ndeaths from cancer", x= Sys.Date()-180, y= 35, vjust=0, hjust=1)+
   geom_col(data = deaths.year %>% filter(year == "Current"), aes(x=date, y=confirmed_deaths), color="#8A9197FF")+
   geom_line(data= deaths.year, aes(x=date, y=deaths7d.avg, group=year, color=year),  size=2.5) +

   scale_color_jama( alpha = .8) +
   geom_label_repel(data= deaths.year %>%
                      filter(date == max(mdph.deaths$date)), 
                    aes(label= paste0(year, "\n7-day avg, ", round(deaths7d.avg, 1)), x = date ,  y = deaths7d.avg, color=year),
                    hjust=0.5,  min.segment.length =0 , nudge_x = 30, nudge_y = 10)+
   coord_cartesian(xlim=c(Sys.Date()-365+90, Sys.Date()+90), ylim=c(0,100))+
   scale_x_date(breaks = scales::pretty_breaks(8), labels=scales::label_date_short()) +
   labs(x="Date (current year)",
        y="Daily Deaths",
        title="Covid-19 Deaths in Massachusetts",
        subtitle = paste0("7-Day running average for current and prior year | Last data: ", max(mdph.deaths$date)),
        caption="Source: MDPH" )+
   guides(color="none") 
 mdph.deaths.plot
 
 ggsave("mdph.deaths.plot.pdf", width = 12, height = 8)
 
# 
#  
#  library(patchwork)
# (wastewater.recent +  theme(plot.margin = unit(c(30,30,30,30), "pt"))) +  (ma.recent +  theme(plot.margin = unit(c(30,30,30,30), "pt"))) + 
#    (mdph.inpatient.plot +  theme(plot.margin = unit(c(30,30,30,30), "pt"))) + (mdph.deaths.plot +  theme(plot.margin = unit(c(30,30,30,30), "pt"))) + plot_layout(ncol=2)
#  ggsave("comparison metrics.pdf", width=16, height=16)
#  
#  
#  mdph.inpatient.vent <-mdph_hosp %>%
#    mutate(date = ymd(as.Date(date)),
#           floor.inpatients = inpatient.total-intubated) %>%
#    select(date, intubated, floor.inpatients) %>%
#    pivot_longer(!date, names_to = "vent.status", values_to = "census")
#  
#  
#  inpatient.year.vent<-
#    rbind(
#      mdph.inpatient.vent %>% mutate(year = "Current"),
#      mdph.inpatient.vent %>%
#        filter(date > as.Date("2020-03-01")) %>%
#        mutate(date = date+365, 
#               year = "Last Year") %>% select(date, vent.status, census, year) )%>%
#    mutate(year = fct_relevel(year, "Last Year", "Current")) %>%
#    filter(vent.status == "intubated")
#  
#  library(ggsci)
#  library(ggrepel)
#  mdph.inpatient.vent.plot <-
#    ggplot() +
#    geom_hline(yintercept=0, linetype="dotted") +
#    theme_classic() + 
#    theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#    geom_line(data= inpatient.year.vent, aes(x=date, y=census, group=year, color=year),  size=2.5) +
#    scale_color_jama( alpha = .8) +
#    geom_label_repel(data= inpatient.year.vent %>%
#                       filter(date == max(mdph.inpatient.vent$date)),
#                     aes(label= paste0(year, "\nIntubated patients, n=",census), x = date ,  y = census, color=year),
#                     hjust=0.5,  min.segment.length =0 , nudge_x = 120, nudge_y= 180)+
#    coord_cartesian(xlim=c(Sys.Date()-365+90, Sys.Date()+90), ylim=c(0,350))+
#    scale_x_date(breaks = scales::pretty_breaks(8), labels=scales::label_date_short()) +
#    labs(x="Date (current year)",
#         y="Number of Inpatients",
#         title="Intubated Covid-19 Inpatients in Massachusetts",
#         subtitle = paste0("7-Day running average for current and prior year | Last data: ", max(mdph.inpatient.vent$date)),
#         caption="Source: Collected by MDPH from Massachusetts hospitals." )+
#    guides(color="none")
#  mdph.inpatient.vent.plot
#  
#  coord_cartesian(xlim=c(Sys.Date()-365+90, Sys.Date()+90), ylim=c(0,100))+
#    scale_x_date(breaks = scales::pretty_breaks(8), labels=scales::label_date_short()) +
#  
#  ggsave("mdph.inpatient.vent compare.pdf", width=10)
#  
 