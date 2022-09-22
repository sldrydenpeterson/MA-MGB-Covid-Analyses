library(tidyverse)
library(readxl)
library(ggsci)
library(patchwork)
library(ggrepel)
library(lubridate)

#vaccine data MDPH https://www.mass.gov/info-details/covid-19-vaccination-program#weekly-covid-19-vaccination-report-

vaccine <-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
  mutate(date=lubridate::ymd(date),
         weekly.fullvax=full.vax.All-lag(full.vax.All,1)) %>%
  dplyr::select(date, weekly.fullvax)
# 
# MAweekly<-MassCovid1 %>%
#   group_by(date) %>%
#   dplyr::summarise(MAcases=sum(cases)) %>%
#   mutate(MAweeklycases=MAcases-(lag(MAcases,7)),
#          CDCestMAweeklycases=round(4.6*MAweeklycases,0),
#          CDCestLBcases=round(4*MAweeklycases, 0),
#          CDCestUBcases=round(5.4*MAweeklycases,0)
#   )
# 
# vaccine<- full_join(MAweekly, vaccine1, by="date")

MApopulation<-tribble(             #https://sites.tufts.edu/jamesjennings/files/2018/06/reportsBlackComparativeExperience2015.pdf
  ~group, ~pop, ~label,
  "AI.AN", 11600, "American Indian\nAlaska Native",
  "Multi", 128000, "Multi-Racial",
  "NH.PI", 2700, "Native Hawaiian\nPacific Islander",
  "Asian", 492900, "Asian",
  "Black", 509200, "Black",
  "Hispanic", 859100, "Latinx",
  "White", 4955500,  "White"
)

MApopulation%>%
  summarize(totalpop=sum(pop))

vaccine2 <- left_join(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
                        mutate(date=lubridate::ymd(date),
                               weekly.fullvax=full.vax.All-lag(full.vax.All,1)) %>%
                        dplyr::select(date, Asian, Black, Hispanic, AI.AN, White, NH.PI, Multi) %>%
                        pivot_longer(!date, names_to = "group", values_to = "fullvaccinated"),
                      MApopulation, by="group") %>%
  mutate(vaccine_incidence=fullvaccinated*100/pop)

library(ggsci)
vac1<-vaccine2 %>%
  filter(!is.na(vaccine_incidence)) %>%
  ggplot()+
  geom_line(aes(x=date, y=vaccine_incidence, group=as.factor(group), color=as.factor(group)), size=2)+
  theme_classic()+ theme(aspect.ratio = 1)+
  scale_colour_jama(palette = c("default"), alpha = 0.9)+
  labs(x="Date", y="Proportion of Estimated Population",
       title="SARS-CoV-2 Vaccination by Racial/Ethnic Group\nin Massachusetts | Fully-Vaccinated Individuals",
       caption="Source: MDPH utilizing population estimates from the Donahue Center at UMass")+
  guides(color="none") +
  coord_cartesian(xlim = c(as.Date("2021-02-12"), as.Date(last(vaccine2$date)+90)),
                  ylim=c(0, 100))+
  scale_x_date(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  geom_label_repel(data=vaccine2 %>% filter(date == last(date)),
                   aes(label=paste0(label, "\n", round(vaccine_incidence,1),"%"), x=date, y = vaccine_incidence, color=as.factor(group)) ,
                   min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA))) +
 geom_label_repel(data=vaccine2 %>% filter(date == last(date)) %>%
                    summarize(pop =sum(pop, na.rm = TRUE), fullvaccinated = sum(fullvaccinated, na.rm = TRUE)) %>%
                    mutate(ma.proportion = round(fullvaccinated/pop*100,1),
                           x =as.Date("2021-03-15"), y=as.numeric(80)),
                 aes(label=paste0("Fully-Vaccinated\nMassachusetts\nResidents:\n", ma.proportion,"%"), x=x, y = y), color = "black" )

MApopulationage<-tribble(           
  ~group, ~pop, ~label,
  "a5to11", 514700, "5 to 11",
  "a12to15", 322200, "12 to 15",
  "a16to19", 377000, "16 to 19",
  "a20to29", 1026800, "20 to 29",
  "a30to49", 1756000, "30 to 49",
  "a50to64", 1417200, "50 to 64",
  "a65to74", 682800,  "65 to 74",
  "a75over", 493300,  "75+",
)

vaccine3 <- left_join(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
                        mutate(date=lubridate::ymd(date)) %>%
                        dplyr::select(date, a5to11, a12to15, a16to19, a20to29, a30to49, a50to64, a65to74, a75over) %>%
                        pivot_longer(!date, names_to = "group", values_to = "fullvaccinated"),
                      MApopulationage, by="group") %>%
  mutate(vaccine_incidence=fullvaccinated*100/pop) %>%
  filter(!is.na(vaccine_incidence))

vac2<-vaccine3 %>%
  ggplot()+
  geom_line(aes(x=date, y=vaccine_incidence, group=as.factor(group), color=as.factor(group)), size=2)+
  theme_classic()+ theme(aspect.ratio = 1)+
  scale_colour_jco()+
  labs(x="Date", y="Percent of Estimated Population",
       title="SARS-CoV-2 Vaccination by Age Group\nin Massachusetts | Fully-Vaccinated Individuals",
       caption="Source: MDPH utilizing population estimates the from Donahue Center at UMass")+
  guides(color="none") +
  coord_cartesian(xlim = c(as.Date("2021-02-12"), as.Date(last(vaccine3$date)+90)),
                  ylim=c(0, 100))+
  scale_x_date(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_label_repel(data=vaccine3 %>% filter(date == last(date)),
                   aes(label=paste(label, "years\n", round(vaccine_incidence,1),"%"), x=date, y = vaccine_incidence, color=as.factor(group)) ,
                   min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA))
  )

library(patchwork)
vaccinationgroups <-vac2 + vac1 + plot_layout(ncol=2)
vaccinationgroups
ggsave("vaccinationgroups.pdf", width =16, height=10)

vac1
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/vaccine by race.jpg", plot = last_plot(), device = "jpeg",
       scale = 1,  units = c("in"),height = 8.67, width = 8.67,
       dpi = 160, limitsize = TRUE)

vaccine.doses <-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine") %>%
  select(date, weekly.doses) %>%
  mutate(date = ymd(as.Date(date)))

total.vax <-(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")) %>%
   filter(date == max(as.Date(date))) %>%
    mutate(
      p.fullvax= round(full.vax.All*100/7029917, 1),
      y.label= 520, 
      x.label= as.Date("2020-12-31")
    )


vaccine.dose.plot<-vaccine.doses %>% ungroup() %>%
  ggplot()+
  theme_classic() +  #theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
  #     axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_col(aes(x=date, y=weekly.doses/1000), color="#374E5599")+
 # geom_line(aes(x=date, y=rolling7day), color="#B24745ff", size=1.5)+
  labs(x="Date", y="Number Doses per Week (in 1000s)",
       title="SARS-CoV-2 Vaccination Administrations in Massachusetts",
       subtitle = paste0("Last data available: ", max(vaccine.doses$date), " | Includes first and second doses"),
       caption="Source: Massachusetts Department of Public Health" )+
  guides(color=FALSE) +
  geom_label_repel(data= total.vax, aes(x=x.label, y=y.label, label=paste0(p.fullvax, "%\n of MA Residents\nwith Complete Vaccination")),
                   max.overlaps = 100, color="#3182bd") +
  coord_cartesian( xlim=c(as.Date("2020-12-01"), Sys.Date()+20))+
  scale_x_date(breaks = scales::pretty_breaks(10), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks())
vaccine.dose.plot
ggsave("vaccine doses in MA.pdf")




