library(tidyverse)
library(ggsci)
library(lubridate)
library(ggrepel)

load("MGBcovidcases.Rdata")
load("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/earlytx.Rdata")



#all therapies

lastobs<- MGBcovidcases  %>%
  filter(age_in_years >= 50) %>%
  filter(dxdate >= max(dxdate)-6) %>%
  count() %>% mutate(n = n/7)

lastyear.cases<- as.numeric((MGBcovidcases %>%
                               filter(age_in_years >= 50) %>%
                               filter(dxdate >= as.Date("2021-09-27") & dxdate < as.Date("2023-03-30")) %>%
                               group_by(week)%>%
                               count()%>%
                               ungroup() %>%
                               rename(cases = n) %>%
                               mutate(lastyr.week = ymd(cut(week+365, "week"))) %>%
                               filter(lastyr.week == max(week)+7))$cases)

MGBcovidcases.proj <-
  MGBcovidcases %>%
  filter(age_in_years >= 50) %>%
  filter(dxdate >= as.Date("2021-09-27") & dxdate < as.Date("2022-03-30")) %>%
  group_by(week)%>%
  count()%>%
  ungroup() %>%
  rename(cases = n) %>%
  mutate(rel.cases = cases/lastyear.cases-1,
         "25% of 2021-2 increase" = (lastobs$n*(1+rel.cases*0.25)),
         "50% of 2021-2 increase" = (lastobs$n*(1+rel.cases*0.5)),
         "75% of 2021-2 increase" = (lastobs$n*(1+rel.cases*0.75)),
         "100% of 2021-2 increase" = (lastobs$n*(1+rel.cases*1.0)),
         week = week+365) %>%
  select(week, "25% of 2021-2 increase":"100% of 2021-2 increase") %>%
  filter(week >= max(MGBcovidcases$dxdate, na.rm = TRUE)) %>%
  pivot_longer(cols = "25% of 2021-2 increase":"100% of 2021-2 increase", names_to = "projection", values_to = "proj.cases")



MGBcases.50plus <-
  MGBcovidcases %>% #filter(dxdate <= as.Date("2022-10-01")) %>%
  filter(age_in_years >= 50) %>%
  group_by(dxdate)%>%
  count()%>%
  ungroup() %>%
  rename(cases = n) %>%
  mutate(avg7day = (cases + lag(cases, 1) + lag(cases, 2) + lag(cases, 3) + lag(cases, 4) + lag(cases, 5) +  lag(cases, 6))/7)

labels.proj <-
  MGBcovidcases.proj %>% group_by(projection) %>%
  filter(proj.cases == max(proj.cases, na.rm = TRUE))


mgbdx.plot.proj <-
  ggplot()+
  theme_classic()  + 
  geom_col(data= MGBcases.50plus, aes(x=dxdate, y=cases), fill="#43a2ca", alpha = 0.8) +
  geom_line(data= MGBcases.50plus, aes(x=dxdate, y= avg7day),  color="#1B1919ff", size=2) +
  geom_line(data = MGBcovidcases.proj, aes(x=week, y= proj.cases, color = projection, group = projection), size=1) +
  scale_color_jama(alpha = .9) +
  labs(x="Date", y="Daily Diagnoses",
       title="Recorded Covid-19 Cases among 50+, Mass General Brigham",
       subtitle = paste0("Seven-day running average shown in black | Possible trends fractional to epidemic intensity increase during 2021-22 winter surge | 1 in 5 Covid cases in 50+ are recorded"),
       caption = "Source: MGB testing data") +
  geom_label_repel(data = labels.proj,
                   aes(x= week, y= proj.cases, color = projection, 
                       label = paste0(projection, "\nPeak: ", signif(proj.cases,2), " cases/day")), hjust = 0.5,
                   xlim=c(as.Date("2023-01-20"), as.Date("2023-04-15")), min.segment.length = 0) +
  scale_x_date(date_breaks = "1 month",  labels=scales::label_date_short())+
  coord_cartesian(xlim = c(as.Date("2021-11-01"), as.Date(max(MGBcovidcases$dxdate, na.rm = TRUE)+180))) +
  guides(color = "none")
mgbdx.plot.proj

ggsave("mgbdx surge proj.pdf", width=13, height =8)




library("scales")
show_col(pal_jama("default")(7))
show_col(pal_jama("default", alpha = 0.6)(7))






infusion.doses <-  earlytx %>% filter(drug != "Nirmatrelvir" | drug == "Molnupiravir") %>%
      group_by(date) %>%
      tally(treated_num) %>%
      mutate(treated_avg = RcppRoll::roll_mean(n, n = 7, align = "right", fill = NA))

infusion.proj <- MGBcovidcases.proj %>%
      mutate(infusions.needed = proj.cases *0.08)

infusion.proj.labels <-
  infusion.proj %>% group_by(projection) %>%
  filter(infusions.needed == max(infusions.needed, na.rm = TRUE))


mgb.infusion.proj <-
  ggplot() +
  theme_classic()  + 
  geom_col(data = infusion.doses, aes(x = date, y = n), fill="#43a2ca", alpha = 0.8) +
  geom_line(data= infusion.doses, aes(x=date, y= treated_avg),  color="#1B1919ff", size=2) +
  geom_line(data = infusion.proj, aes(x=week, y= infusions.needed, color = projection, group = projection), size=1) +
  scale_color_jama(alpha = .9) +
  labs(x="Date", y="Daily Infusion starts",
       title="COVID Infusion Therapy, Mass General Brigham",
       subtitle = paste0("Seven-day running average shown in black | Projected needs fractional to epidemic intensity increase during 2021-22 winter surge"),
       caption = "Source: MGB treatment data") +
  geom_label_repel(data = infusion.proj.labels,
                   aes(x= week, y= infusions.needed, color = projection, 
                       label = paste0(projection, "\nPeak: ", signif(infusions.needed,2), " infusion starts")), hjust = 0.5,
                   xlim=c(as.Date("2023-01-20"), as.Date("2023-04-15")), min.segment.length = 0) +
  scale_x_date(date_breaks = "1 month",  labels=scales::label_date_short())+
  coord_cartesian(xlim = c(as.Date("2021-12-01"), as.Date(max(MGBcovidcases$dxdate, na.rm = TRUE)+180))) +
  guides(color = "none")
mgb.infusion.proj
ggsave("mgb.infusion.proj.pdf", width=13, height =8)
