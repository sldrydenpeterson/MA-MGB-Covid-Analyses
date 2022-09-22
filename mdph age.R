library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(ggsci)
library(ggrepel)
library(patchwork)


age.pop<-read_excel("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/covid-19-raw-data-6-11-2021.xlsx", sheet="AgeLast2Weeks") %>%
  distinct(Age, Population_Estimate_N) %>% filter(!is.na(Population_Estimate_N)) %>%
  mutate(age = if_else(Age=="0-19", as.character(NA), Age),
         pop = Population_Estimate_N,
         age.early = if_else(Age == "<5" | Age == "5-9" | Age == "10-14" | Age == "15-19",as.character(NA), Age)) %>%
  mutate(
    age = fct_recode(age, "Age 0-4" = "<5",
                     "Age 5-9" = "5-9",
                     "Age 10-14" = "10-14",
                     "Age 15-19" = "15-19",
                     "Age 20-29" = "20-29",
                     "Age 30-39" = "30-39",
                     "Age 40-49" = "40-49",
                     "Age 50-59" = "50-59",
                     "Age 60-69" = "60-69",
                     "Age 70-79" = "70-79",
                     "Age 80+" = "80+")) %>%
  select(age, pop) %>%
  filter(!is.na(age))


age<-left_join(read_csv("mdph_age.csv") %>%
  clean_names() %>%
  mutate(x0_19_years = if_else(!is.na(x0_4_years),  x0_4_years + x5_9_years + x10_14_years + x15_19_years, x0_19_years)) %>%
  select(-start_date, -end_date, -average_daily_incidence_rate_per_100_000_last_14_days) %>%
  mutate(x0_4_years = if_else(is.na(x0_4_years), x0_19_years*0.16, x0_4_years),
         x5_9_years = if_else(is.na(x5_9_years), x0_19_years*0.2, x5_9_years),
         x10_14_years = if_else(is.na(x10_14_years), x0_19_years*0.247, x10_14_years),
         x15_19_years = if_else(is.na(x15_19_years), x0_19_years*0.394, x15_19_years),
         date = ymd(as.Date(date))) %>%
  select(-x0_19_years) %>%
  pivot_longer(!date, names_to = "age", values_to = "cases") %>%
  mutate(age = fct_recode(age, "Age 0-4" = "x0_4_years",
                               "Age 5-9" = "x5_9_years",
                               "Age 10-14" = "x10_14_years",
                               "Age 15-19" = "x15_19_years",
                          "Age 20-29" = "x20_29_years",
                          "Age 30-39" = "x30_39_years",
                          "Age 40-49" = "x40_49_years",
                          "Age 50-59" = "x50_59_years",
                          "Age 60-69" = "x60_69_years",
                          "Age 70-79" = "x70_79_years",
                          "Age 80+" = "x80_years"),
         lifestage = fct_collapse(age, "Age 0-9" = c("Age 0-4", "Age 5-9"),
                                     "Age 10-19" = c("Age 10-14", "Age 15-19"),
                                      "Age 20-29" = c("Age 20-29"),
                                      "Age 30-49" = c("Age 30-39", "Age 40-49"),
                                      "Age 50-69" = c("Age 50-59", "Age 60-69"),
                                      "Age 70+"   = c("Age 70-79", "Age 80+"))),
  age.pop, by = "age") %>%
  group_by(date, lifestage) %>%
  summarize(cases = sum(cases),
            pop = sum(pop)) %>%
  mutate(incidence1day = (100000*cases/pop)/7)

library(ggsci)
age %>%
  ggplot(aes(x=date, y=incidence1day, group=lifestage, color=lifestage)) +
  theme_classic() + theme( plot.title = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(alpha=.7, size=1.5) +
  geom_line() +
  scale_color_npg() +
  geom_label_repel(data=age %>% ungroup() %>%
                     dplyr::filter(date == max(date)), aes(label= paste0(lifestage, "\n",round(incidence1day, 1), " per 100,000"),
                                                    x = date ,  y = incidence1day),
                   hjust=0.5, max.overlaps = 20,
                   xlim=c(Sys.Date(), as.Date(NA)))+
  coord_cartesian(xlim=c(as.Date("2021-02-15"), Sys.Date()+30), ylim=c(0,100))+
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short()) +
  labs(x="Date",
       y="Daily Incidence",
       title="Covid-19 Incidence, by Age Category, Massachusetts",
       subtitle = max(age$date),
       caption="Source: Mass Department of Public Health" )+
  guides(color=FALSE)
ggsave("incidence by age.pdf", width= 12, height=9)


