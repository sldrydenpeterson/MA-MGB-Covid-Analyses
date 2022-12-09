library(tidyverse)
library(janitor)
library(lubridate)
library(RcppRoll)
library(ggrepel)

Outpatientdiagnosis<- 
  rbind(
    #Salem
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.salem.csv")) %>%
  clean_names() %>%
  ungroup() %>%
  mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
  select(-c(report_dts, metric_nm)) %>%
  pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
  clean_names() %>%
  mutate(first_ed_positives = replace_na(first_ed_positives, 0),
         icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
         
         icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
         week = as.Date(cut(date, "week"))) %>%
  group_by(week) %>%
  summarize(first_ed_positives = sum(first_ed_positives),
            icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
  ungroup() %>%
  mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
         hospital = "Salem Hospital",
         outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #NWH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.nwh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
           hospital = "Newton-Wellesley Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #BWH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.bwh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
           hospital = "Brigham and Women's Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #MGH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.mgh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
                     hospital = "Mass General Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #CDH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.cdh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
               hospital = "Cooley Dickinson Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #WDH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.cdh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
           hospital = "Wentworth-Douglass Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #BWFH
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.bwfh.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
           hospital = "BW Faulkner Hospital",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA)),
  
  #Island
  tibble(read_csv("~/Dropbox (Partners HealthCare)/Covid/Early therapy/program data/covid admissions/ws_Epi__Trend_Lines_data.island.csv")) %>%
    clean_names() %>%
    ungroup() %>%
    mutate(date = mdy(paste0(report_dts, ", 2022"))) %>%
    select(-c(report_dts, metric_nm)) %>%
    pivot_wider(names_from = c_dynamic_epi_event_grouper, values_from = c_epi_curve_dyn_measure) %>%
    clean_names() %>%
    mutate(first_ed_positives = replace_na(first_ed_positives, 0),
           icu_and_general_admissions = replace_na(icu_and_general_admissions, 0),
           icu_and_general_admissions = if_else(icu_and_general_admissions< first_ed_positives, first_ed_positives, icu_and_general_admissions),
           week = as.Date(cut(date, "week"))) %>%
    group_by(week) %>%
    summarize(first_ed_positives = sum(first_ed_positives),
              icu_and_general_admissions = sum(icu_and_general_admissions)) %>%
    ungroup() %>%
    mutate(outpatientdx.pct = 100- (100*first_ed_positives/icu_and_general_admissions),
           hospital = "Martha's Vineyard and Nantucket Hospitals",
           outpatientdx.pct.4wk = roll_mean(100- (100*first_ed_positives/icu_and_general_admissions), n = 8, align = "right", fill = NA))
  ) %>%
  filter(week < Sys.Date()-7)

Outpatientdiagnosis %>%
  ggplot() +
  theme_classic() +
  facet_wrap(vars(hospital), ncol=4) +
  geom_hline(yintercept = 60, linetype = "dashed") +
  annotate("text", x = min(Outpatientdiagnosis$week), y = 62, label = "Testing access\nperformance target", vjust = 0, hjust = 0) +
  geom_point(aes(x=week, y=outpatientdx.pct), shape = 21, fill = "#00A1D5FF") +
  geom_line(aes(x=week, y=outpatientdx.pct.4wk), size=2.5, color ="#B24745ff" ) +
  coord_cartesian(xlim=c(as.Date("2022-04-01"), Sys.Date()+100), ylim=c(0,100)) +
  geom_label_repel(data =  Outpatientdiagnosis %>%
                     group_by(hospital) %>%
                     filter(week == max(week)),
            aes(x=week, y = outpatientdx.pct.4wk, label = paste0(hospital, " \n  ",round(outpatientdx.pct.4wk, 0), "% for past 4 weeks  " )),
            min.segment.length = 0, nudge_x = 30, nudge_y = -20) +
  scale_x_date(date_breaks = "1 month",  labels=scales::label_date_short()) + 
  labs(x="Week", y="Percent of admitted patients",
       title="COVID admissions with preceding outpatient diagnosis, MGB hospitals",
       subtitle = paste0("Weekly percent and 8-week running average | Most recent data: ", max(Outpatientdiagnosis$week)),
       caption = "Source: Data aggregated from MGB Surveillance Tableau page") 
ggsave("Outpatientdiagnosis.pdf", width = 16, height =8)



  