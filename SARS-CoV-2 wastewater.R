library(tidyverse)
library(readxl)
library(viridis)
library(ggrepel)
library(ggsci)
library(patchwork)
setwd("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses")

load("mwra.wastewater.covid.Rdata")

top10wastewater <- sewage %>%
  filter(year == "Current") %>%
  mutate(week = cut(date, "week"),
         ) %>%
  group_by(week) %>%
  summarize(week.mean = sum(mean, na.rm = TRUE)) %>%
  arrange(desc(week.mean)) %>%
  mutate(Year = case_when(
    as.Date(week) < as.Date("2021-06-01") ~ "Last Year",
    as.Date(week) >= as.Date("2021-06-01") ~ "Current Year")) %>%
  slice(1:10) %>%
  ggplot(aes(x=week, y= week.mean, fill=Year)) +
  geom_col() +
  scale_fill_simpsons() +
  labs(x="Week", y="Weekly Viral Load (copies/mL)",
       title="Highest Weekly Measurements in Metro Boston",
       subtitle = paste0("Last available measurement: ", as.Date(last(sewage$date))),
       caption="Average of Southern and Northern sewer lines; Source: Massachusetts Water Resources Authority" )+
  theme_classic() +
  theme(aspect.ratio=.75, plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)), axis.text.x = element_text(angle = 90))
  

sewage %>% filter(year == "Current") %>%
  ggplot( aes(x=date, y=mean, color=as.factor(1)) )+
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_smooth(alpha=.7, size=2.5, method="loess", span=0.16, se=FALSE) +
  geom_point(alpha=.5, size=1.5) +
  #geom_point(data=sewage %>% filter(B117 == 1), aes(x=date, y=1300), shape= "|", color="#B24745ff", alpha=0.7, size=10) +
  #geom_text(aes(x=as.Date("2021-03-01"), y=1300, label="B.1.1.7 Detected:"),hjust=1, size=5, color="#B24745ff")+
  labs(x="Date", y="SARS-CoV-2 Load (copies/mL)",
       title="Sewage SARS-CoV-2 Concentration in Metro Boston",
       subtitle = paste0("Last available measurement: ", as.Date(last(sewage$date))),
       caption="Average of Southern and Northern sewer lines; Source: Massachusetts Water Resources Authority" )+
  scale_color_viridis(discrete = TRUE, end=0.85, option = "C")+
  guides(color="none") +
  coord_cartesian(xlim=c(as.Date("2020-03-11"), Sys.Date()+35), ylim=c(0,2500))+
  scale_x_date(breaks = scales::pretty_breaks(12), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  geom_hline(yintercept = 0, linetype ="dotted")
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/Wastewater.jpg", plot = last_plot(), device = "jpeg", 
       scale = 1,  units = c("in"),height = 8.67, width = 8.67,
       dpi = 300, limitsize = TRUE)
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/Wastewater.pdf", height = 8.67, width = 8.67)



sewage$index <- 1:nrow(sewage)  # create index variable

wastewater.smooth <-
  rbind( 
    cbind(sewage %>%
            filter(year == "Current") %>%
            mutate(mean.log10 = log10(mean)) %>%
            summarize(smoothed =  predict(loess(mean ~ index, span=0.035), na.rm = TRUE),
                      smoothed.log10 =  predict(loess(mean.log10 ~ index, span=0.035), na.rm = TRUE)),
          sewage %>% filter(year == "Current" & !is.na(mean)) %>% select(date)) %>%
      mutate(year = "Current"),
    
    cbind(sewage %>%
            filter(year == "Last Year") %>%
            mutate(mean.log10 = log10(mean)) %>%
            summarize(smoothed =  predict(loess(mean ~ index, span=0.035), na.rm = TRUE),
                      smoothed.log10 =  predict(loess(mean.log10 ~ index, span=0.035), na.rm = TRUE)),
          sewage %>% filter(year == "Last Year" & !is.na(mean)) %>% select(date)) %>%
      mutate(year = "Last Year")) %>%
  filter(smoothed >0) %>%
  mutate(year = fct_relevel(year,"Current", "Last Year" ))

wastewater.recent<-
  ggplot() +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_point(data= sewage %>% filter(year == "Current"), aes(x=date, y=mean), alpha=.5, size=1.5, color= "#8A9197FF") +
  geom_line(data= wastewater.smooth, aes(x=date, y=smoothed, color=year, group=year ), 
              alpha=.7, size=2.5) +
  labs(x="Date (current year)", y="SARS-CoV-2 Load (copies/mL)",
       title="Wastewater SARS-CoV-2 in Metro Boston",
       subtitle = paste0("LOESS average for current and prior year | Last available measurement: ", as.Date(last(sewage$date))),
       caption="Average of Southern and Northern sewer lines; Source: Massachusetts Water Resources Authority" )+
  scale_colour_npg() +
  #scale_color_jama( alpha = .8)+
  guides(color="none") +
  coord_cartesian(xlim=c(as.Date("2021-08-01"), Sys.Date()+90), ylim=c(0,2000))+
  scale_x_date(breaks = scales::pretty_breaks(12), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  geom_hline(yintercept = 0, linetype ="dotted") +
  geom_label_repel(data = wastewater.smooth %>% filter(date == Sys.Date()-2),
                   aes(x = date, y = smoothed, label = paste0(year), color= year),
                   hjust=0.5, min.segment.length =0 , nudge_x = 90, nudge_y = 300)

wastewater.recent



#log transformed
wastewater.recent.log<-
  ggplot() +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_point(data= sewage %>% filter(year == "Current"), aes(x=date, y=log10(mean)), alpha=.5, size=1.5, color= "#8A9197FF") +
  geom_line(data= wastewater.smooth, aes(x=date, y=smoothed.log10, color=year, group=year ), 
            alpha=.7, size=2.5) +
  labs(x="Date (current year)", y="Log10 SARS-CoV-2 Load (copies/mL)",
       title="Wastewater SARS-CoV-2 in Metro Boston",
       subtitle = paste0("LOESS average for current and prior year | Last measurement: ", as.Date(last(sewage$date))),
       caption="Average of Southern and Northern sewer lines; Source: Massachusetts Water Resources Authority" )+
  scale_colour_npg() +
  # scale_color_jama( alpha = .8)+
  guides(color="none") +
  coord_cartesian(xlim=c(Sys.Date()+90 -365, Sys.Date()+90), ylim=c(1,4.5))+
  scale_x_date(breaks = scales::pretty_breaks(10), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  geom_hline(yintercept = 0, linetype ="dotted") +
  geom_label_repel(data = wastewater.smooth %>% filter(date == Sys.Date()-5),
                   aes(x = date, y = smoothed.log10, label = paste0(year), color= year),
                   hjust=0.5, min.segment.length =0 , nudge_x = 90, nudge_y = .2)

wastewater.recent.log
ggsave("wastewater.recent.pdf")
ggsave("wastewater.recent.png")


wastewater.recent + wastewater.recent.log + plot_layout(ncol =2)
ggsave("wastewater.recent.pdf", width = 20, height = 10)
ggsave("wastewater.recent.png", width = 20, height = 10)

# 
# ggplot() +
#   theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#                            axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_point(data= sewage %>% filter(year == "Current"), aes(x=date, y=mean), alpha=.5, size=1.5, color= "#8A9197FF") +
#   geom_smooth(data= sewage, aes(x=date, y=mean, color=year, group=year ), alpha=.7, size=2.5, method="loess", span=0.05, se=FALSE) +
#   
#   #geom_point(data=sewage %>% filter(B117 == 1), aes(x=date, y=1300), shape= "|", color="#B24745ff", alpha=0.7, size=10) +
#   #geom_text(aes(x=as.Date("2021-03-01"), y=1300, label="B.1.1.7 Detected:"), hjust=1, size=5, color="#B24745ff")+
#   labs(x="Date (current year)", y="SARS-CoV-2 Load (copies/mL)",
#        title="Wastewater SARS-CoV-2 in Metro Boston",
#        subtitle = paste0("Last available measurement: ", as.Date(last(sewage$date))),
#        caption="Average of Southern and Northern sewer lines; Source: Massachusetts Water Resources Authority" )+
#   scale_color_viridis(discrete = TRUE, end=0.85, option = "C")+
#   guides(color="none") +
#   coord_cartesian(xlim=c(as.Date("2021-04-01"), Sys.Date()+90), ylim=c(0,1500))+
#   scale_x_date(breaks = scales::pretty_breaks(12), labels = scales::label_date_short()) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
#   geom_hline(yintercept = 0, linetype ="dotted")
# 


# recent incidence plot, compared with 2020

load("mdph_testingbydate.Rdata")


mdph_testingbydate.years <- 
  rbind(
    mdph_testingbydate %>%
      filter(date > as.Date("2020-03-01")) %>%
      mutate(year = "Current"),
  mdph_testingbydate %>%
  filter(date > as.Date("2020-03-01")) %>%
  mutate(date = date+365, 
         year = "Last Year")) %>%
  mutate(year = fct_relevel(year,  "Current" ,"Last Year"))



total.cases<- mdph_testingbydate %>%
  filter(date> as.Date("2021-06-19") & date < as.Date("2021-07-19")) %>%
  summarize(total.interval = sum(newpositive))
total.cases<- total.cases$total.interval

ma.recent<- 
  ggplot() +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_col(data = mdph_testingbydate.years %>% filter(year == "Current"), aes(x=date, y=incidence1day), color="grey", alpha =0.3)+
  geom_line(data = mdph_testingbydate.years, aes(x=date, y=rolling7day, group=year, color=year),linetype="solid", alpha=.7, size=2.5)+
  labs(x="Date of Test (Current Year)", y="Cases per 100,000",
       title="Incidence of reported COVID, Massachusetts",
       subtitle = paste0("7-Day running average for current and prior year | Data through ", max(mdph_testingbydate$date)),
       caption="Source: MDPH" )+
  guides(color="none") +
  scale_color_npg() +
  #scale_color_jama( alpha = .7)+
  coord_cartesian( xlim=c(Sys.Date()+90 -365, Sys.Date()+90), ylim=c(0, 140) ) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  geom_label_repel(data=mdph_testingbydate.years   %>% filter(date == max(mdph_testingbydate$date)), 
                   aes(label=paste0(year, "\n", round(rolling7day,1), " per 100,000"), x = date, y = rolling7day, color=year),
                   min.segment.length = 0, nudge_y = 30, nudge_x = 50)
ma.recent
ggsave("ma.recent.pdf", width = 10, height = 10)
ggsave("ma.recent.png", width = 10, height = 10)




## CDC wastewater

CDC.wastewater<- read_csv("cdcwastewater.csv") %>% 
  mutate(ptc_15d = if_else(detect_prop_15d == 0, 0, ptc_15d)) %>%
  filter(!is.na(ptc_15d)) %>%
  filter(population_served >10000) %>%
  mutate(increased = if_else(ptc_15d >0, 1, 0),
         date = date_end) %>%
  add_count(date_end, wt=increased) %>%
  rename(num_increased=n) %>%
  add_count(date_end) %>%
  mutate(sites_increasing = num_increased/n) %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, num_increased, n, sites_increasing) 

US.sewersheds <- ggplot(data=CDC.wastewater, aes(x=date, y=sites_increasing*100)) +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(color="#8A9197FF", size=2) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels = scales::label_date_short()) +
  scale_color_jama( alpha = .9)+
  coord_cartesian( xlim=c(as.Date("2021-10-01"), Sys.Date()+90), ylim=c(0, 100) ) +
  labs(x="Test", y="Surviellance Sites with\nIncreasing Concentration (%)",
       title="Nationwide SARS-CoV2 Wastewater",
       subtitle = paste0(">700 surviellance sites in US, 15-day trend | Data through ", max(CDC.wastewater$date)),
       caption="Source: CDC" )+
  geom_hline(yintercept = 50, linetype ="dotted") +
  geom_label_repel(data= CDC.wastewater %>%
                     filter(date == max(date, na.rm = TRUE)) %>% distinct(date, .keep_all = TRUE),
                   aes(x=date, y= sites_increasing*100, label=paste0(round(sites_increasing*100,0), "% of sewersheds\nincreasing") ), color="#00A1D5FF",
                   min.segment.length = 0, xlim = c(as.Date(max(CDC.wastewater$date, na.rm = TRUE))+5, as.Date(NA)))
US.sewersheds

# getting zips for CT, RI, VT, NH, ME, and MA
NE.state.codes <-c("09", "44","50","33","23","25")


CDC.wastewater.NE<- read_csv("cdcwastewater.csv") %>% 
  mutate(state = str_sub(county_fips, 1,2)) %>%
  filter(state %in% NE.state.codes) %>%
  mutate(ptc_15d = if_else(detect_prop_15d == 0, 0, ptc_15d)) %>%
  filter(!is.na(ptc_15d)) %>%
  mutate(increased = if_else(ptc_15d >0, 1, 0),
         date = date_end) %>%
  add_count(date_end, wt=increased) %>%
  rename(num_increased=n) %>%
  add_count(date_end) %>%
  mutate(sites_increasing = num_increased/n) %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, num_increased, n, sites_increasing) %>%
  filter(n>10)


NE.sewersheds <- ggplot(data=CDC.wastewater.NE, aes(x=date, y=sites_increasing*100)) +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(color="#8A9197FF", size=2) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels = scales::label_date_short()) +
  scale_color_jama( alpha = .9)+
  coord_cartesian( xlim=c(as.Date("2022-02-01"), Sys.Date()+30), ylim=c(0, 100) ) +
  labs(x="Date", y="Surviellance Sites with\nIncreasing Concentration (%)",
       title="US Northeast SARS-CoV2 Wastewater",
       subtitle = paste0(">20 surviellance sites in New England, 15-day trend | Data through ", max(CDC.wastewater$date)),
       caption="Source: CDC" )+
  geom_label_repel(data= CDC.wastewater.NE %>%
                     filter(date == max(date, na.rm = TRUE)) %>% distinct(date, .keep_all = TRUE),
                   aes(x=date, y= sites_increasing*100, label=paste0(round(sites_increasing*100,0), "% of sewersheds\nincreasing") ), color="#00A1D5FF",
                   min.segment.length = 0, xlim = c(as.Date(max(CDC.wastewater.NE$date, na.rm = TRUE))+5, as.Date(NA)))+
  geom_hline(yintercept = 50, linetype ="dotted") 
NE.sewersheds
