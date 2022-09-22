library(tidyverse)
library(ggsci)
library(ggrepel)

load("kids0to19.Rdata")
load("age0to100.Rdata")

jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")

kids<- kids0to19 %>%
  ggplot(aes(x=date, y=incidence1day))+
  geom_line(aes(color=agecat), size=2) +
  theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  coord_cartesian(ylim=c(0,440), xlim=c(as.Date("2021-05-01"), Sys.Date()+50))+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  scale_color_jama(palette = c("default"), name="Age")+
  labs(x="Date", y="Daily Cases, per 100,000",
       title="Incidence of Pediatric Covid-19",
       subtitle = paste0("Two-week average incidence in Massachusetts | Most recent data: ", 
                         max(kids0to19$date))) +
  guides(color="none")+
  geom_label_repel(data=kids0to19  %>%
                     filter(date == last(date)), aes(label= paste0(agecat, "\n ", round(incidence1day,1), " per 100,000"),
                                                     x = date ,  y = incidence1day, color=agecat),
                   hjust=0, min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA)))
kids

ggsave("children incidence.pdf")

all0to100<- age0to100 %>%
  ggplot(aes(x=date, y=incidence1day))+
  geom_line(aes(color=agecat), size=2) +
  theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
  coord_cartesian(ylim=c(0,150), xlim=c(as.Date("2022-02-01"), Sys.Date()+20))+
  scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
  scale_color_jama(palette = c("default"), name="Age")+
  labs(x="Date", y="Daily Cases, per 100,000",
       title="Incidence of Covid-19 by Age",
       subtitle = paste0("Two-week average incidence in Massachusetts | Most recent data: ", 
                         max(kids0to19$date))) +
  guides(color="none")+
  geom_label_repel(data=age0to100  %>%
                     filter(date == last(date)), aes(label= paste0(agecat, "\n ", round(incidence1day,1), " per 100,000"),
                                                     x = date ,  y = incidence1day, color=agecat),
                   hjust=0, min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA)))
all0to100