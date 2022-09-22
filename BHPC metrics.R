library(tidyverse)
library(ggsci)
library(ggrepel)
library(showtext)
library(patchwork)

showtext_auto()

bphc_metrics<- as.data.frame(read_csv("bphc_metrics.csv")) %>%
  mutate(ed.cases7d.avg = (ed.cases + lag(ed.cases, 1) + lag(ed.cases, 2) + lag(ed.cases, 3) + lag(ed.cases, 4) + lag(ed.cases, 5) + lag(ed.cases, 6))/7,
         bos.cases7d.avg = (bos.cases + lag(bos.cases, 1) + lag(bos.cases, 2) + lag(bos.cases, 3) + lag(bos.cases, 4) + lag(bos.cases, 5) + lag(bos.cases, 6))/7)

boston.edvisits<-
bphc_metrics %>%
ggplot() +
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_col(aes(x=date, y=ed.cases), fill="#6F99AD99", color="#6F99AD99")+
  #geom_line(data=lastyear, aes(x=date, y=rolling7day), color="#67a9cf", size=1, alpha=0.8, linetype="solid")+
  #annotate("text", x= as.Date("2021-05-26"), y=10, label="2020 Incidence",color="#67a9cf", hjust=0)+
  geom_line(aes(x=date, y=ed.cases7d.avg), color="#E18727FF", size=1.5)+
  labs(x="Date", y="Visits",
       title="Covid-19 Emergency Department Visits, City of Boston",
       subtitle = paste0("ED Visits for confirmed or suspected Covid-19 | Data current through ", max(bphc_metrics$date)),
       caption="Source: Boston Public Health Commission" )+
  guides(color="none") +
  coord_cartesian( xlim=c(as.Date("2021-01-01"), Sys.Date()+90), ylim=c(120, NA) ) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  geom_label_repel(data=bphc_metrics  %>% filter(date == last(date)), 
                   aes(label=paste0("Running (7-day)\ndaily average:\n\n",round(ed.cases7d.avg,0), " visits"),  fontface = 'bold', x = date, y = ed.cases7d.avg),
                   color="#E18727FF", min.segment.length = 0, nudge_y = 6, #nudge_x = 30,
                   xlim=c(Sys.Date(), Sys.Date()+10))
boston.edvisits
ggsave("Boston Covid ED Visits.pdf", width=12, height=8)

boston.cases<-
bphc_metrics %>%
  ggplot() +
  theme_classic() +  theme(plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_col(aes(x=date, y=bos.cases), fill="#6F99AD99", color="#6F99AD99")+
  #geom_line(data=lastyear, aes(x=date, y=rolling7day), color="#67a9cf", size=1, alpha=0.8, linetype="solid")+
  #annotate("text", x= as.Date("2021-05-26"), y=10, label="2020 Incidence",color="#67a9cf", hjust=0)+
  geom_line(aes(x=date, y=bos.cases7d.avg), color="#E18727FF", size=1.5)+
  labs(x="Date", y="Cases",
       title="Confirmed Covid-19, City of Boston",
       subtitle = paste0("Data current through ", max(bphc_metrics$date)-2),
       caption="Source: Boston Public Health Commission" )+
  guides(color="none") +
  coord_cartesian( xlim=c(as.Date("2021-01-01"), Sys.Date()+90), ylim=c(0, NA) ) +
  scale_x_date(breaks = scales::pretty_breaks(10), labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  geom_label_repel(data=bphc_metrics  %>% filter(date == last(date)-2), 
                   aes(label=paste0("Running (7-day)\ndaily average:\n\n",round(bos.cases7d.avg,0), " cases"),  fontface = 'bold', x = date, y = bos.cases7d.avg),
                   color="#E18727FF", min.segment.length = 0, nudge_y = 6, #nudge_x = 30,
                   xlim=c(Sys.Date(), Sys.Date()+10))
boston.cases
ggsave("Boston Covid19 cases.pdf", width=12, height=8)

bphc.metrics<- boston.edvisits + boston.cases + plot_layout(ncol=2)
bphc.metrics
ggsave("Boston Covid19 metrics.pdf", width=16, height=10)

# library("scales")
# show_col(pal_nejm("default")(10))
# show_col(pal_nejm("default", alpha = 0.6)(10))




