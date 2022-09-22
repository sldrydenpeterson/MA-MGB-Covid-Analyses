library(tidyverse)
library(readxl)
library(lubridate)
library(ggsci)
library(ggrepel)
library(patchwork)
library(zoo)
setwd("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing")


data_path <- "~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/GISAID MA files" 
files <-  dir(data_path , pattern = "*.tsv")

# gathering ~twice weekly files downloaded from GISAIDS
MAviruses <- data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
       ~ read_tsv(file.path(data_path, .), col_select = c("gisaid_epi_isl", "date", "pangolin_lineage")))) %>% # a new data column
  unnest(cols = c(file_contents)) %>%
  distinct(gisaid_epi_isl, .keep_all = TRUE) %>%
mutate(week = as.Date(cut(date, "week")),
       VOC_VOI = case_when(
         str_detect(pangolin_lineage, "AY") ~ "Delta",
         str_detect(pangolin_lineage, "B.1.617") ~ "Delta",
         str_detect(pangolin_lineage, "B.1.417") ~ "Epsilon",
         str_detect(pangolin_lineage, "P.1") ~ "Gamma",
         str_detect(pangolin_lineage, "P.2") ~ "Zeta",
         str_detect(pangolin_lineage, "P.3") ~ "Theta",
         str_detect(pangolin_lineage, "B.1.526") ~ "Iota", 
         str_detect(pangolin_lineage, "B.1.621") ~ "Iota", 
         str_detect(pangolin_lineage, "B.1.1.529") ~ "Omicron (BA.1)",
         str_detect(pangolin_lineage, "BA.1.1") ~ "Omicron (BA.1)",
         str_detect(pangolin_lineage, "BA.1") ~ "Omicron (BA.1)",
       str_detect(pangolin_lineage, "BA.3") ~ "Omicron (BA.4/5)",
       str_detect(pangolin_lineage, "BA.4")~ "Omicron (BA.4/5)",
       str_detect(pangolin_lineage, "BA.5")~ "Omicron (BA.4/5)",
        str_detect(pangolin_lineage, "BA.2.12.1")~ "Omicron (BA.2)",
       str_detect(pangolin_lineage, "BA.2")~ "Omicron (BA.2)",
         TRUE ~ "Other"
       ))


library(treemapify)
mycolors <- c(pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9))

recent.viruses <- MAviruses %>%
  filter(date > max(MAviruses$date) - 8) %>%
  count(pangolin_lineage) %>%
  mutate(pangolin_lineage = fct_reorder(pangolin_lineage, n, .desc = TRUE)) %>%
  ggplot() +
  geom_treemap(aes(area = n , fill = pangolin_lineage)) +
  geom_treemap_text(aes(label = pangolin_lineage, area =n), color = "white") +
  scale_fill_manual(values = mycolors) +
  guides(fill = "none") +
  labs(title="Variant Proportions, Massachusetts",
       subtitle = paste0("Sequences from ", as.Date(max(MAviruses$date) - 7), " to ", as.Date(max(MAviruses$date))),
       caption = "Source: GISAID")
recent.viruses
ggsave("recent.viruses.pdf", width = 10, height =8)


MAviruses %>% group_by(week) %>% tally() %>% arrange(desc(week))
weekly.variants<-MAviruses %>% group_by(week, VOC_VOI) %>% tally() %>% arrange(desc(week))

#last week with more than 10 sequences
date10 <- as.Date(max((MAviruses %>% group_by(week) %>% tally() %>% filter(n >= 30))$week))



load("mdph_testingbydate.Rdata")

# variant incidence

variant.calc <-left_join(
 rbind(
   MAviruses %>%   #carrying forward the last week observation of proportions by variant
    filter(week <= as.Date(date10)) %>% # include weeks up to last one with >50 sequences
      # mutate(VOC_VOI = fct_collapse(VOC_VOI, "Alpha" = "Alpha",
      #                               "Other" = "Gamma",
      #                               "Delta" = "Delta",
      #                               "Delta" = "Kappa",
      #                               "Other" = "Zeta",
      #                               "Other" = "Other",
      #                               "Other" = "Mu",
      #                               "Other" = "Beta",
      #                               "Other" = "Beta",
      #                               "Other" = "Epsilon",
      #                               "Other" = "Eta",
      #                               "Omicron BA.1.X" = "Omicron",
      #                               "Omicron BA.2" = "BA.2",
      #                               "Omicron BA.2.12.X" = "BA.2.12.X",
      #                               #"Other" = "Theta"
      #                               "Other" = "Iota" ) )%>%
    group_by(week, VOC_VOI) %>%
    summarise(n =n()) %>% ungroup(),
    
 MAviruses %>%
    filter(week <= as.Date(date10)) %>% # include weeks up to last one with >50 sequences
    # mutate(VOC_VOI = fct_collapse(VOC_VOI, "Alpha" = "Alpha",
    #                               "Other" = "Gamma",
    #                               "Delta" = "Delta",
    #                               "Delta" = "Kappa",
    #                               "Other" = "Zeta",
    #                               "Other" = "Other",
    #                               "Other" = "Mu",
    #                               "Other" = "Beta",
    #                               "Other" = "Beta",
    #                               "Other" = "Epsilon",
    #                               "Other" = "Eta",
    #                               "Omicron BA.1.X" = "Omicron",
    #                               "Omicron BA.2" = "BA.2",
    #                               "Omicron BA.2.12.X" = "BA.2.12.X",
    #                               #"Other" = "Theta"
    #                               "Other" = "Iota" ) )%>%
    group_by(week, VOC_VOI) %>%
    summarise(n =n()) %>%
    group_by(VOC_VOI) %>%
    arrange(week) %>%
    filter(row_number()==n()) %>% mutate(week = week+7) %>%
    ungroup()) %>%
    group_by(week) %>%
    mutate(prop= n/sum(n),
           total= sum(n)
          # se = (sqrt(prop * (1-prop)/sum(n))),
           #lower = prop - (se*1.96),
           #upper = prop + (se*1.96),
           #lower = if_else(lower < 0, 0, lower),
           #upper = if_else(upper < 0, 0, upper)
          ) %>%
    rowwise()%>%
    mutate(
      broom::tidy(prop.test(n, total, conf.level=0.80, correct = FALSE))[1, 5], 
      broom::tidy(prop.test(n, total, conf.level=0.80, correct = FALSE))[1, 6]) %>%
    ungroup() %>%
    mutate(date = week) %>%
    group_by(VOC_VOI) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    mutate(prop = na.approx(prop, maxgap = 60, rule = 2)),
  mdph_testingbydate,
  by="date") %>%
  mutate(variant.incidence = prop *rolling7day) %>% filter(date != as.Date("2021-04-04")) %>% # date of MDPH date cleaning
  filter(date < max(date)-6)

variant.calc %>% filter(date >as.Date("2021-10-01")) %>%
  count(VOC_VOI)

variant.incidence<- variant.calc %>% filter(date > as.Date("2021-10-01")) %>%
ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Week",
       y="Confirmed Daily Covid-19, per 100,000",
       title="Covid-19 Incidence by Variant, Massachusetts",
       subtitle = paste0("Estimated from incidence and weekly measured variant prevalence", " | Sequences through ", as.Date(max(MAviruses$date))),
       caption="Calculated from MDPH reported incidence and sequences posted to GISAID") +
  coord_cartesian(xlim=c(as.Date("2021-10-01"), as.Date(max(variant.calc$date+65)) ), ylim=c(0,350) ) +
  scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = variant.calc %>% 
                     filter(date > as.Date("2021-10-01")) %>%
                     group_by(VOC_VOI) %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
                       hjust=0.5, 
                   xlim=c(max(variant.calc$date)+5, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())
variant.incidence
ggsave("~/Dropbox (Partners HealthCare)/R files/covid/variant incidence.pdf", width=12, height=12)
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/variant incidence.jpg", plot = last_plot(), device = "jpeg",
#        scale = 1,  units = c("in"),height = 8.67, width = 8.67,
#        dpi = 160, limitsize = TRUE)

#zoom on minor variants


variant.incidence.zoom <-
  variant.calc %>% filter(date > as.Date("2021-10-01")) %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Week",
       y="Confirmed Daily Covid-19, per 100,000",
       title="Covid-19 Incidence by Variant, Massachusetts",
       subtitle = paste0("Estimated from incidence and weekly measured variant prevalence", " | Sequences through ", as.Date(max(MAviruses$date))),
       caption="Calculated from MDPH reported incidence and sequences posted to GISAID") +
  coord_cartesian(xlim=c(as.Date("2022-01-01"), as.Date(max(variant.calc$date+80)) ), ylim=c(0,70) ) +
  scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = variant.calc %>% 
                     filter(date > as.Date("2021-10-01")) %>%
                     group_by(VOC_VOI) %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
                   hjust=0.5, 
                   xlim=c(max(variant.calc$date)+5, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())
variant.incidence.zoom


variant.calc.updated<-MAviruses %>%
  filter(week <= as.Date(date10)) %>% 

  count(week, VOC_VOI,.drop = FALSE) %>%
  group_by(week) %>%
  mutate(prop= n/sum(n),
         total= sum(n)
  ) %>%
  ungroup() %>%
  rowwise()%>%
  mutate(
    broom::tidy(prop.test(n, total, conf.level=0.90, correct = FALSE))[1, 5], 
    broom::tidy(prop.test(n, total, conf.level=0.90, correct = FALSE))[1, 6]) %>%
  ungroup() %>%
  mutate(date=week)

v2<-variant.calc.updated %>% filter(date <= max(date)) %>%
  filter(date > as.Date("2021-10-01")) %>%

  ggplot() +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_line(aes(x=week, y=prop, group=VOC_VOI, color=VOC_VOI), size=2) +
  geom_linerange(aes(x=week, ymax=conf.high, ymin=conf.low, color=VOC_VOI)) +
  theme_classic() + theme( plot.title = element_text(size = rel(1.5)),
               axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Week",
       y="Proportion of Sequences",
       title="SARS CoV-2 Variants in Massachusetts by Week",
       subtitle = paste0("Sequences through week starting on ", as.Date(max(variant.calc.updated$date))),
       caption="Calculated (number of variants detected / total number sequenced)\nfrom posted sequences on GISAID. Binomial confidence limits.")+
  coord_cartesian(xlim=c(as.Date("2022-01-01"), as.Date(max(variant.calc$date+80)) ), ylim = c(0, 1)) +
    scale_colour_jama(palette = c("default"), alpha = 1) +
  geom_label_repel(data = variant.calc.updated %>% filter(date <= max(date)) %>%
                     filter(date > as.Date("2021-10-01")) %>% group_by(VOC_VOI) %>% 
                     filter(date == max(date)), 
                   aes(x = date, y = prop, label = paste0(VOC_VOI, "\n", round(prop*100, 1), "%\nn= ", n), 
                       color = VOC_VOI), hjust=0.5, max.overlaps = 40,
                   xlim=c(max(variant.calc$date)+5, as.Date(NA)))+
  guides(color="none") +
  scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short()) 
v2

v2+ variant.incidence.zoom + plot_layout(ncol=2)
ggsave("MA variant proportion and incidence 2.pdf", width =16, height = 9)

# ## variant emergent plots
# 
# alpha <- variant.calc %>%
#   #filter(date>as.Date("2021-01-04") & date < as.Date("2021-01-04")+100) %>%
#   mutate(VOC_VOI = fct_relevel(as.factor(VOC_VOI), "Omicron BA.1.X", "Omicron BA.2", "Delta", "Alpha", "Other")) %>% 
#   filter(VOC_VOI %in% c("Omicron BA.1.X", "Omicron BA.2",  "Delta","Alpha",  "Other")) %>%
#   ggplot() +
#   geom_hline(yintercept=0, linetype="dotted")+
#   geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
#   theme_classic() + theme(plot.title = element_text(size = rel(1.5)), aspect.ratio = 1.5,
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   labs(x="Week",
#        y="Confirmed Daily Covid-19, per 100,000",
#        title="Alpha Emergence",
#        caption="Calculated from MDPH reported incidence\nand sequences posted to GISAID") +
#   coord_cartesian(ylim=c(0,30) , xlim = c(as.Date("2021-01-04"), as.Date("2021-01-04")+100 )) +
#   scale_colour_jama(palette = c("default"), alpha = 1) +
#   geom_label_repel(data = variant.calc %>% 
#                      filter(date>as.Date("2021-01-04") & date < as.Date("2021-01-04")+100) %>%
#                      group_by(VOC_VOI) %>% filter(date == max(date)-50) %>%
#                      filter(VOC_VOI %in% c("Alpha")) ,
#                    aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
#                    hjust=0.5, nudge_y = 2, min.segment.length = 0)+
#   guides(color="none") +
#   scale_y_continuous(breaks = scales::pretty_breaks(5)) +
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
# alpha
# 
# delta <- variant.calc %>%
#   mutate(VOC_VOI = fct_relevel(as.factor(VOC_VOI), "Omicron BA.1.X", "Omicron BA.2", "Delta", "Alpha", "Other")) %>% 
#   filter(VOC_VOI %in% c("Omicron BA.1.X", "Omicron BA.2",  "Delta","Alpha",  "Other")) %>%
#   ggplot() +
#   geom_hline(yintercept=0, linetype="dotted")+
#   geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
#   theme_classic() + theme(plot.title = element_text(size = rel(1.5)), aspect.ratio = 1.5,
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   labs(x="Week",
#        y="",
#        title="Delta Emergence",
#        caption="Calculated from MDPH reported incidence\nand sequences posted to GISAID") +
#   coord_cartesian(ylim=c(0,30) , xlim = c(as.Date("2021-06-15"), as.Date("2021-06-15")+100 )) +
#   scale_colour_jama(palette = c("default"), alpha = 1) +
#   geom_label_repel(data = variant.calc %>% 
#                      filter(date>as.Date("2021-06-15") & date < as.Date("2021-06-15")+100) %>%
#                      group_by(VOC_VOI) %>% filter(date == max(date)-50) %>%
#                      filter(VOC_VOI %in% c("Delta")) ,
#                    aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
#                    hjust=0.5, nudge_y = 2, min.segment.length = 0)+
#   guides(color="none") +
#   scale_y_continuous(breaks = scales::pretty_breaks(5)) +
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
# delta
# 
# omicron <- variant.calc %>%
#   mutate(VOC_VOI = fct_relevel(as.factor(VOC_VOI), "Omicron BA.1.X", "Omicron BA.2", "Delta", "Alpha", "Other")) %>% 
#   filter(VOC_VOI %in% c("Omicron BA.1.X", "Omicron BA.2",  "Delta","Alpha",  "Other")) %>%
#   ggplot() +
#   geom_hline(yintercept=0, linetype="dotted")+
#   geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
#   theme_classic() + theme(plot.title = element_text(size = rel(1.5)), aspect.ratio = 1.5,
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   labs(x="Week",
#        y="",
#        title="Omicron Emergence",
#        caption="Calculated from MDPH reported incidence\nand sequences posted to GISAID") +
#   coord_cartesian(ylim=c(0,30) , xlim = c(as.Date("2021-12-01"), as.Date("2021-12-01")+100 )) +
#   scale_colour_jama(palette = c("default"), alpha = 1) +
#   geom_label_repel(data = variant.calc %>% 
#                      filter(date>as.Date("2021-12-01") & date < as.Date("2021-12-01")+100) %>%
#                      group_by(VOC_VOI) %>% filter(date == max(date)-50) %>%
#                      filter(VOC_VOI %in% c("Omicron BA.1.X")) ,
#                    aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
#                    hjust=0.5, nudge_y = 2, min.segment.length = 0)+
#   guides(color="none") +
#   scale_y_continuous(breaks = scales::pretty_breaks(5)) +
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
# omicron
# 
# BA.2 <- variant.calc %>%
#   mutate(VOC_VOI = fct_relevel(as.factor(VOC_VOI), "Omicron BA.1.X", "Omicron BA.2", "Delta", "Alpha", "Other")) %>% 
#   filter(VOC_VOI %in% c("Omicron BA.1.X", "Omicron BA.2",  "Delta","Alpha",  "Other")) %>%
#   ggplot() +
#   geom_hline(yintercept=0, linetype="dotted")+
#   geom_line(aes(x=date, y=variant.incidence, group=VOC_VOI, color=VOC_VOI), size=2, alpha=1) +
#   theme_classic() + theme(plot.title = element_text(size = rel(1.5)), aspect.ratio = 1.5,
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   labs(x="Week",
#        y="",
#        title="BA.2 Emergence",
#        caption="Calculated from MDPH reported incidence\nand sequences posted to GISAID") +
#   coord_cartesian(ylim=c(0,30) , xlim = c(as.Date("2022-01-06"), as.Date("2022-01-06")+100 )) +
#   scale_colour_jama(palette = c("default"), alpha = 1) +
#   geom_label_repel(data = variant.calc %>% 
#                      filter(date>as.Date("2022-01-06") & date < as.Date("2022-01-06")+100) %>%
#                      group_by(VOC_VOI) %>% filter(date == min(date)+50) %>%
#                      filter(VOC_VOI %in% c("Omicron BA.2")) ,
#                    aes(x = date, y = variant.incidence, label = paste0(VOC_VOI, "\n", round(variant.incidence, 1), "/100,000"), color= VOC_VOI),
#                    hjust=0.5, nudge_y = 5, nudge_x = -15, min.segment.length = 0)+
#   guides(color="none") +
#   scale_y_continuous(breaks = scales::pretty_breaks(5)) +
#   scale_x_date(breaks = scales::pretty_breaks(10), labels=scales::label_date_short())
# BA.2
# 
# emergent.variant <- alpha + delta + omicron + BA.2 + plot_layout(ncol=4)
# emergent.variant 
# ggsave("MA emergent variant incidence.pdf", width =16, height = 9)