library(zoo)
library(tidyverse)
library(ggsci)
library(ggrepel)
setwd("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing")
load("mdph_testingbydate.Rdata")
load("CDCnowcast.Rdata")


CDCnowcast.long <- CDCnowcast %>%
  group_by(usa_or_hhsregion, variant) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>%
  mutate(share = na.approx(share, maxgap = 60, rule = 2)) %>%
  ungroup()

recent.nowcast.variants<- (CDCnowcast.long %>% filter(date == max(date, na.rm = TRUE)) %>% filter(usa_or_hhsregion == "1") %>% filter(variant != "Other") %>% filter(share >0.004) )$variant
jama.pal <- c(pal_jama(alpha = 0.8)(7), pal_jama(alpha = 0.8)(7), pal_jama(alpha = 0.8)(7), pal_jama(alpha = 0.8)(7))

variant.calc <- left_join(
  CDCnowcast.long %>%
    filter(usa_or_hhsregion == "1") %>%
    filter(variant %in% recent.nowcast.variants),
  mdph_testingbydate, 
  by = "date") %>%
  mutate(variant.incidence = share *rolling7day) %>% filter(date != as.Date("2021-04-04")) %>% # date of MDPH date cleaning
  filter(date <= max(mdph_testingbydate$date))

variant.calc.collapsed <- variant.calc %>% 
  filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
#  filter(variant %in% c("B.1.1.529", "BA.4", "BA.4.6","BA.5", "BA.2", "BA.2.12.1")) %>%
  # mutate(
  #   variant = fct_collapse(variant,
  #                          "Omicron\nB.1.1.529" = c("B.1.1.529"),
  #                          "Omicron\nB.2 and BA.2.12.1" = c("BA.2.12.1", "BA.2"),
  #                          "Omicron\nBA.4.6" = c("BA.4.6"),
  #                          "Omicron\nBA.4" = c("BA.4"),
  #                          "Omicron\nBA.5" = c("BA.5"),
  #                          "Other" = c("Other", "B.1.617.2")
  #   )  ) %>% 
  group_by(date, variant) %>%
  summarize(variant.incidence = sum(variant.incidence, na.rm = TRUE)) %>% ungroup()


NE.nowcast <- CDCnowcast.long %>%
  filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>%
 # filter(variant %in% recent.nowcast.variants) %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=share, group=variant, color=variant), size=2, alpha=1) +
  geom_linerange(aes(x=date, ymin=share_lo, ymax=share_hi, group=variant, color=variant), na.rm = TRUE, alpha=.5) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Estimated Proportion of Infections",
       title="SARS-CoV-2 variant proportions",
       subtitle = paste0("CDC Nowcast for New England | Through week ending on ", max(CDCnowcast.long$date)) )+
  coord_cartesian(xlim=c(as.Date("2022-02-01"), as.Date(max(variant.calc$date+90)) ), ylim=c(0,1) ) +
  scale_colour_manual(values = jama.pal) +
  geom_label_repel(data = CDCnowcast.long %>%
                     filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>%
                     filter(variant %in% recent.nowcast.variants) %>%
                     group_by(variant) %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = share, label = paste0(variant, "\n", round(share, 2)), color= variant),
                   hjust=0.5, max.overlaps = 100,
                   xlim=c(max(CDCnowcast.long$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())

NE.nowcast 
  




variant.incidence<-  variant.calc.collapsed %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=variant.incidence, group=variant, color=variant), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Confirmed Daily Covid-19, per 100,000",
       title="Covid-19 Incidence by Variant, Massachusetts",
       subtitle = paste0("Estimated from MA incidence and CDC Variant Nowcast for New England")) +
  coord_cartesian(xlim=c(as.Date("2022-02-01"), as.Date(max(variant.calc$date+90)) ), ylim=c(0,45) ) +
  scale_colour_manual(values = jama.pal) +
  geom_label_repel(data = variant.calc.collapsed  %>% 
                     group_by(variant) %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = variant.incidence, label = paste0(variant, "\n", round(variant.incidence, 2), "/100,000"), color= variant),
                   hjust=0.5, max.overlaps = 500,
                   xlim=c(max(variant.calc$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())
variant.incidence


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

MAviruses %>% filter(date>= as.Date("2022-03-16") & date<= as.Date("2022-05-31")) %>%
  count() %>%
  mutate(pct = n/sum(n))

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
