library(zoo)
library(tidyverse)
library(ggsci)
library(ggrepel)
library(patchwork)
setwd("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses")
load("mdph_testingbydate.Rdata")
load("CDCnowcast.Rdata")
load("MGBcovidcases.Rdata")



CDCnowcast.long <- CDCnowcast %>%
  group_by(usa_or_hhsregion, variant) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>%
  mutate(share = na.approx(share, maxgap = 60, rule = 2)) %>%
  ungroup()


susceptibility<- tribble(
  ~variant, ~evusheld.status, ~bebtelov.status,

  "B.1.617.2", "Susceptible", "Susceptible",
  "B.1.1.529", "Susceptible", "Susceptible",
  "B.1.1", "Susceptible", "Susceptible",
  "BA.2", "Susceptible", "Susceptible",
  "BA.2.12.1", "Susceptible", "Susceptible",
  "BA.4", "Susceptible", "Susceptible",
  "BA.5", "Susceptible", "Susceptible",
  "BA.2.75", "Susceptible", "Susceptible",
  "Other", "Susceptible", "Susceptible",
  
  "BA.4.6", "Resistant", "Susceptible",
  "BF.7", "Resistant", "Susceptible",
  "BF.11", "Resistant", "Susceptible",
  "BN.1", "Resistant", "Susceptible",
  "BA.2.75.2", "Resistant", "Susceptible",
  "BA.5.2.6", "Resistant", "Susceptible",
  
  "BA.5.6.2", "Resistant", "Resistant",
  "BA.4.6.3", "Resistant", "Resistant",
  "XBB", "Resistant", "Resistant",
  "BQ.1.1", "Resistant", "Resistant",
  "BQ.1", "Resistant", "Resistant"
)

recent.nowcast.variants<- left_join(CDCnowcast.long %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  filter(usa_or_hhsregion == "1")  %>% filter(share >0.005) %>%
  rename(current.share = share)  %>%
  select(variant, current.share), 
  susceptibility, by = "variant") %>%
  mutate(evusheld.status = replace_na(evusheld.status ,"Susceptible"),
         bebtelov.status = replace_na(bebtelov.status,"Susceptible"))




jama.pal <- c(pal_nejm()(8), pal_simpsons(alpha = .6)(16), pal_nejm(alpha = 0.6)(8), pal_jama(alpha = 0.6)(7))


MGB.incidence<- MGBcovidcases %>% 
  group_by(dxdate)%>%
  count()%>%
  ungroup() %>%
  rename(cases = n,
         date = dxdate) %>%
  mutate(incidence1day = cases*100000/1200000,
         incidence7day = zoo::rollsumr(cases, k=7, fill=NA)*100000/1200000,
         rolling7day = incidence7day/7)

maxMGBcase.date = as.Date(max(MGB.incidence$date))

# mutate(date=as.Date(date),
#        newpositive=molecular_positive_new + antigen_positive_new) %>%
#   select(date, newpositive) %>%
#   filter(date < max(date)-1) %>% #remove last day given delayed reported and resultant downward bias
#   mutate(incidence1day = newpositive*100000/7029917,
#          incidence7day = zoo::rollsumr(newpositive, k=7, fill=NA)*100000/7029917,
#          rolling7day = incidence7day/7)

variant.calc <- left_join(
  CDCnowcast.long %>%
    filter(usa_or_hhsregion == "1"),
  MGB.incidence, 
  by = "date") %>%
  mutate(MGB.incidence = share *rolling7day) %>%
  filter(!is.na(date) & date <= maxMGBcase.date)

variant.calc.collapsed <- left_join(variant.calc %>% 
  filter( usa_or_hhsregion == 1) %>% 
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
  summarize(variant.incidence = sum(MGB.incidence, na.rm = TRUE)) %>% ungroup(), 
recent.nowcast.variants, by = "variant")


NE.nowcast <- left_join(CDCnowcast.long, recent.nowcast.variants, by = "variant")%>%
  filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
  mutate(variant = fct_reorder(variant, current.share)) %>%
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
       subtitle = paste0("CDC Nowcast for New England | Through ", max(CDCnowcast.long$date)) )+
  coord_cartesian(xlim=c(as.Date("2022-05-01"), as.Date(max(variant.calc$date+120)) ), ylim=c(0,1) ) +
  scale_colour_manual(values = jama.pal) +
  geom_label_repel(data = CDCnowcast.long %>%
                     filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>%
                     filter(variant %in% recent.nowcast.variants$variant) %>%
                     group_by(variant) %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = share, label = paste0(variant, "\n", round(share, 2)), color= variant),
                   hjust=0.5, max.overlaps = 100,
                   xlim=c(max(CDCnowcast.long$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())

NE.nowcast 
  

NE.nowcast.evu.susceptibility  <- left_join(CDCnowcast.long, recent.nowcast.variants, by = "variant")%>%
  filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
  mutate(variant = fct_reorder(variant, current.share),
         evusheld.status = replace_na(evusheld.status ,"Susceptible"),
         bebtelov.status = replace_na(bebtelov.status,"Susceptible")) %>%
  group_by(date, evusheld.status) %>%
  summarise(share = sum(share, na.rm = TRUE)) %>%
  # filter(variant %in% recent.nowcast.variants) %>%
  ggplot() +
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=share, group=evusheld.status, color=evusheld.status), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Estimated Proportion of Infections",
       title="SARS-CoV-2 Susceptibility to Evusheld",
       subtitle = paste0("CDC Nowcast for New England | Through ", max(CDCnowcast.long$date)), 
       caption = "Following lineages considered resistant: BA.4.6, BF.7, BF.11, BQ.1, BQ.1.1, BA.2.75.2, BA.5.2.6, BN.1, XBB, BA.4.6.3, BA.5.6.2\nKnowledge of susceptiblity of Omicron subvariants limited and change in classifications expected.")+
  coord_cartesian(xlim=c(as.Date("2022-04-01"), as.Date(max(variant.calc$date+90)) ), ylim=c(0,1) ) +
  scale_colour_manual(values = jama.pal) +
  geom_label_repel(data = left_join(CDCnowcast.long, recent.nowcast.variants, by = "variant")%>%
                     filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
                     mutate(variant = fct_reorder(variant, current.share),
                            evusheld.status = replace_na(evusheld.status ,"Susceptible"),
                            bebtelov.status = replace_na(bebtelov.status,"Susceptible")) %>%
                     group_by(date, evusheld.status) %>%
                     summarise(share = sum(share, na.rm = TRUE)) %>%
                     ungroup() %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = share, label = paste0(evusheld.status, "\n", round(share, 2)), color= evusheld.status),
                   hjust=0.5, max.overlaps = 100, min.segment.length = 0,
                   xlim=c(max(CDCnowcast.long$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())

NE.nowcast.evu.susceptibility 
ggsave("NE.nowcast.evu.susceptibility.pdf", width=12, height = 8)


NE.nowcast.beb.susceptibility  <- left_join(CDCnowcast.long, recent.nowcast.variants, by = "variant")%>%
  filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
  mutate(variant = fct_reorder(variant, current.share),
         evusheld.status = replace_na(evusheld.status ,"Susceptible"),
         bebtelov.status = replace_na(bebtelov.status,"Susceptible")) %>%
  group_by(date, bebtelov.status) %>%
  summarise(share = sum(share, na.rm = TRUE)) %>%
  # filter(variant %in% recent.nowcast.variants) %>%
  ggplot() +
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=share, group=bebtelov.status, color=bebtelov.status), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Estimated Proportion of Infections",
       title="SARS-CoV-2 Susceptibility to Bebtelovimab",
       subtitle = paste0("CDC Nowcast for New England | Through ", max(CDCnowcast.long$date)) ,
       caption = "Following lineages considered resistant: BQ.1, BQ.1.1, XBB, BA.4.6.3, BA.5.6.2\nKnowledge of susceptiblity of Omicron subvariants limited and change in classifications expected.")+
  coord_cartesian(xlim=c(as.Date("2022-04-01"), as.Date(max(variant.calc$date+90)) ), ylim=c(0,1) ) +
  scale_colour_manual(values = jama.pal) +
  geom_label_repel(data = left_join(CDCnowcast.long, recent.nowcast.variants, by = "variant")%>%
                     filter(date > as.Date("2021-10-01") & usa_or_hhsregion == 1) %>% 
                     mutate(variant = fct_reorder(variant, current.share),
                            evusheld.status = replace_na(evusheld.status ,"Susceptible"),
                            bebtelov.status = replace_na(bebtelov.status,"Susceptible")) %>%
                     group_by(date, bebtelov.status) %>%
                     summarise(share = sum(share, na.rm = TRUE)) %>%
                     ungroup() %>%
                     filter(date == max(date, na.rm = TRUE)) ,
                   aes(x = date, y = share, label = paste0(bebtelov.status, "\n", round(share, 2)), color= bebtelov.status),
                   hjust=0.5, max.overlaps = 100, min.segment.length = 0,
                   xlim=c(max(CDCnowcast.long$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())

NE.nowcast.beb.susceptibility 
ggsave("NE.nowcast.beb.susceptibility.pdf", width=12, height = 8)


mAb.susceptibility <- NE.nowcast.beb.susceptibility + NE.nowcast.evu.susceptibility + plot_layout(ncol= 2)
mAb.susceptibility
ggsave("NE.nowcast.beb.evu.susceptibility.pdf", width=16, height = 8)

variant.incidence.evu<-  variant.calc.collapsed %>%
  mutate(evusheld.status = replace_na(evusheld.status ,"Susceptible")) %>%
  group_by(date, evusheld.status) %>%
  summarize(variant.incidence = sum(variant.incidence, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_hline(yintercept=0, linetype="dotted")+
  geom_line(aes(x=date, y=variant.incidence, group=evusheld.status, color=evusheld.status), size=2, alpha=1) +
  theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                          axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  labs(x="Date",
       y="Confirmed Daily Covid-19, per 100,000",
       title="Covid-19 Incidence by Variant Groups, Massachusetts",
       subtitle = paste0("MGB incidence and CDC Nowcast | ", "Incidence through ", maxMGBcase.date),
       caption = "Note: MGB patient population estimated at 1.2 million") +
  coord_cartesian(xlim=c(as.Date("2022-05-01"), as.Date(max(variant.calc$date+90)) ), ylim=c(0,45) ) +
  scale_colour_jama() +
  geom_label_repel(data = variant.calc.collapsed %>%
                     mutate(evusheld.status = replace_na(evusheld.status ,"Susceptible")) %>%
                     group_by(date, evusheld.status) %>%
                     summarize(variant.incidence = sum(variant.incidence, na.rm = TRUE)) %>%
                     ungroup() %>%
                     filter(date == max(date, na.rm = TRUE)) %>%
                     mutate( label = case_when(
                       evusheld.status == "Susceptible" ~ "BA.2, BA.4, BA.5",
                       evusheld.status == "Resistant" ~ "Novel immuno-evasive\n(BQ.1, BQ.1.1, XBB,\nBA.4.6, others) ")),
                   aes(x = date, y = variant.incidence, label = paste0(label, "\n", round(variant.incidence, 0), "/100,000"), color= evusheld.status),
                   hjust=0.5, max.overlaps = 500, min.segment.length = 0,
                   xlim=c(max(variant.calc.collapsed$date)+2, as.Date(NA)))+
  guides(color="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  scale_x_date(breaks = scales::pretty_breaks(15), labels=scales::label_date_short())
variant.incidence.evu


ggsave("MA incidence by variant.pdf", width=8, height = 8)
# 
# long.nowcast.variants<- 
#   left_join(
#       left_join(CDCnowcast.long %>% 
#                                           filter(usa_or_hhsregion == "1") %>% filter(variant != "Other") %>% filter(share >0.005) %>%
#                                           select(date, variant, share), 
#                                         susceptibility, by = "variant") %>%
#       mutate(evusheld.status = replace_na(evusheld.status ,"Susceptible"),
#              bebtelov.status = replace_na(bebtelov.status,"Susceptible")) %>%
#       group_by(date, evusheld.status) %>%
#       summarize(share = sum(share, na.rm = TRUE)),
# 
# 
# 
# variant.calc.collapsed <- left_join(variant.calc %>% 
#                                       filter( usa_or_hhsregion == 1) %>% 
#                                       #  filter(variant %in% c("B.1.1.529", "BA.4", "BA.4.6","BA.5", "BA.2", "BA.2.12.1")) %>%
#                                       # mutate(
#                                       #   variant = fct_collapse(variant,
#                                       #                          "Omicron\nB.1.1.529" = c("B.1.1.529"),
#                                       #                          "Omicron\nB.2 and BA.2.12.1" = c("BA.2.12.1", "BA.2"),
#                                       #                          "Omicron\nBA.4.6" = c("BA.4.6"),
#                                       #                          "Omicron\nBA.4" = c("BA.4"),
#                                       #                          "Omicron\nBA.5" = c("BA.5"),
#                                       #                          "Other" = c("Other", "B.1.617.2")
#                                       #   )  ) %>% 
#                                       group_by(date, variant) %>%
#                                       summarize(variant.incidence = sum(variant.incidence, na.rm = TRUE)) %>% ungroup(), 
#                                     recent.nowcast.variants, by = "variant")
#   
# 
# library(patchwork)
# NE.nowcast + variant.incidence + plot_layout(ncol =2)
# 
# 
# 
# 
# ggsave("NE.nowcast.pdf", width =16, height = 8)
# 
# 
# # 
# # data_path <- "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/GISAID MA files" 
# # files <-  dir(data_path , pattern = "*.tsv")
# # 
# # # gathering ~twice weekly files downloaded from GISAIDS
# # MAviruses <- data_frame(filename = files) %>% # create a data frame
# #   # holding the file names
# #   mutate(file_contents = map(filename,          # read files into
# #                              ~ read_tsv(file.path(data_path, .), col_select = c("gisaid_epi_isl", "date", "pangolin_lineage")))) %>% # a new data column
# #   unnest(cols = c(file_contents)) %>%
# #   distinct(gisaid_epi_isl, .keep_all = TRUE) %>%
# #   mutate(week = as.Date(cut(date, "week")),
# #          VOC_VOI = case_when(
# #            str_detect(pangolin_lineage, "AY") ~ "Delta",
# #            str_detect(pangolin_lineage, "B.1.617") ~ "Delta",
# #            str_detect(pangolin_lineage, "B.1.417") ~ "Epsilon",
# #            str_detect(pangolin_lineage, "P.1") ~ "Gamma",
# #            str_detect(pangolin_lineage, "P.2") ~ "Zeta",
# #            str_detect(pangolin_lineage, "P.3") ~ "Theta",
# #            str_detect(pangolin_lineage, "B.1.526") ~ "Iota", 
# #            str_detect(pangolin_lineage, "B.1.621") ~ "Iota", 
# #            str_detect(pangolin_lineage, "B.1.1.529") ~ "Omicron (BA.1)",
# #            str_detect(pangolin_lineage, "BA.1.1") ~ "Omicron (BA.1)",
# #            str_detect(pangolin_lineage, "BA.1") ~ "Omicron (BA.1)",
# #            str_detect(pangolin_lineage, "BA.3") ~ "Omicron (BA.4/5)",
# #            str_detect(pangolin_lineage, "BA.4")~ "Omicron (BA.4/5)",
# #            str_detect(pangolin_lineage, "BA.5")~ "Omicron (BA.4/5)",
# #            str_detect(pangolin_lineage, "BA.2.12.1")~ "Omicron (BA.2)",
# #            str_detect(pangolin_lineage, "BA.2")~ "Omicron (BA.2)",
# #            TRUE ~ "Other"
# #          ))
# # 
# # MAviruses %>% filter(date>= as.Date("2022-03-16") & date<= as.Date("2022-05-31")) %>%
# #   count() %>%
# #   mutate(pct = n/sum(n))
# # 
# # library(treemapify)
# # mycolors <- c(pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9), pal_npg(alpha = 0.7)(9))
# # 
# # recent.viruses <- MAviruses %>%
# #   filter(date > max(MAviruses$date) - 11) %>%
# #   count(pangolin_lineage) %>%
# #   mutate(pangolin_lineage = fct_reorder(pangolin_lineage, n, .desc = TRUE)) %>% arrange(desc(n)) %>% filter( !is.na(pangolin_lineage)) %>% slice_head(n = 30) %>%
# #   ggplot() +
# #   geom_treemap(aes(area = n , fill = pangolin_lineage)) +
# #   geom_treemap_text(aes(label = pangolin_lineage, area =n), color = "white") +
# #   scale_fill_manual(values = mycolors) +
# #   guides(fill = "none") +
# #   labs(title="Variant Proportions, Massachusetts",
# #        subtitle = paste0("Sequences from ", as.Date(max(MAviruses$date) - 10), " to ", as.Date(max(MAviruses$date))),
# #        caption = "Source: GISAID")
# # recent.viruses
# # ggsave("recent.viruses.pdf", width = 10, height =8)
