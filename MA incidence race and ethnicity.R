
labels <- tribble(
  ~race.ethnicity, ~race.ethnicity.short, 
  "Hispanic", "Latinx",
  "White, non-Hispanic", "White",
  "Native Hawaiian/ Pacific Islander, non-Hispanic", "NH/PI",
  "Black or African American, non-Hispanic", "Black",
  "Asian, non-Hispanic", "Asian",
  "American Indian/Alaskan Native, non-Hispanic", "AI/AN"
)


mdph_race <- left_join(read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/mdph_race.csv") %>%
  rename(race.ethnicity = 'Race/Ethnicity',
         all.cases = 'All Cases',
         pop = 'Population_Estimate_N') %>%
  dplyr::select(Date, Start_Date, End_Date, race.ethnicity, all.cases, pop) %>%
  group_by(race.ethnicity) %>%
  mutate(Date = ymd(Date),
         new.cases = all.cases - lag(all.cases, 1),
         new.cases = if_else(new.cases < 0, lag(new.cases,1), new.cases), # some case correction leads to negative case counts, mitigating here
    incidence1day= (new.cases*100000/pop)/7), labels, by = "race.ethnicity")


mdph_race %>% filter( race.ethnicity.short %in% c("Latinx", "White", "Black", "Asian")) %>%
  ggplot( aes(x=Date, y=incidence1day, group=race.ethnicity.short, color= race.ethnicity.short)) +
  theme_classic() +  theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(size=1.5)+
  #geom_smooth(alpha=.7, size=1.5, method="loess", span= 0.2, se=FALSE) +
  labs(x="Date", y="Daily Incidence, per 100,000",
       title="Incidence of Covid-19 by Race/Ethnicity",
       subtitle = paste0("Last data available: ", max(mdph_race$Date)),
       caption="Source: Massachusetts Department of Public Health" )+
  scale_color_jama(palette = c("default"))+
  geom_label_repel(data=mdph_race %>% ungroup() %>% filter( race.ethnicity.short %in% c("Latinx", "White", "Black", "Asian"))  %>% filter(Date == max(ymd(Date))), 
                   aes(label=paste0( race.ethnicity.short,"\n",round(incidence1day,1),"/100,000"),  x = Date, y = incidence1day, 
                       color= race.ethnicity.short),min.segment.length = 0,
                   xlim=c(Sys.Date(), Sys.Date()+150)) +
  guides(color=FALSE) +
  coord_cartesian( xlim=c(as.Date("2020-10-01"), Sys.Date()+150))    +
  scale_x_date(breaks = scales::breaks_pretty(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(10))

ggsave("~/Dropbox (Partners HealthCare)/R files/covid/Incidence by race ethnicity.pdf", width = 10, height=10)


