

#read in ADI
ADI<- read_csv("US_2019_ADI_Census Block Group_v3.1.txt") %>%
  mutate(state.code= str_sub(FIPS,1,2),
         county.code = str_sub(FIPS, 3, 5),
         decile = cut(as.numeric(ADI_NATRANK), breaks = c(-1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 101), labels = c(1:10))) %>%
  filter(state.code == "21" | state.code == "25") %>%
  group_by(state.code, decile) %>%
  tally()

#obtain populations for zip codes
zip.pop2010<-read_csv("zip_code_database.csv")

MA.ADI.zip <- left_join(
  read_csv("MA_2019_ADI_9 Digit Zip Code_v3.1.txt") %>%
  mutate(zip = str_sub(ZIPID, 2,6)) %>%
  group_by(zip) %>%
  summarise(ADI_NATRANK= median(ADI_NATRANK, na.rm = TRUE)),
  zip.pop2010 %>% select(zip, irs_estimated_population_2015), by="zip") %>%
  mutate(decile = cut(as.numeric(ADI_NATRANK), 
                      breaks = c(-1, 9, 19, 29, 39, 49, 59, 69, 79, 89, 101), 
                      labels = c(1:10)))

MO.ADI.zip <- left_join(
  read_csv("MO_2019_ADI_9 Digit Zip Code_v3.1.txt") %>%
  mutate(zip = str_sub(ZIPID, 2,6)) %>%
  group_by(zip) %>%
  summarise(ADI_NATRANK = median(ADI_NATRANK, na.rm = TRUE)),
  zip.pop2010 %>% select(zip, irs_estimated_population_2015), by="zip") %>%
  mutate(decile = cut(as.numeric(ADI_NATRANK), 
                      breaks = c(-1, 9, 19, 29, 39, 49, 59, 69, 79, 89, 101), 
                      labels = c(1:10)))


weights<- left_join(
  MO.ADI.zip %>%
    filter(!is.na(decile)) %>%
    group_by(decile) %>%
    summarise(pop=sum(irs_estimated_population_2015, na.rm = TRUE)) %>%
    mutate(MO.proportion = pop/sum(pop)) %>%
    select(decile, MO.proportion),
  MA.ADI.zip %>%
    filter(!is.na(decile)) %>%
    group_by(decile) %>%
    summarise(pop=sum(irs_estimated_population_2015, na.rm = TRUE)) %>%
    mutate(MA.proportion = pop/sum(pop)) %>%
    select(decile, MA.proportion), 
  by = "decile") %>%
  mutate(weight = MO.proportion/MA.proportion)

pop.dist<-weights %>%
  select(decile, MO.proportion, MA.proportion) %>%
  pivot_longer(!decile, names_to="state", values_to="proportion") %>%
  mutate(state = if_else(state == "MO.proportion", "Missouri", "Massachusetts")) 

pop.dist.MO<- ggplot(data=pop.dist %>% filter(state == "Missouri"), aes(x=decile, y=proportion*100))+
  theme_classic()+
  geom_col(fill="#1d91c0", color="black") +
  labs(x="Decile of Vulnerablity",
       y="Percent of Population",
       title="Population Vulnerablity, Missouri",
       subtitle = "Area Depreviation Index (national ranks)")

pop.dist.MA<- ggplot(data=pop.dist %>% filter(state == "Massachusetts"), aes(x=decile, y=proportion*100))+
  theme_classic()+
  geom_col(fill="#1d91c0", color="black") +
  labs(x="Decile of Vulnerablity",
       y="Percent of Population",
       title="Population Vulnerablity, Massachusetts",
       subtitle = "Area Depreviation Index (national ranks)")

load("vaccine.sex.Rdata")

MA.weighted <- left_join(
                left_join(
                    MA.ADI.zip, 
                    vaccine.sex %>% filter(reportdate==max(reportdate)) %>% 
                      rename(zip = zipcode) %>%
                      select(zip, fullvax.total),
                    by = "zip"),
                weights, by= "decile") %>%
  mutate(fullvax.wt = fullvax.total*weight,
         fullvax = fullvax.total,
         pop.wt= irs_estimated_population_2015*weight,
         pop= irs_estimated_population_2015)

MA.decile<- MA.weighted %>%
  filter(irs_estimated_population_2015>0 & !is.na(decile)) %>%
  group_by(decile) %>%
    summarise(decile.pop.wt=sum(pop.wt, na.rm = TRUE) *1.18, #total pop increase 2015 (~18% per comparison of sums)
              decile.pop=sum(pop, na.rm = TRUE) *1.18,
              decile.fullvax.wt=sum(fullvax.wt, na.rm = TRUE),
              decile.fullvax=sum(fullvax, na.rm = TRUE),
              coverage.decile= decile.fullvax/decile.pop)  
MA.decile %>%
  summarise(sum(decile.pop)) #check to make sure aligns iwth MA population

coverage<- MA.decile %>%
  mutate(state = "Massachusetts") %>%
  group_by(state) %>%
  summarize(coverage.raw = sum(decile.fullvax)/sum(decile.pop),
            coverage.std = sum(decile.fullvax.wt)/sum(decile.pop.wt)) %>%
  add_row(state="Missouri", coverage.raw = 0.461) %>%
  pivot_longer(!state, names_to="type", values_to="proportion") %>%
  mutate(state = fct_rev(state)) %>%
  mutate(type= if_else(type == "coverage.raw", "Measured", "Standardized to Missouri Population"))

coverage.plot<- coverage %>%
  ggplot(aes(fill=type, y=proportion*100, x=state)) +
  theme_classic()+
  geom_bar(position = "dodge", stat="identity", color="black") +
  geom_text(
    aes(label = paste0(round(proportion*100, 1), "%")),
    colour = "black", size = 5,
    vjust = -1, position = position_dodge(.9))+
  geom_text(
    aes(label = type),
    colour = "black", size = 5, angle =90,
    hjust = 1.15, position = position_dodge(.9))+
  scale_fill_brewer(palette = "PuBu")+
  coord_cartesian(ylim=c(0,75)) +
 # geom_text(aes(label = paste0(round(proportion*100,1), "%"), vjust = -0.2), position = "dodge")+
  labs(y="Percent of Population", x="",
       title="Vaccination Coverage, Missouri and Massachusetts") +
  theme(legend.position = "none")

library(devtools)
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

household_data <- left_join(countydata, counties, by = "county_fips") 

MO<-household_data %>%
  filter(state_name =="Missouri") %>% 
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  scale_fill_distiller(palette = "PuBu")+
  geom_polygon(color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")+
  theme_void() +  theme(legend.position = "none")

MA<-household_data %>%
  filter(state_name =="Massachusetts") %>% 
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  scale_fill_distiller(palette = "PuBu")+
  geom_polygon(color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")+
  theme_void() +  theme(legend.position = "none")

library(patchwork)
layout <-'
  ABC
  DEC
'

MA.MO.plot <- MO + pop.dist.MO + coverage.plot + MA +pop.dist.MA +plot_layout(design=layout, widths = 1)


  
