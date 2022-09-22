
covid50over.zipcode <- MGBcovidcases %>%
  filter(age_in_years >= 50) %>%
  filter(dxdate > as.Date("2022-04-01")) %>%
  group_by(zipcode, race.eth) %>%
  summarize(tests = n()) %>%
  ungroup()


OAV.town<-
  left_join(
    
    left_join(  
      covid50over.zipcode,   
      read_csv("~/Dropbox (Partners HealthCare)/GitHub/MA-Covid-Testing/MAtowns.csv") %>%
        mutate(zipcode = fixzip(zipcode)) %>%
        distinct(zipcode, Town, .keep_all = TRUE),
      
      by = c("zipcode")),
      OAV.rx %>% filter(rxdate > as.Date("2022-04-01") & age_in_years >49) %>%
        group_by(zipcode, race.eth) %>%
        count() %>%
        ungroup() %>%
        rename(oav_number = n),
      by = c("zipcode", "race.eth")) %>%
  mutate(oav_number = replace_na(oav_number, 0),
         tests = replace_na(tests, 0)) %>%
  filter(!is.na(Town)) %>%
  filter(tests> 4) %>%
  group_by(Town, race.eth, zipcode) %>%
  summarise(
    OAV_prescribed = sum(oav_number, na.rm = TRUE),
   # town_population = mean(town_population, na.rm = TRUE),
    COVID_cases50andover = sum(tests, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Town %in% boston) %>%
  arrange(Town, zipcode, race.eth)

write_csv(OAV.town, "MGB OAV prescribing Apr1toJune4.csv")