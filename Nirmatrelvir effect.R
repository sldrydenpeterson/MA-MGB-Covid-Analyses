library(tidyverse)
library(readxl)
library(broom)
library(janitor)
library(lubridate)
library(tableone)
library(flextable)
library(officer)
fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}
`%!in%` = Negate(`%in%`)

#setwd("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing")
setwd("~/Dropbox (Partners HealthCare)/Covid/Manuscripts/Nirmatrelvir")

# read in Enclave datasest and high SES SVI zipcode table for northeast
#enclave.pax<- data_frame(read_excel("paxlovid_dataset_29jun2022.xlsx", sheet = "all", guess_max =100000))

enclave.pax<- tibble(read_excel("paxlovid_dataset_30aug2022_LOCKED.xlsx", sheet = "paxlovid_rebound_dataset_17aug2", guess_max =150000))


ADI.pax<- read_csv("ADI.pax.csv") %>% ungroup() 

highSES_SVI.NE <-  data.frame(read_csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/highSES_SVI.NE.csv")) %>% select(-state) %>%
  arrange(zipcode, desc(highSES_SVI)) %>% distinct(zipcode, .keep_all = TRUE)


enclave.total<- 
  left_join(
      left_join(enclave.pax %>%
                mutate(zipcode = fixzip(zip)),highSES_SVI.NE, by = "zipcode"),
      ADI.pax, by = "EMPI") %>%
  mutate(
    covid_date = date(covid_date),
    admit_date = date(admit_date),
    paxlovid_date = case_when(
      date(paxlovid_date) < as.Date("2022-01-21") ~ as.Date(NA),  # date prescribing became available at MGB, prior scripts when out-of-state or not filled
      date(paxlovid_date) >=  as.Date("2022-01-21") ~  date(paxlovid_date),
      TRUE ~ as.Date(NA)),
    paxlovid = case_when(
      date(paxlovid_date) < as.Date("2022-01-21") ~ 0,  # date prescribing became available at MGB, prior scripts when out-of-state or not filled
      date(paxlovid_date) >=  as.Date("2022-01-21") ~  1, 
      TRUE ~ 0),
    birth_date = date(birth_date),
    death_date = date(death_date),
    molnupiravir_date = date(molnupiravir),
    remdesivir_date = date(remdesivir),
    sotrovimab_date = date(sotrovimab),
    bebtelovimab_date = date(bebtelovimab),
   # evusheld_date = date(evusheld),
    age.at.covid = floor(as.numeric((covid_date - birth_date)/365.25)), #) %>% select(EMPI, covid_date, admit_date, paxlovid_date, birth_date, death_date, remdesivir_date:evusheld_date)
  
    covid_admit = case_when(
      !is.na(admit_date) & admit_date-covid_date <29 ~ 1,
      TRUE ~ 0),
    covid_admit_date = if_else(covid_admit == 1, admit_date, as.Date(NA)),
    outpatient.coviddx = case_when(
      covid_date >= admit_date-1 ~ 0,
      covid_date >= death_date-1 & !is.na(death_date) ~ 0,
      covid_date < admit_date-1 ~ 1,
      is.na(admit_date) ~ 1,
      is.na(death_date) ~ 1),
    death.4wks = case_when(
      death_date <= covid_date+28 ~ 1,
      death_date > covid_date+28 ~ 0,
      is.na(death_date) ~0
    ),
   death.2wks = case_when(
     death_date <= covid_date+14 ~ 1,
     death_date > covid_date+14 ~ 0,
     is.na(death_date) ~0
   ),
   covid_admit_death = case_when(
     !is.na(admit_date) & admit_date-covid_date <15 ~ 1,
     !is.na(death_date) & death_date-covid_date <29 ~ 1,
     TRUE ~ 0),
    
    eGFR = case_when(
      is.na(gfr) ~ as.numeric(65),
      gfr == "> 60" ~ as.numeric(61),
      gfr == ">60" ~ as.numeric(61),
      gfr == ">60.0" ~ as.numeric(61),
      gfr == "> 60.0" ~ as.numeric(61),
      gfr == "> 90" ~ as.numeric(91),
      gfr == ">90" ~ as.numeric(91),
      gfr == "> 100" ~ as.numeric(101),
      gfr == ">100" ~ as.numeric(101),
      gfr == "> 120" ~ as.numeric(121),
      gfr == ">120" ~ as.numeric(121),
      TRUE ~ as.numeric(gfr)      ),
    
    highSES_SVI = if_else(is.na(highSES_SVI), 0, highSES_SVI),  #code as not high SVI for small number of zip codes without SVI (?PO boxes)
   
   #22 is the the 80% percentile in the sample
   highADI = case_when(
        ADI_NATRANK >=19 ~ 1,
        ADI_NATRANK <19 ~ 0,
        is.na(ADI_NATRANK) ~ 0),
    
    race.eth = case_when(
      ethnicity == "Hispanic" ~ "Hispanic or Latinx",
      race == "Black" ~ "Black",
      race == "White" ~ "White",
      race == "Asian" ~ "Asian",
      race == "Other" ~ "Other or unavailable",
      race == "Two or More" ~ "Other or unavailable",
      race == "Unknown/Missing" ~ "Other or unavailable",
      race == "American Indian or Alaska Nati" ~ "Other or unavailable",
      race == "Native Hawaiian or Other Pacif" ~ "Other or unavailable",
      TRUE ~ "Other or unavailable"),
    
    covid_vaccine_dose_1 = date(covid_vaccine_dose_1),
    covid_vaccine_dose_1 = date(covid_vaccine_dose_1),
    covid_vaccine_dose_2 = date(covid_vaccine_dose_2),
    covid_vaccine_dose_3 = date(covid_vaccine_dose_3),
    covid_vaccine_dose_4 = date(covid_vaccine_dose_4),
    covid_vaccine_dose_5 = date(covid_vaccine_dose_5),
    covid_vaccine_dose_6 = date(covid_vaccine_dose_6),
    
    contraindicated = case_when(
      immunosuppression_type___cyclosporine == 1 ~1, 
      immunosuppression_type___tacrolimus == 1 ~1, 
      immunosuppression_type___everolimus == 1 ~1, 
      immunosuppression_type___sirolimus == 1 ~1, 
      immunosuppression_type___clopidogrel == 1 ~1, 
      immunosuppression_type___rivaroxaban == 1 ~1, 
      immunosuppression_type___amiodarone == 1 ~1, 
      immunosuppression_type___carbamazepine == 1 ~1, 
      immunosuppression_type___phenytoin == 1 ~1, 
      immunosuppression_type___ranolazine == 1 ~1, 
      eGFR < 30 ~ 1,
      TRUE ~ 0),
    
    recentvaxdt = case_when(
      !is.na(covid_vaccine_dose_6) & covid_vaccine_dose_6 + 14 < covid_date ~ covid_vaccine_dose_6,
      !is.na(covid_vaccine_dose_5) & covid_vaccine_dose_5 + 14< covid_date ~ covid_vaccine_dose_5,
      !is.na(covid_vaccine_dose_4) & covid_vaccine_dose_4 + 14< covid_date ~ covid_vaccine_dose_4,
      !is.na(covid_vaccine_dose_3) & covid_vaccine_dose_3 + 14< covid_date ~ covid_vaccine_dose_3,
      !is.na(covid_vaccine_dose_2) & covid_vaccine_dose_2 + 14< covid_date ~ covid_vaccine_dose_2,
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date ~ covid_vaccine_dose_1,
      TRUE ~ as.Date(NA)),
    
    vaxstatus = case_when(             #vax receipt 14d or more prior to infection date
      !is.na(covid_vaccine_dose_6) & covid_vaccine_dose_6 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_5) & covid_vaccine_dose_5 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_4) & covid_vaccine_dose_4 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_3) & covid_vaccine_dose_3 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_2) & covid_vaccine_dose_2 + 14< covid_date ~ "Vaccinated",
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date & covid_vaccine_brand_1 == "JJ" ~ "Vaccinated",
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date ~ "Partially vaccinated",
      TRUE ~"Unvaccinated"),
    daysfromvax = as.numeric(covid_date - recentvaxdt),
    lastvaxgt20wks = case_when(
      daysfromvax >140 ~ 1,
      is.na(daysfromvax) ~ 1, 
      daysfromvax < 141 ~0),
    
    age.at.covid = floor(as.numeric((covid_date - birth_date)/365.25)), 
    
    age.cat = cut(age.at.covid, c(17, 49, 64, 79, 150)),
    age.cat = fct_recode(age.cat,
                         "18 to 49" = "(17,49]", 
                         "50 to 64" = "(49,64]", 
                         "65 to 79"= "(64,79]", 
                         "80 and older"= "(79,150]" ),
    
    bmi = case_when(
      !is.na(bmi) & bmi >= 12 ~ bmi,
      (is.na(bmi) | bmi < 12) & !is.na(weight_kg) & sex == "Female" & age.at.covid > 16 ~ weight_kg/1.63^2,  #average height in MA per CDC
      (is.na(bmi) | bmi < 12) & !is.na(weight_kg) & sex == "Male" & age.at.covid > 16 ~ weight_kg/1.78^2,   #average height in MA per CDC
      TRUE ~ as.numeric(NA)    ),
    obesity.cat = 
      fct_relevel(
        case_when(
          is.na(bmi) ~ "BMI unavailable",
          bmi < 12 ~ "BMI unavailable",
          bmi < 25 ~ "BMI less than 25",
          bmi < 30 ~ "BMI 25 to 30",
          bmi <= 35 ~ "BMI 30 to 35",
          bmi > 35 ~ "BMI greater than 35"), 
        "BMI unavailable", "BMI less than 25", "BMI 25 to 30", "BMI 30 to 35"),
    
    HTN = medhx___htn,
    diabetes = medhx___dm,
    CKD = case_when(
      eGFR < 60 ~ 1, 
      TRUE ~ 0),
    CVD = case_when(
      medhx___phtn == 1 ~ 1,
      medhx___cad == 1 ~ 1,
      medhx___chf == 1 ~ 1,
      # medhx___congenital == 1 ~ 1,
      medhx___stroke == 1 ~ 1,
      TRUE ~ 0),
    pulm.dis = case_when(
      medhx___copd == 1 ~ 1,
      medhx___ipfild == 1 ~ 1, 
      medhx___cf == 1 ~ 1,
      medhx___bronchiectasis == 1 ~ 1, 
      TRUE ~ 0),
    asthma = case_when(
      medhx___asthma == 1 & pulm.dis == 0 ~ 1,
      TRUE ~ 0), 
    hiv = case_when(
      medhx___hiv == 1  ~ 1,
      TRUE ~ 0), 
    inflam.dis = case_when(
      medhx___ibd == 1 ~ 1,
      medhx___sle == 1 ~ 1,
      medhx___ra == 1 ~ 1,
      medhx___sjogrens == 1 ~ 1,
      medhx___psoriatic == 1 ~ 1,
      TRUE ~ 0),
    ms = case_when(
      medhx___ms == 1 ~ 1,
      TRUE ~ 0 ),
    solid.malig = `cancer___solid onc`,
    # case_when(
    # cancer___breast ==1 ~1,
    # cancer___lung ==1 ~1,
    # cancer___colon ==1 ~1,
    # cancer___gastric ==1 ~1,
    # cancer___brain ==1 ~1,
    # cancer___melanoma ==1 ~1,
    # cancer___renal ==1 ~1,
    # cancer___gu ==1 ~1,
    # cancer___metastatic ==1 ~1,
    # cancer___breast ==1 ~1,
    # TRUE ~ 0),
    heme.malig = `cancer___heme mal`,
    # case_when(
    # cancer___all == 1 ~1,
    # cancer___aml == 1 ~1,
    # cancer___cll == 1 ~1,
    # cancer___cml == 1 ~1,
    # cancer___hl == 1 ~1,
    # cancer___nhl == 1 ~1,
    # cancer___mm == 1 ~1,
    # TRUE ~ 0),
    psych.non.unipolar = case_when(
      psych___bipolar == 1 ~ 1, 
      psych___schizodo == 1 ~ 1, 
      psych___schizo == 1 ~ 1, 
      psych___ocd == 1 ~ 1, 
      TRUE ~ 0    ),
    depr.anxiety = case_when(
      psych___depression == 1 & psych.non.unipolar == 0 ~ 1, 
      psych___anxiety == 1 & psych.non.unipolar == 0  ~1, 
      TRUE ~ 0 ),
    current.pregnant = pregnant, 
    immunosupr.meds = case_when(
      immunosuppression_type___prednisone == 1 ~ 1, 
      immunosuppression_type___mycophenolate == 1 ~ 1, 
      immunosuppression_type___cyclosporine == 1 ~ 1, 
      immunosuppression_type___azathioprine == 1 ~ 1, 
      immunosuppression_type___tacrolimus == 1 ~ 1,
      immunosuppression_type___everolimus == 1 ~1, 
      immunosuppression_type___sirolimus == 1 ~1,
      immunosuppression_type___rituximab== 1 ~1,
      immunosuppression_type___obinutuzumab == 1 ~1,
      immunosuppression_type___infliximab == 1 ~1,
      immunosuppression_type___etanercept == 1 ~1,
      immunosuppression_type___adalimumab == 1 ~1,
      immunosuppression_type___pembrolizumab == 1 ~1,
      immunosuppression_type___nivolumab == 1 ~1,
      immunosuppression_type___ocrelizumab == 1 ~1,
      immunosuppression_type___sirolimus == 1 ~1,
      TRUE ~ 0    ),
    MASS.age = case_when(
      age.at.covid >=65 ~ 2,
      TRUE ~ 0),
    MASS.dm = case_when(
      diabetes == 1 ~2,
      TRUE ~0),
    MASS.ckd = case_when(
      CKD == 1 ~3,
      TRUE ~0),
    MASS.ic = case_when(
      immunosupr.meds == 1 ~3,
      solid.malig == 1 ~ 3, 
      heme.malig == 1 ~3,
      hiv == 1 ~ 3,
      TRUE ~0),
    MASS.pulm = case_when(
      pulm.dis == 1 & age.at.covid >=55 ~ 3,
      TRUE ~0),
    MASS.cvd = case_when(
      CVD == 1 & age.at.covid >=55 ~ 2,
      TRUE ~0),
    MASS.bmi = case_when(
      bmi >=35 ~ 2,
      TRUE ~0),
    MASS.htn = case_when(
      medhx___htn == 1 & age.at.covid >=55 ~ 1,
      TRUE ~0),
    MASS = MASS.age + MASS.dm + MASS.ckd + MASS.ic + MASS.pulm + MASS.cvd + MASS.bmi + MASS.htn,
    MASS.cat = fct_relevel(case_when(
      MASS >= 6 ~ "MASS 6 or greater",
      MASS >= 4 ~ "MASS 4 and 5",
      MASS < 4  ~ "MASS 3 or less"), "MASS 3 or less",  "MASS 4 and 5"),
    vaxstatus.collapsed = case_when(
      vaxstatus == "Vaccinated and boosted" ~ "Vaccinated",
      vaxstatus == "Vaccinated" ~ "Vaccinated",
      TRUE ~ "Not fully vaccinated"    ),
    seqn = row_number(),
    outpatient.remdesivir = case_when(
      remdesivir_date >= covid_date & remdesivir_date < covid_date+30 & remdesivir_date < admit_date ~ 1,
      remdesivir_date >= covid_date & remdesivir_date < covid_date+30 & is.na(admit_date) ~ 1,
      TRUE ~ 0),
   coviddx_month = month(covid_date),
   studyperiod = case_when(
     covid_date < as.Date("2022-05-01") ~ "Jan to Apr",
    TRUE ~ "May to Jul")
   )
   


enclave.clean<- enclave.total %>%
  
  #setup cohort (dataset pre-filtered, not required to run these)
  filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
  filter(!is.na(bmi)) %>%
  filter(age.at.covid >=50) %>%  # include those 50 and older
  filter(state == "Massachusetts" | state == "New Hampshire") %>%  #include those with positive recorded test Jan 21 or later, and residing in MA/NH
  filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+30) %>%  # remove those that received molnupiravir within 30d of covid diagnosis
  filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+30) %>%      # remove those that received sotrovimab within 30d of covid diagnosis
  filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+30) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
  filter(is.na(paxlovid_date) |  paxlovid_date <  covid_date+2) %>%      # remove those that received paxlovid more then 1 day after covid diagnosis
  filter(outpatient.remdesivir == 0) %>%
  filter(contraindicated != 1) %>%  # remove those with eGFR <30 and with contraindicated meds
  
  select(EMPI, seqn, MASS.cat, age.cat, vaxstatus.collapsed, vaxstatus, lastvaxgt20wks, race.eth, highSES_SVI, highADI, outpatient.coviddx, paxlovid, paxlovid_date,
         covid_admit, covid_admit_death, covid_date, admit_date, death.4wks, death.2wks, death_date, sex, obesity.cat, MASS, prior_covid, prior_covid_date, eGFR, MASS.ic, MASS.dm, age.at.covid,
         CVD, pulm.dis, psych.non.unipolar, depr.anxiety, heme.malig, solid.malig, inflam.dis, coviddx_month, studyperiod)


enclave.clean %>% filter( outpatient.coviddx ==1) %>% group_by(paxlovid) %>% count(studyperiod, covid_admit_death) %>%
  pivot_wider(names_from = covid_admit_death, values_from = n) %>%
  mutate(pct = `1`/ (`1` +`0`))

# outpatient dx weights

# check positivity
enclave.clean %>% count(outpatient.coviddx, MASS.cat)
enclave.clean %>% count(outpatient.coviddx, age.cat)
enclave.clean %>% count(outpatient.coviddx, vaxstatus.collapsed)
enclave.clean %>% count(outpatient.coviddx, lastvaxgt20wks)
enclave.clean %>% count(outpatient.coviddx, race.eth)
enclave.clean %>% count(outpatient.coviddx, highADI)
enclave.clean %>% count(outpatient.coviddx, studyperiod)

enclave.clean %>%
  count(covid_admit_death, outpatient.coviddx)

# logistic model to generate denominator of IP weights, probability of outpatient covid diagnosis

denom.fit <- glm(
  outpatient.coviddx ~ as.factor(MASS.cat) + as.factor(age.cat) + as.factor(vaxstatus.collapsed)  
  + as.factor(lastvaxgt20wks) + as.factor(race.eth) + as.factor(highADI) + as.factor(studyperiod) ,
  family = binomial(),
  data = enclave.clean %>% mutate(race.eth = fct_relevel(race.eth, "White"))
)
summary(denom.fit)
tidy(denom.fit, conf.int = TRUE, exponentiate = TRUE)

pd.outpt <- predict(denom.fit, type = "response")

# numerator of stabilized weights, overall probability of outpt diagnosis of covid
numer.fit <- glm(outpatient.coviddx ~ 1, family = binomial(), data =enclave.clean)
pn.outpt <- predict(numer.fit, type = "response")


# calculate weights
enclave.clean$sw.outpt <-
  if_else(enclave.clean$outpatient.coviddx == 0, ((1- pn.outpt) / (1-pd.outpt)),
          (pn.outpt / pd.outpt))

quantile(enclave.clean$sw.outpt, c(0.025, 0.5, 0.975))
mean(enclave.clean$sw.outpt)
max(enclave.clean$sw.outpt)
min(enclave.clean$sw.outpt)

enclave.clean %>% 
  count(outpatient.coviddx, race.eth) %>%
  pivot_wider(names_from = outpatient.coviddx, values_from = n) %>%
  mutate(pct = `0`/ (`1` + `0`))

enclave.clean %>% 
  count(outpatient.coviddx, highADI) %>%
  pivot_wider(names_from = outpatient.coviddx, values_from = n) %>%
  mutate(pct = `0`/ (`1` + `0`))




# paxlovid prescription weights
enclave.clean %>% filter(outpatient.coviddx == 0) %>% count()
enclave.clean.outptdx <- enclave.clean %>% filter(outpatient.coviddx == 1) #limit to those diagnosed as outpatients


## abstract statistics
# overall analyzed population
enclave.clean.outptdx %>% count()

#vax percent
enclave.clean.outptdx %>% count(vaxstatus.collapsed) %>%
  mutate(pct = n/sum(n))

#pax number
enclave.clean.outptdx %>% count(paxlovid)%>%
  mutate(pct = n/sum(n))

#endpoints
enclave.clean.outptdx %>% count(paxlovid, covid_admit_death)%>%
  group_by(paxlovid) %>%
  mutate(pct = n/sum(n))

# median age
median(enclave.clean.outptdx$age.at.covid)

# #hosp with 48h
# enclave.clean.outptdx %>%
#   filter( paxlovid == 1 & covid_admit == 1) %>%
#   mutate(days = admit_date - covid_date) %>%
#   count(days <4)
# 
# #hosp with 48h
# enclave.clean.outptdx %>%
#   filter( paxlovid == 1 ) %>%
#   mutate(days = paxlovid_date - covid_date) %>%
#   count(days <4)



write_csv(enclave.clean.outptdx %>% filter(death.4wks == 1) %>%
            arrange(EMPI) %>%
            select(EMPI), "deathclean.csv")

#check positivity
enclave.clean.outptdx %>% count(paxlovid, MASS.cat)
enclave.clean.outptdx %>% count(paxlovid, age.cat)
enclave.clean.outptdx %>% count(paxlovid, vaxstatus.collapsed)
enclave.clean.outptdx%>% count(paxlovid, lastvaxgt20wks)
enclave.clean.outptdx %>% count(paxlovid, race.eth)
enclave.clean.outptdx %>% count(paxlovid, highADI)
enclave.clean.outptdx %>% count(paxlovid, studyperiod)

enclave.clean.outptdx %>% count(paxlovid, covid_admit)

# logistic model to generate denominator if IP weights, probability of paxlovid
denom.fit <- glm(
  paxlovid ~ as.factor(MASS.cat) + as.factor(age.cat) 
  + as.factor(vaxstatus.collapsed)  + as.factor(lastvaxgt20wks) + as.factor(race.eth) 
  + as.factor(highADI) + as.factor(studyperiod),
  family = binomial(),
  data = enclave.clean.outptdx %>% mutate(race.eth = fct_relevel(race.eth, "White")) )

tidy(denom.fit, conf.int = TRUE, exponentiate = TRUE)
pd.pax <- predict(denom.fit, type = "response")

# numerator of stabilized weights, overall probabilty of being prescribed paxlovid
numer.fit <- glm(paxlovid ~ 1, family = binomial(), data =enclave.clean.outptdx)
pn.pax <- predict(numer.fit, type = "response")

# calculate weights
enclave.clean.outptdx$sw.pax <-if_else(enclave.clean.outptdx$paxlovid == 0, ((1- pn.pax) / (1-pd.pax)),
                                       (pn.pax / pd.pax))

quantile(enclave.clean.outptdx$sw.pax, c(0.025, 0.5, 0.975))
mean(enclave.clean.outptdx$sw.pax)
max(enclave.clean.outptdx$sw.pax)
min(enclave.clean.outptdx$sw.pax)

enclave.clean.outptdx <- enclave.clean.outptdx %>%
  mutate(sw.pax = as.numeric(sw.pax),
         sw.outpt = as.numeric(sw.outpt),
         sw.overall = sw.pax * sw.outpt)
enclave.clean.outptdx %>%
  count(death.4wks, vaxstatus)

# raw hospitalization rates

enclave.clean.outptdx %>%
  group_by(paxlovid) %>%
  summarize(
    total = n(),
    covid_admit_death = sum(covid_admit_death)) %>%
  ungroup() %>%
  mutate(percent.covid_admit_death = covid_admit_death*100/total)

# diagnostics for weights
mean(enclave.clean.outptdx$sw.overall)
quantile(enclave.clean.outptdx$sw.overall, c(0, 0.01, 0.025, 0.05, .5, 0.95, 0.975, 0.99, 1))



# model for primary endpoint
pax.msm <- geepack::geeglm(
  covid_admit_death ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)


# model for hosp alone
pax.msm <- geepack::geeglm(
  covid_admit ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)

pax.msm <- geepack::geeglm(
  death.4wks ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)


# assessing whether model estimate stable to exclusion of
#influential observations (< 2.5% or >97.5% of sw.overall) [similar estimate and lower std.error with full dataset]
pax.msm <- geepack::geeglm(
  covid_admit_death ~ paxlovid ,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% filter(sw.overall>0.512 & sw.overall < 1.764))
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)

survival <- enclave.clean.outptdx %>%
  mutate(
    censordt = case_when(
      !is.na(admit_date) & admit_date <= covid_date +28 ~ admit_date,
      !is.na(death_date) & death_date <= covid_date +28 ~ death_date,
      TRUE ~ covid_date +29 ),
    futime = as.numeric(pmax(.5,  censordt - covid_date)), 
    paxlovid.fct = fct_relevel(as.factor(
      case_when(
      paxlovid == 1 ~ "Nirmatrelvir",
      paxlovid == 0 ~ "No nirmatrelvir")), "No nirmatrelvir")
    )

library(ggsci)
library(survival)
library(survminer)
library(gtsummary)
library(adjustedCurves)
library(riskRegression)
library(pammtools)

# Figure 2
library(showtext)
#showtext_auto(enable = FALSE)
font_add_google("Roboto", "Roboto")
##Primary endpoint

cox_mod <- coxph(Surv(futime, covid_admit_death) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# use it to calculate adjusted survival curves

adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="covid_admit_death",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100


# plot with confidence intervals
cum_primary<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Incidence (%)",
       title="Primary endpoint",
       subtitle = "Hospitalization within 14 days or death within 28 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_primary 



#secondary endpoint Death
cox_mod <- coxph(Surv(futime, death.4wks) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# use it to calculate adjusted survival curves

adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="death.4wks",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
#scale to per 1000
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100


# plot with confidence intervals
cum_deaths<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Deaths (%)",
       title="Deaths",
       subtitle = "Within 28 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_deaths


#secondary endpoint hospitalization

cox_mod <- coxph(Surv(futime, covid_admit) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# use it to calculate adjusted survival curves

adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="covid_admit",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
#scale to per 1000
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100

#limit to 14 days
adjsurv$adjsurv <-adjsurv$adjsurv %>% filter(time <= 14)


# plot with confidence intervals
cum_hosp<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Hospitalizations (%)",
       title="Hospitalizations",
       subtitle = "Within 14 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_hosp


## construct figure
library(patchwork)
layout <- "
AAAAAAA
AAAAAAA
AAAAAAA
BBB#CCC
BBB#CCC"

cum_primary + cum_hosp + cum_deaths + 
  plot_layout(design = layout)

ggsave("Fig 3 cumulative incidence.png", width = 10, height = 12, dpi=400)



# assessing for effect modification

#age
pax.msm1 <- geepack::geeglm(
  covid_admit ~ paxlovid + age.cat + age.cat*paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older"))) ))

pax.msm2 <- geepack::geeglm(
  covid_admit ~ paxlovid + age.cat,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older"))) ))

p.age <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`


#vax status
pax.msm1 <- geepack::geeglm(
  covid_admit ~ paxlovid + vaxstatus.collapsed +  vaxstatus.collapsed*paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )


pax.msm2 <- geepack::geeglm(
  covid_admit ~ paxlovid + vaxstatus.collapsed,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )

p.vaxstatus <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`

pax.msm1 <- geepack::geeglm(
  covid_admit ~ paxlovid + lastvaxgt20wks +  lastvaxgt20wks*paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )

pax.msm2 <- geepack::geeglm(
  covid_admit ~ paxlovid + lastvaxgt20wks,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )

p.vaxgt20wks <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`


# pax.msm1 <- geepack::geeglm(
#   covid_admit ~ paxlovid + (obesity.cat == "BMI 30 to 35" | obesity.cat == "BMI greater than 35")  +  (obesity.cat == "BMI 30 to 35" | obesity.cat == "BMI greater than 35")*paxlovid,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# 
# pax.msm2 <- geepack::geeglm(
#   covid_admit ~ paxlovid + (obesity.cat == "BMI 30 to 35" | obesity.cat == "BMI greater than 35"),
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# p.obesity.cat <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`


pax.msm1 <- geepack::geeglm(
  covid_admit ~ paxlovid + MASS.cat + MASS.cat*paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )

pax.msm2 <- geepack::geeglm(
  covid_admit ~ paxlovid + MASS.cat,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx )

p.MASS.cat <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`

# pax.msm1 <- geepack::geeglm(
#   covid_admit ~ paxlovid + highADI  + highADI*paxlovid,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# pax.msm2 <- geepack::geeglm(
#   covid_admit ~ paxlovid + highADI ,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# p.highSES_SVI  <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`

# pax.msm1 <- geepack::geeglm(
#   covid_admit ~ paxlovid + MASS.ic  + MASS.ic*paxlovid,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# pax.msm2 <- geepack::geeglm(
#   covid_admit ~ paxlovid + MASS.ic ,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx )
# 
# p.MASS.ic  <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`

# pax.msm1 <- geepack::geeglm(
#   covid_admit ~ paxlovid + race.eth + paxlovid*race.eth,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx %>% 
#     mutate(race.eth = fct_collapse(race.eth,
#                                    "Black or Hispanic/Latinx" = c("Black", "Hispanic or Latinx"),
#                                    "Asian, other or unavailable" = c("Asian", "Other or unavailable")) ))
# 
# pax.msm2 <- geepack::geeglm(
#   covid_admit ~ paxlovid  + race.eth,
#   family = poisson(link=log),
#   weights = sw.overall,
#   id = seqn,
#   corstr = "independence",
#   data = enclave.clean.outptdx  %>% 
#     mutate(race.eth = fct_collapse(race.eth,
#                                    "Black or Hispanic/Latinx" = c("Black", "Hispanic or Latinx"),
#                                    "Asian, other or unavailable" = c("Asian", "Other or unavailable")) ))
# 
# p.MASS.race  <-(anova(pax.msm1, pax.msm2))$`P(>|Chi|`



em.table <- tribble(
  ~Subgroup, ~p,
  "Age", p.age,
  "Vaccination status", p.vaxstatus,
  "Vaccination timing", p.vaxgt20wks,
  "Comorbidity score", p.MASS.cat
)



# #write_csv(enclave.clean.outptdx, "analytic cohort (diagnosed prior to admit), de-identified with weights.csv")
# 
# possible.nothighrisk<- enclave.clean.outptdx %>% filter(paxlovid == 1 & MASS == 0)
# enclave.clean.outptdx %>% count(paxlovid, covid_admit)
# enclave.clean.outptdx %>% count(paxlovid, death.4wks)

#unadjusted
pax.unadj <- glm(
  covid_admit_death ~ paxlovid,
  family = binomial(),
  data = enclave.clean.outptdx
)
summary(pax.unadj)
tidy(pax.unadj, conf.int = TRUE, exponentiate = TRUE)


enclave.clean.outptdx %>% count(paxlovid, covid_admit, death.4wks)

enclave.pax %>% count(paxlovid, covid_admit, death.4wks)

deaths<-enclave.clean.outptdx %>% 
  filter(death.4wks == 1) %>%
  select(EMPI, paxlovid, covid_admit, death.4wks)

deaths<-enclave.total %>% 
  filter(death.4wks == 1) %>%
  select(EMPI, paxlovid, paxlovid_date, covid_admit, death_date, death.4wks, covid_date, admit_date, age.at.covid, state, contraindicated, remdesivir, sotrovimab, molnupiravir, bebtelovimab)


pax.unadj <- glm(
  death.4wks ~ paxlovid,
  family = binomial(),
  data = enclave.clean.outptdx
)
summary(pax.unadj)
tidy(pax.unadj, conf.int = TRUE, exponentiate = TRUE)



# PLOTS

epi.plots <- rbind(
  enclave.clean.outptdx %>%
    group_by(covid_date) %>%
    count() %>% 
    rename(date = covid_date) %>%
    ungroup() %>%
    complete(date = seq.Date(as.Date("2022-01-01"), as.Date("2022-08-13"), by="day")) %>%
    mutate(name = "COVID-19",
           n = replace_na(n, 0)),
  enclave.clean.outptdx %>%
    group_by(paxlovid_date) %>%
    count() %>%
    rename(date = paxlovid_date) %>%
    ungroup() %>%
    complete(date = seq.Date(as.Date("2022-01-01"), as.Date("2022-08-13"), by="day")) %>%
    mutate(name = "Nirmatrelvir plus ritonavir prescription",
           n = replace_na(n, 0)),
  enclave.clean.outptdx %>%
    mutate(admit_date = pmin(admit_date, death_date, na.rm = TRUE)) %>%
    group_by(admit_date) %>%
    count() %>%
    rename(date = admit_date) %>%
    ungroup() %>%
    complete(date = seq.Date(as.Date("2022-01-01"), as.Date("2022-08-13"), by="day")) %>%
    mutate(name = "Hospital admission or death",
           n = replace_na(n, 0))) %>% 
  filter(!is.na(date)) %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-08-13")) %>%
  mutate(name = fct_relevel(name, "COVID-19", "Nirmatrelvir plus ritonavir prescription"))

epi.plots<- rbind(
enclave.clean.outptdx %>% filter(!is.na(covid_date)) %>%
  mutate(date = covid_date,
         endpoint = "COVID-19") %>%
  select(date, endpoint) %>% arrange(date),
enclave.clean.outptdx %>% filter(!is.na(paxlovid_date)) %>%
  mutate(date = paxlovid_date,
         endpoint = "Nirmatrelvir plus ritonavir prescription") %>%
  select(date, endpoint) %>% arrange(date),
enclave.clean.outptdx %>%
  mutate(admit_date = pmin(admit_date, death_date, na.rm = TRUE)) %>%
  filter(!is.na(admit_date)) %>%
  mutate(date = admit_date,
         endpoint = "Hospital admission or death") %>%
  select(date, endpoint) %>% arrange(date)) %>%
  mutate(endpoint = as.factor(endpoint)) %>%
  mutate(endpoint = fct_relevel(endpoint, "COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death"))


epi.plots %>% count(endpoint)
           

library(ggsci)
rollout<- epi.plots  %>%
  ggplot()+
  theme_classic()  + 
  theme(
    plot.title = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)),
    legend.position = c(0.75, 0.8),
    #axis.text.x = element_text(angle = 90, hjust =1),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank()) +
  geom_bar(data = epi.plots %>% filter(endpoint == "COVID-19"), aes( x= date, fill = endpoint)) +
  geom_bar(data = epi.plots %>% filter(endpoint == "Nirmatrelvir plus ritonavir prescription"), aes( x= date, fill = endpoint)) +
  geom_bar(data = epi.plots %>% filter(endpoint == "Hospital admission or death"), aes( x= date, fill = endpoint)) +
  #scale_fill_discrete(breaks=c("COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death")) +
  #scale_fill_nejm(alpha =  0.8, name = "", breaks=c("COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death")) +
  scale_fill_manual(values = c("#B2474599", "#00A1D599", "#grey98"), name = "", breaks=c("COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death")) +
   coord_cartesian(ylim = c(0,800), xlim= c(as.Date("2022-01-01"), as.Date("2022-08-13"))) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2022-08-13")),  labels=scales::label_date_short(),
               breaks = scales::pretty_breaks(8))+
  # scale_x_date(date_breaks = "1 week",  labels=scales::label_date_short())+
  labs(x="Date", y="Number of Patients",
       title="",
       subtitle = "") 
rollout
ggsave("Covid diagnosis treatment.pdf", width = 12, height =10)
ggsave("Covid diagnosis treatment.png", width = 12, height =10)




