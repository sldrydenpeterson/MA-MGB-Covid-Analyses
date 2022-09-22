library(rgdal)
library(rgeos)
#library(zipcodeR)
library(tidyverse)
library(readxl)
library(broom)
library(gganimate)
library(gifski)
library(ggrepel)


load(file = "vaccine.sex.Rdata")
load(file = "vaccine.age.Rdata")
load(file = "vaccine.town.Rdata")
load(file = "vaccine.race.Rdata") #just to get the race/eth breakdown by town

#weekly MDPH printed summary reports, statewide
load(file = "VaxRace.Rdata")
load(file = "CasesRace.Rdata")

# 
# MAzipcodes<- search_state("MA") %>%
#   dplyr::select(zipcode, population, major_city, median_household_income)
# 
# vaccine_prev<-left_join(MAzipcodes, vaccine.sex, by="zipcode") %>%
#         mutate(vaccine_prev=fullvax.total/population)
# 
# 
# 
metrotowns<-c("Chelsea", "Allston Brighton", "Charlestown","Back Bay Downtown", "East Boston", "Dorchester Uphams", "Dorchester Codman", "Fenway", "Hyde Park",
              "Jamaica Plain", "Mattapan", "Roslindale", "Roxbury", "South Boston", "South End", "West Roxbury", "Brookline", "Newton", "Watertown",
              "Cambridge", "Somerville", "Medford", "Everett", "Winthrop", "Revere", "Weston", "Wellesley", "Waltham", "Belmont", "Arlington", "Malden",
              "Needham", "Dover", "Dedham", "Milton", "Quincy", "Nahant", "Lynn", "Saugus", "Melrose", "Woburn", "Lexington", "Lincoln", "Concord",
              "Lincoln", "Sudbury", "Wayland", "Natick", "Sherborn", "Westwood", "Norwood", "Canton", "Randolph",
              "Weymouth", "Hingham", "Hull", "Braintree", "Winchester", "Wakefield", "Stoneham", "Salem", "Swampscott",
              "Marblehead", "Framingham", "Bedford", "Burlington", "Hanscom Afb", "Medfield", "Sherborn", "Walpole")
# 
# #import massachusetts zipcodes from docx.digital.mass.gov
# zips<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/zipcodes_nt/ZIPCODES_NT_POLY.shp")
# zips <- gBuffer(zips, byid=TRUE, width=0) #fixing errors related to junction errors
# zips_WSG84 <- spTransform(zips, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# zips.tidy <- tidy(zips_WSG84, region = c('POSTCODE'))
# 
# ZipVaxGeo <-  right_join(vaccine_prev, zips.tidy, by = c("zipcode" ="id"))
# 
# ggplot()+
#   geom_polygon(data = ZipVaxGeo,  
#                aes(x = long, y = lat, group = group, fill=vaccine_prev*100), 
#                colour = "white", alpha = 0.9, size = 0.05) +
#   coord_map() + theme_void()+ 
#   scale_fill_distiller(palette='YlOrRd', direction=1, breaks=c(0,25,50),
#                        limits=c(0,51), na.value = "#800026",
#                        name="Percent of\npopulation")  +
#   labs(title="Vaccination Prevalence by Zip Code, Metro-Boston",
#        subtitle=paste0("Vaccinations through ", as.Date("2021-04-13")), 
#        caption="Source: Massachusetts Department of Public Health") +
#   geom_label( data = incid.metroboston %>%
#                 distinct(Town, .keep_all = TRUE), 
#               size=1.5, color="#374E55FF",
#               aes(x=xname, y=yname,  label=paste0(Town)))+
#   coord_cartesian(xlim = c(-71.3, -70.8), ylim=c(42.1, 42.5))
# ggsave("VaccinePrev MetroBoston.pdf")
# 
# ggplot()+
#   geom_polygon(data = ZipVaxGeo,  
#                aes(x = long, y = lat, group = group, fill=vaccine_prev*100), 
#                colour = "white", alpha = 0.9, size = 0.05) +
#   coord_map() + theme_void()+ 
#   scale_fill_distiller(palette='YlOrRd',  direction=1, breaks=c(0,25,50),
#                        limits=c(0,51), na.value = "#800026",
#                        name="Percent of\npopulation")  +
#   labs(title="Vaccination Prevalence by Zip Code, Massachusetts",
#        subtitle=paste0("Vaccinations through ", as.Date("2021-04-13")), 
#        caption="Source: Massachusetts Department of Public Health") 
# ggsave("VaccinePrev MA.pdf")
# 
MAtownpoptable<- read_csv("MAtownpoptable.csv")

#get town prevalence of vaccine and those older then 64yrs, using separate file from MDPH
town.vax1 <- left_join( #to add in population
  left_join( vaccine.town %>% filter(reportdate == max(as.Date(reportdate))) %>%
  group_by(Town)%>%
  filter(Age_group !="Total")%>%
  summarise(fullvax.total=sum(fullvax, na.rm = TRUE),
            boostvax.total=sum(boostvax, na.rm = TRUE))%>%
  mutate( Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  ), #remove parenthetic phrase
  
  vaccine.town %>% filter(reportdate == max(as.Date(reportdate))) %>%
  group_by(Town)%>%
  mutate(Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  ) %>% #remove parenthetic phrase
  filter(Age_group=="65-74 Years" | Age_group=="75+ Years" ) %>%
  summarise(age65up=sum(pop)),
  by="Town"),
  MAtownpoptable, by= "Town") %>%
  mutate(age65up.pct=town_population,
         fullvaccine_prev = fullvax.total*100/town_population,
         boostvaccine_prev = boostvax.total*100/town_population) %>% dplyr::select(-age65up)

town.vax1.long <- left_join(vaccine.town %>%
  group_by(reportdate, Town, .groups = TRUE)%>%
  filter(Age_group !="Total")%>%
  summarise(fullvax.total=sum(fullvax, na.rm = TRUE),
            boostvax.total=sum(boostvax, na.rm = TRUE)), MAtownpoptable, by= "Town")  %>%
  mutate(fullvaccine_prev=fullvax.total*100/town_population,
         boostvaccine_prev=boostvax.total*100/town_population,
         Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  )


#get boston neighborhood prevalence of vaccine
boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
             "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

#read in .csv files of zipcodes and population
MAtownsbos <- read_csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/MAtowns.csv") %>%
  filter(Town %in% boston) %>%
  mutate(zipcode = sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
         )


bos.vax1<-left_join(MAtownsbos, vaccine.sex %>%  ungroup()%>%
                      filter(reportdate == max(reportdate)), by="zipcode") %>%
  group_by(Town) %>%
  summarise(town_population=mean(town_population, na.rm= TRUE),
            fullvax.total=sum(fullvax.total, na.rm= TRUE),
            boostvax.total=sum(boostvax.total, na.rm= TRUE)) %>%
  mutate(fullvaccine_prev=fullvax.total*100/town_population,
         boostvaccine_prev=boostvax.total*100/town_population,
         age65up.pct = 0.1166) # boston wide population, does not use zipcode-specific

bos.vax1.long <-left_join(vaccine.sex %>%  ungroup(), MAtownsbos, by="zipcode") %>%
  group_by(reportdate, Town, .groups = TRUE) %>%
  summarise(town_population=mean(town_population, na.rm= TRUE),
            fullvax.total=sum(fullvax.total, na.rm= TRUE),
            boostvax.total=sum(boostvax.total, na.rm= TRUE)) %>%
  mutate(vaccine_prev=fullvax.total*100/town_population,
         age65up.pct = 0.1166) %>% # boston wide population, does not use zipcode-specific
  filter(!is.na(Town))


#add boston neighborhoods to MA town table
town.vax2<- rbind(town.vax1, bos.vax1) %>%
  filter (Town !="Boston", Town !="Unspecified") 

town.vax2.long<- rbind(town.vax1.long, bos.vax1.long) %>%
  filter (Town !="Boston", Town !="Unspecified") 

town.vax2.long<- rbind(town.vax2.long, 
                      town.vax2.long %>% filter(reportdate == as.Date("2021-03-11")) %>%
                      mutate(reportdate = as.Date("2020-12-24"),
                             fullvax.total = 0, 
                             vaccine_prev = 0.1)
)
                      
  

# #get town populations and cumulative incidence
# town.incid<-left_join(read_csv("allcovidtowns.csv") %>% group_by(Town) %>% 
#   filter(date==max(as.Date(date)) & Town != "Unknown town"),
#     read_csv("MAtownpoptable.csv"), by="Town") %>%
#   mutate(cumulative_incidence=Count*100/town_population)

#read in incidence covid cases and characteristics of communities
allcovidtowns <-left_join(read_csv("allcovidtowns.csv", guess_max=10000),
                MAtownSES <-read_csv("MAtownSES.csv", guess_max=10000), by="Town") %>%
  rename(SVI_SES=RPL_THEME1.town, SVI_ages_disability=RPL_THEME2.town,
         SVI_minority=RPL_THEME3.town, SVI_house=RPL_THEME4.town,
         SVI_overall=RPL_THEMES.town)  %>%
  # Socioeconomic – RPL_THEME1
  # Household Composition & Disability – RPL_THEME2
  # Minority Status & Language – RPL_THEME3
  # Housing Type & Transportation – RPL_THEME4
  # Overall tract rankings:  RPL_THEMES.
  mutate(
    quartile.SVI_SES=cut(SVI_SES, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_ages_disability=cut(SVI_ages_disability, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_minority=cut(SVI_minority, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_house=cut(SVI_house, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_overall=cut(SVI_overall, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE)
  ) 

labels<- read_csv("town.neighborhood.labels.csv")

town.vax<- left_join(
  left_join(town.vax2, allcovidtowns %>% filter(date==max(as.Date(date))), by="Town") %>%
  mutate(cumulative_incidence=Count*100/town_population,
         VIR=fullvaccine_prev/cumulative_incidence, 
         blacklatinx.pct=(pop.black + pop.latino)/pop.total, 
         blacklatinx.ord=cut(blacklatinx.pct, breaks=c(-1, 0.20, Inf), labels=c(1,2)),
         population.ord=as.numeric(cut(town_population, breaks=c(-1, 50000, Inf), labels=c(1,2))),
         population.10=town_population/10000,
         herd = fullvaccine_prev + (1-fullvaccine_prev/100) * (cumulative_incidence*2),
         age65up.ord=as.numeric(cut(age65up.pct, breaks=c(-1, 0.15, 0.2, 0.25, Inf), labels=c(1,2,3,4)))), 
         labels, by = "Town") #%>%
 # filter(population>3000)

town.vax.long<-left_join(town.vax2.long, allcovidtowns %>% mutate(reportdate = date), by=c("reportdate", "Town") ) %>%
  mutate(cumulative_incidence=Count*100/town_population,
         VIR=fullvaccine_prev/cumulative_incidence, 
         blacklatinx.pct=(pop.black + pop.latino)/pop.total, 
         blacklatinx.ord=cut(blacklatinx.pct, breaks=c(-1, 0.20, Inf), labels=c(1,2)),
         population.ord=as.numeric(cut(town_population, breaks=c(-1, 50000, Inf), labels=c(1,2))),
         population.10=town_population/10000,
         herd = fullvaccine_prev + (1-fullvaccine_prev/100) * (cumulative_incidence*2),
         age65up.ord=as.numeric(cut(age65up.pct, breaks=c(-1, 0.15, 0.2, 0.25, Inf), labels=c(1,2,3,4)))) 


# write_csv(left_join(read_csv("MAtowns.csv"), town.vax %>%
#             mutate(population=round(population, 0),
#                    cum_covid_total=Count,
#                    covid_prev=Count/population) %>%
#             arrange(VIR), by="Town" ) %>%
#             dplyr::select(Town, zipcode, population, SVI_SES, blacklatinx.pct, cum_covid_total, covid_prev, fullvax.total, vaccine_prev, VIR) %>%
#             filter(population>1000) %>% arrange(VIR),
#           "20210501 VIR for med students.csv")


totalpop <- town.vax %>%
  tally(town_population)

totalcovid<- town.vax %>%
  tally(Count)

totalvax<- town.vax %>%
  tally(fullvax.total)

totalvax/totalcovid

min(town.vax$fullvaccine_prev)
max(town.vax$fullvaccine_prev)

totalcovid/totalpop
totalvax/totalpop

# 
# library(sandwich)
# library(pscl)
# 

# summary(m1 <- glm(VIR ~ blacklatinx.ord + population.ord + quartile.SVI_SES + age65up.ord,
#                   family="poisson", data=town.vax %>% filter(town_population>3000)))
# 
# cov.m1 <- vcovHC(m1, type="HC0")
# std.err <- sqrt(diag(cov.m1))
# r.est <- cbind(Estimate= exp(coef(m1)), "Robust SE" = std.err,
#                "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
#                LL = exp(coef(m1) - 1.96 * std.err),
#                UL = exp(coef(m1) + 1.96 * std.err))
# r.est
# with(m1, cbind(res.deviance = deviance, df = df.residual,
#                p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# 
# detach(package:pscl,unload=TRUE)
# detach(package:sandwich,unload=TRUE)


# #Lorenz, Gini
# 
# totalvaxMA<-(town.vax %>% tally(fullvax.total))$n
# totalcaseMA<-(town.vax %>% tally(Count))$n
# jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")
# town.vax %>%
#   arrange(desc(SVI_SES)) %>%  #arrange by SES
#   mutate(cum_Count=cumsum(Count), 
#          cum_Count_prop=cum_Count/totalcaseMA,
#          cum_fullvax=cumsum(fullvax.total),
#          cum_fullvax_prop=cum_fullvax/totalvaxMA,
#          identity_x=cum_Count_prop,
#          identity_y=cum_Count_prop) %>%
#   mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
#                      labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)",
#                               "Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
#   dplyr::select(Town, Count, cum_Count, cum_Count_prop, fullvax.total, cum_fullvax, cum_fullvax_prop, SVI_SES,
#          identity_y, identity_x, SES.cat) %>%
#   add_row(identity_x = 0, identity_y = 0, cum_fullvax_prop=0, cum_Count_prop=0, SES.cat=as.factor("High\n(75-100 percentile)"))   %>%  #add origin to complete lines
#   ggplot() +
#   #theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#   #     axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_ribbon(aes(ymin=cum_fullvax_prop-0.01, ymax=cum_fullvax_prop+0.01,x=cum_Count_prop, fill=SES.cat))+
#   geom_line(aes(x=identity_x, y=identity_y),linetype="dashed") +
#   theme_light() +
#   labs(title= "Alignment of Vaccination to Covid-19 Burden in Massachusetts Communities",
#        subtitle=paste0("Lorenz Curve | Data current as of ", max(town.vax$date-2)),
#        x="Cumulative Proportion of Confirmed Covid-19 Cases",
#        y="Cumulative Proportion of Fully-Vaccinated Individual") +
#   scale_fill_manual(values=jama, name="Community\nSocioeconomic\nVulnerability")+
#   theme(legend.position = c(.3, .8), aspect.ratio = 1, panel.grid.minor = element_blank(),
#        panel.grid.major = element_blank(), plot.title = element_text(size = rel(1.5)),
#        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5) ),
#        legend.text = element_text(size = rel(1.5) ))
# ggsave("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/Lorenz plot SES.pdf", units = "in", width = 8, height=8)


#Gini by individual
#install.packages("REAT_3.0.2.tar", repos = NULL, type="source")

# 
# library(REAT)
# # CasesRace<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="CasesRace") %>%
# #     mutate(date=lubridate::ymd(date)) %>%
# #   mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
# #   dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
# #   pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "CaseCount")
# # VaxRace<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine") %>%
# #   mutate(date=lubridate::ymd(date)) %>%
# #   filter(date==max(date)) %>%
# #   mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
# #   dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
# #   pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "VaccineCount")
# 
# CasesRace1<-CasesRace %>%
#     filter(date == max(date)) %>%
#   mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
#   dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
#   pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "CaseCount")
# 
# VaxRace1<- VaxRace %>% filter(date == max(date)) %>%
#   mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
#   dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
#   pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "VaccineCount") 
# 
# ## developing Lorenz curves
# 
# ##by individuals
# #vaccines and cases by race/ethnic categories and combined for all categories
# total.byinvid<-left_join(CasesRace1, VaxRace1, by=c("date", "RaceEth")) 
# totalvaxMA.byinvid<-(total.byinvid %>% tally(VaccineCount))$n
# totalcaseMA.byinvid<-(total.byinvid %>% tally(CaseCount))$n
# 
# 
# #cumulative number of cases/vaccines for Lorenz
# g1<-total.byinvid %>%  
#   mutate(VIR=VaccineCount/CaseCount) %>%
#   arrange(VIR) %>%
#   mutate(cum_Count=cumsum(CaseCount),
#          cum_Count_prop=cum_Count/totalcaseMA.byinvid,
#          cum_fullvax=cumsum(VaccineCount), 
#          cum_fullvax_prop=cum_fullvax/totalvaxMA.byinvid,
#          identity_x=cum_Count_prop,
#          identity_y=cum_Count_prop) %>%
#   dplyr::select(cum_fullvax_prop, cum_Count_prop, identity_x, identity_y, RaceEth, VIR)
# 
# #calculating Hoover and Gini indices using REAT package
# g1.hoover<-hoover(total.byinvid$VaccineCount, total.byinvid$CaseCount)
# g1.gini<-gini(total.byinvid$VaccineCount, total.byinvid$CaseCount, coefnorm = FALSE,  na.rm = TRUE)
# 
# #need to duplicate to get colors to go from min to max
# g2<-rbind(g1, g1) %>%
#   arrange(VIR) %>%
#   mutate(cum_fullvax_prop=lag(cum_fullvax_prop,1),
#          cum_Count_prop=lag(cum_Count_prop,1),
#          RaceEth=fct_reorder(fct_recode(as.factor(RaceEth), 
#                             "Latinx" = "Hispanic",
#                             "Multiple/Other" = "AllOther" ), VIR),
#          identity_x = if_else(is.na(cum_Count_prop),0,cum_Count_prop),
#          identity_y = if_else(is.na(cum_Count_prop),0,cum_Count_prop))%>%
#   replace(is.na(.), 0)
# 
# 
# jama5<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF", "#00A1D5FF")
# l2<-g2 %>%
#   ggplot() +
#   geom_ribbon(aes(ymin=cum_fullvax_prop-0.015, ymax=cum_fullvax_prop+0.015, x=cum_Count_prop, fill=RaceEth))+
#   geom_line(aes(x=identity_x, y=identity_y),linetype="dashed") +
#   theme_light() +
#   labs(title= "Distribution of Vaccination and SARS-CoV-2 Infection\namong Massachusetts Residents",
#        x="Cumulative Proportion of Confirmed SARS-CoV-2 Infection",
#        y="Cumulative Proportion of Fully-Vaccinated Individuals") +
#   scale_fill_manual(values=jama5, name="Race/Ethnicity")+
#   annotate("text", x=0.8, y=0.1, label=paste0("Gini Index ", round(g1.gini,2), "\nHoover Index ", round(g1.hoover,2)), size=7)+
#   theme(legend.position = c(.3, .8), aspect.ratio = 1, panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(), plot.title = element_text(size = rel(1.5)),
#         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5) ),
#         legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5)) )
# 
# 
# totalvaxMA<-(town.vax %>% tally(fullvax.total))$n
# totalcaseMA<-(town.vax %>% tally(Count))$n
# jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")
# 
# ##by Towns
# g3<-town.vax %>% filter(town_population>3000) %>%
#   arrange(desc(blacklatinx.pct)) %>%  #arrange by SES
#   mutate(cum_Count=cumsum(Count), 
#          cum_Count_prop=cum_Count/totalcaseMA,
#          cum_fullvax=cumsum(fullvax.total),
#          cum_fullvax_prop=cum_fullvax/totalvaxMA,
#          identity_x=cum_Count_prop,
#          identity_y=cum_Count_prop) %>%
#   mutate(blacklatinx.cat=cut(blacklatinx.pct, breaks=c(-1, 0.05, 0.1, 0.2, Inf),
#                      labels=c("0 to 5%", "5 to 10%", "10 to 20%", "> 20%"))) %>%
#   dplyr::select(Town, Count, cum_Count, cum_Count_prop, fullvax.total, cum_fullvax, cum_fullvax_prop, blacklatinx.cat,
#          identity_y, identity_x, blacklatinx.pct)
# 
# g3.hoover<-hoover(town.vax$fullvax.total, town.vax$Count)
# g3.gini<-gini(town.vax$fullvax.total, town.vax$Count, coefnorm = FALSE, na.rm = TRUE)
# 
# #detach(package:REAT, unload=TRUE)
# 
# # need to duplicate to allow colors to go from min to max cum cases/vax
# g4<-rbind(g3, g3) %>%
#   arrange(desc(blacklatinx.pct)) %>%
#   mutate(cum_fullvax_prop=lag(cum_fullvax_prop,1),
#          cum_Count_prop=lag(cum_Count_prop,1),
#          identity_x = if_else(is.na(cum_Count_prop),0,cum_Count_prop),
#          identity_y = if_else(is.na(cum_Count_prop),0,cum_Count_prop))%>%
#   replace(is.na(.), 0) %>%
#   mutate(blacklatinx.cat= fct_reorder(blacklatinx.cat, desc(blacklatinx.pct)))
# 
# l1<- g4 %>%
#   ggplot() +
#   geom_ribbon(aes(ymin=cum_fullvax_prop-0.015, ymax=cum_fullvax_prop+0.015,x=cum_Count_prop, fill=blacklatinx.cat))+
#   geom_line(aes(x=identity_x, y=identity_y),linetype="dashed") +
#   theme_light() +
#   labs(title= "Distribution of Vaccination and SARS-CoV-2 Infection\namong Massachusetts Communities",
#        x="Cumulative Proportion of Confirmed SARS-CoV-2 Infection",
#        y="Cumulative Proportion of Fully-Vaccinated Individuals") +
#   scale_fill_manual(values=jama, name="Community\nBlack and/or Latinx\nProportion")+
#   annotate("text", x=0.8, y=0.1, label=paste0("Gini Index ", round(g3.gini,2), "\nHoover Index ", round(g3.hoover,2)),size=7)+
#   theme(legend.position = c(.3, .8), aspect.ratio = 1, panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(), plot.title = element_text(size = rel(1.5)),
#         axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5) ),
#         legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5)) )
# library(patchwork)
# l1 +l2 + plot_layout(ncol=2) + plot_annotation(tag_levels = 'A')
# ggsave("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/Lorenz plot race.pdf", units = "in", width = 16, height=8)
# 
# l2
# ggsave("lorenz.pdf", width=9, height=9)
# 
  
#vaccination during Delta
# select predelta vax
vaxsincejuly<- town.vax.long %>%
            ungroup() %>%
            filter(date == as.Date("2021-07-01")) %>%
  mutate(predelta.date = date, 
         predelta.fullvax = fullvax.total) %>%
  select(Town, predelta.fullvax, predelta.date) 

town.vax <- left_join(town.vax, vaxsincejuly, by = c("Town")) %>%
  # select(date, Town, town_population, fullvax.total, predelta.fullvax, predelta.date, 
  #        vaccine_prev, quartile.SVI_SES, blacklatinx.ord, xname, yname ) %>%
  mutate(vaxsincejuly = fullvax.total - predelta.fullvax,
         vaxsincejuly.pct = vaxsincejuly/town_population, 
         vaxsincejuly.pct = if_else(vaxsincejuly.pct <=0, 0, vaxsincejuly.pct))


#make geographic dataset
townsgeobos<-read.csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/townsgeo_onlybostonneigh.csv")
townsgeo<-read.csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(townsgeo, townsgeobos)


TownVaxGeo <-  left_join(matownsgeo, town.vax, by = "Town")



YlOrRd<-c(  "#feb24c","#fd8d3c", "#f03b20", "#bd0026")
jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")
MAmeanVIR <- ((town.vax %>% tally(fullvax.total)) / (town.vax %>% tally(Count)))$n

VIR<-ggplot(town.vax %>% filter(town_population >15000), aes(x=reorder(Town, SVI_SES),
            y=VIR, fill=as.factor(quartile.SVI_SES)))+
  geom_col()+ theme_classic() +
  scale_fill_manual(values=jama, name="Socioeconomic Vulnerabilty\n(CDC SVI Percentile)",
                    labels=c("Low", "Low to Moderate","Moderate to High",  "High"))  +
  theme(legend.position = c(.8, .95), legend.justification = c(1, 1),
        legend.direction = "vertical",axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_hline(yintercept = MAmeanVIR, linetype="dashed") +
  annotate("text", x=148, y=MAmeanVIR+0.2, label=paste0("Massachusetts mean: ", round(MAmeanVIR, 2)), hjust=1)+
  labs(x="Communities (ordered by vulnerabilty)", y="Vaccination to Infection Risk Ratio",
       title="Alignment of Vaccination to Infection Risk in Massachusetts Communitities",
       subtitle = "Towns and Boston neighborhoods with population greater than 15,000 | Vaccination-to-Infection Risk Ratio = Cumulative incidence of complete vaccination / Cumulative incidence of confirmed Covid-19",
       caption="Calculated from Covid-19 incidence and vaccination data from the Massachusetts\nDepartment of Public Health and Boston Public Health Commisssion")
VIR
ggsave("Town VIR by SVI annotated.pdf", units = "in", width = 18, height=8)

# #mean VIR for MA
# MAmeanVIR <- ((town.vax %>% tally(fullvax.total)) / (town.vax %>% tally(Count)))$n
# 
# ggplot(town.vax %>% filter(population >25000), aes(x=reorder(Town, SVI_SES), 
#                                                         y=VIR, fill=as.factor(quartile.SVI_SES)))+
#   geom_col()+ theme_classic() + 
#   scale_fill_manual(values=jama, name="Socioeconomic Vulnerabilty\n(CDC SVI Percentile)",
#                     labels=c("Low", "Low to Moderate","Moderate to High",  "High"))  +
#   theme(legend.position = c(.95, .95), legend.justification = c(1, 1), 
#         legend.direction = "vertical",axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
#   geom_hline(yintercept = MAmeanVIR, linetype="dashed") +
#   annotate("text", x=93, y=MAmeanVIR+0.2, label=paste0("Massachusetts mean: ", round(MAmeanVIR, 2)), hjust=1)+
#   labs(x="Communities (ordered by vulnerabilty)", y="Vaccination to Infection Risk Ratio")
# ggsave("Town VIR ratio by SVI.pdf", units = "in", width = 10, height=8)

# 
# 
# write_csv(town.vax %>% dplyr::select(Town, date, population, Count, fullvax.total, SVI_SES, age65up.pct,
#                                      SVI_overall, blacklatinx.pct, VIR, vaccine_prev, cumulative_incidence) %>% rename(cum.covid.cases = Count, 
#                                         cum.fullvax.indiv=fullvax.total,
#                                         VIRratio=VIR), "20210408 MA vaccine and covid dataset.csv"
# )
# 
# 
# #MAPS of VIR
# 
# 
# ggplot()+
#   geom_polygon(data = TownVaxGeo %>% filter(Town %in% metrotowns),  
#                aes(x = long, y = lat, group = group, fill=VIR), 
#                colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ 
#   scale_fill_distiller(palette='YlOrRd', direction=-1, breaks=c(0, 2, 4, 6),
#                        limits=c(0,7), na.value = "#ffffcc",
#                        name="Vaccination to\nNeed Ratio (VIR)")  +
#   labs(title="Alignment of Vaccination to Need, Metro-Boston",
#        subtitle=paste0("Vaccinations through ", last(as.Date(TownVaxGeo$date)), " | Vaccination-to-Need Ratio = Cumulative incidence of complete vaccination / Cumulative incidence of confirmed Covid-19"), 
#        caption="Calculated from Covid-19 incidence and vaccination data from the Massachusetts\nDepartment of Public Health and Boston Public Health Commisssion")+
#   geom_label( data = incid.metroboston %>%
#                 distinct(Town, .keep_all = TRUE), 
#               size=1.5, color="#374E55FF",
#               aes(x=xname, y=yname,  label=paste0(Town)))
# ggsave("VIR MetroBoston.pdf", width=14, height=10)


#MAPs of vaccination

TownVaxLabels <- TownVaxGeo %>% distinct(Town, .keep_all = TRUE) %>%
                    dplyr::select(xname, yname, Town, fullvaccine_prev, boostvaccine_prev)


metroboston.vax<- ggplot()+
  geom_polygon(data = TownVaxGeo, 
               aes(x = long, y = lat, group = group, fill=fullvaccine_prev), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = TownVaxLabels,
             size=1.5,
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(fullvaccine_prev,1), "%)", sep="")))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "#800026",
                       limits=c(55, 90),breaks = c(60, 75, 90),
                       name="Percent with\nComplete\nVaccination") +
  labs(title="SARS-CoV-2 Vaccination in Metro Boston",
       subtitle = paste0("Last reported data ",max(town.vax$date)),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.7) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
#  coord_cartesian(xlim=c(-71.8, -70.7), ylim = c(42.15, 42.65))
metroboston.vax
ggsave("metroboston.vax.pdf", width=16, height = 12)


metroboston.boostvax<- ggplot()+
  geom_polygon(data = TownVaxGeo %>% mutate(boostvaccine_prev= case_when(
    boostvaccine_prev <20 ~ 19, 
    boostvaccine_prev >80 ~ 81,
    boostvaccine_prev >= 20 & boostvaccine_prev <= 80 ~ boostvaccine_prev
  )), 
               aes(x = long, y = lat, group = group, fill=boostvaccine_prev), 
               colour = "white", alpha = 0.9, size = .25) +
  geom_text( data = TownVaxLabels,
             size=1.5,
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(boostvaccine_prev,1), "%)", sep="")))+
  coord_quickmap() + theme_void() + 
  # geom_label_repel(data =incid.metroboston %>% filter(Count1wk>250) %>%
  #                    distinct(Town, xname, yname, incidence1day, .keep_all = TRUE), size=2.2, min.segment.length = 0,
  #                  aes(x=xname, y=yname,  label=paste(Town,"\n", round(Count1wk,0), " cases", sep="")), nudge_x=0.1)+
  scale_fill_distiller(palette='YlOrRd', direction=1,na.value = "light gray", # na.value = "#ffffcc",
                       limits=c(19, 81),breaks = c(20, 40, 60, 80),
                       name="Percent with\nBoosted\nVaccination") +
  labs(title="SARS-CoV-2 Boosted Vaccination in Metro Boston",
       subtitle = paste0("Last reported data ",max(town.vax$date)),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.7) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))
#  coord_cartesian(xlim=c(-71.8, -70.7), ylim = c(42.15, 42.65))
metroboston.boostvax
ggsave("metroboston.boostvax.pdf", width=16, height = 12)

MA.vax<- ggplot()+
  geom_polygon(data = TownVaxGeo %>%
                 mutate(vaccine_prev = if_else(fullvaccine_prev >80, 80, fullvaccine_prev), 
                        vaccine_prev = if_else(fullvaccine_prev <40, 40, fullvaccine_prev),
                        vaccine_prev = if_else(fullvaccine_prev == 0, as.numeric(NA), fullvaccine_prev)), 
               aes(x = long, y = lat, group = group, fill=fullvaccine_prev), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light gray", ##ffffcc",
                       limits=c(40, 80),breaks = c(40, 60, 80),
                       name="Percent with\nComplete\nVaccination") +
  labs(title="SARS-CoV-2 Vaccination in Massachusetts",
       subtitle = paste0("Last reported data ", max(town.vax$date)),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.3, .3), legend.justification = c(1, 1),
        legend.direction = "horizontal")
MA.vax
ggsave("MA.vax.pdf",  width=16, height = 12)

MA.boostvax<- ggplot()+
  geom_polygon(data = TownVaxGeo %>% mutate(boostvaccine_prev= case_when(
                 boostvaccine_prev <20 ~ 19, 
                 boostvaccine_prev >80 ~ 81,
                 boostvaccine_prev >= 20 & boostvaccine_prev <= 80 ~ boostvaccine_prev)), 
               aes(x = long, y = lat, group = group, fill=boostvaccine_prev), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light gray", ##ffffcc",
                       limits=c(19, 81),breaks = c(20, 40, 60, 80),
                       name="Percent with\nBoosted\nVaccination") +
  labs(title="SARS-CoV-2 Boosted Vaccination in Massachusetts",
       subtitle = paste0("Last reported data ", max(town.vax$date)),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.3, .3), legend.justification = c(1, 1),
        legend.direction = "horizontal")
MA.boostvax
ggsave("MA.boostvax.pdf",  width=16, height = 12)




MA.vaxsincejuly<- ggplot()+
  geom_polygon(data = TownVaxGeo %>% 
    mutate(vaxsincejuly.pct = if_else(is.na(vaxsincejuly.pct) | vaxsincejuly.pct < 0.05, 5, vaxsincejuly.pct*100)), 
               aes(x = long, y = lat, group = group, fill=vaxsincejuly.pct), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  scale_fill_distiller(palette='PuRd', direction=1, na.value = "#67001f",
                       limits=c(5, 12),breaks = c(5, 10),
                       name="Percent becoming\nfully vaccinated") +
  labs(title="SARS-CoV-2 Vaccination during Delta Wave, Massachusetts",
       subtitle = paste0("Completed vaccination since July 2021 | Last reported data ", max(town.vax$date)),
       caption="Source: BPHC and MDPH") +
  theme(legend.position = c(.3, .3), legend.justification = c(1, 1),
        legend.direction = "horizontal") +
  geom_label_repel( data = TownVaxGeo %>% distinct(Town, .keep_all = TRUE) %>%
                filter(vaxsincejuly.pct>.12 & town_population>4000),
              aes(x=xname, y=yname,  label=paste0(Town, "\n", round(vaxsincejuly.pct*100, 1), "%")), 
              min.segment.length = 0, max.overlaps = 40,
              size=2.5, color="#374E55FF")
MA.vaxsincejuly
ggsave("MA.vaxdelta.pdf",  width=16, height = 12)
s

#MAPS of herd immunity

#MAPS of herd immunity


ggplot()+
  geom_polygon(data = TownVaxGeo %>% mutate(herd = if_else(herd>90, 90, herd)) %>% 
                 filter(Town %in% metrotowns),
               aes(x = long, y = lat, group = group, fill=100-herd),
               colour = "grey", alpha = 0.9, size = 0.15) +
  coord_map() + theme_void()+
  scale_fill_distiller(palette='YlOrRd', direction=1, breaks=c(30, 40, 50, 60, 70),
                       limits=c(0,50), #na.value = "#ffffcc",
                       name="Percent of\nPopulation")  +
  labs(title="Estimated Non-Immune Population, Metro-Boston",
       subtitle=paste0("Fully-vaccinated + unvacciated * estimated prevalence of prior infection (2 times confirmed cases) | last data:  ", last(as.Date(TownVaxGeo$date))),
       caption="Calculated from Covid-19 incidence and vaccination data from the Massachusetts\nDepartment of Public Health and Boston Public Health Commisssion") +
  geom_label( data = incid.metroboston %>%
                distinct(Town, .keep_all = TRUE),
              size=1.5, color="#374E55FF",
              aes(x=xname, y=yname,  label=paste0(Town)))
ggsave("Non-immune MetroBoston.pdf", width=14, height=10)

ggplot()+
  geom_polygon(data = TownVaxGeo %>% mutate(herd = if_else(herd>90, 90, herd)),
               aes(x = long, y = lat, group = group, fill=100-herd),
               colour = "grey", alpha = 0.9, size = 0.15) +
  coord_map() + theme_void()+
  scale_fill_distiller(palette='YlOrRd', direction=1, breaks=c(0,  25, 50, 75),
                        limits=c(0,50), #na.value = "#ffffcc",
                       name="Percent of\nPopulation")  +
  labs(title="Estimated Non-Immune Population, Massachusetts",
       subtitle=paste0("Fully-vaccinated + unvacciated * estimated prevalence of prior infection (2 times confirmed cases) | last data:  ", last(as.Date(TownVaxGeo$date))),
       caption="Calculated from Covid-19 incidence and vaccination data from the Massachusetts\nDepartment of Public Health and Boston Public Health Commisssion")
ggsave("Non-immune Massachusetts.pdf", width=14, height=10)

# 
# 
# ggplot()+
#   geom_polygon(data = TownVaxGeo ,  
#                aes(x = long, y = lat, group = group, fill=VIR), 
#                colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ 
#   scale_fill_distiller(palette='YlOrRd', direction=-1, breaks=c(0, 2, 4, 6),
#                        limits=c(0,7), na.value = "#ffffcc",
#                        name="Vaccination to\nNeed Ratio (VIR)")  +
#   labs(title="Alignment of Vaccination to Need, Massachusetts",
#        subtitle=paste0("Vaccinations through ", last(as.Date(TownVaxGeo$date)), " | Vaccination-to-Need Ratio = Cumulative incidence of complete vaccination / Cumulative incidence of confirmed Covid-19"), 
#        caption="Calculated from Covid-19 incidence and vaccination data from the Massachusetts\nDepartment of Public Health and Boston Public Health Commisssion")
# ggsave("VIR Massachusetts.pdf", width=14, height=10)
# 


# write_csv(town.vax %>%
#             select(date, Town, Count, fullvax.total, vaccine_prev, cumulative_incidence, VIR, population,
#                    SVI_SES:SVI_overall, cumulative_incidence, population), "20210422 MA town vaccination.csv")
library(ggrepel)

VIRplot.data<-left_join(town.vax, allcovidtowns %>% group_by(Town) %>%
            mutate(case7day = Count- lag(Count, 1)) %>%
            ungroup() %>%
            filter(date== max(date)) %>%
            select(Town, case7day), by = "Town") %>%
            mutate(incidence1day= (case7day*100000)/(town_population *7)) %>%
            select(date, Town, VIR, incidence1day, town_population, SVI_SES) %>%
            filter(town_population>5000) %>%
            mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                     labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)",
                              "Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)")))
VIRplot <- VIRplot.data %>%
  ggplot(aes(x=VIR, y=incidence1day)) +
  geom_smooth(span=1, se= FALSE, color="black",  size=2)+
  geom_point(aes(size=town_population, fill=SES.cat), shape=21) +
  theme_light() +
  coord_cartesian(xlim =c(0,25), ylim=c(0,60)) +
  guides(size=FALSE) +
  scale_fill_manual(values=jama, name="Socioeconomic SVI\nDomain Percentile")  +
  theme(legend.position = c(.8, .8),
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        legend.background = element_rect(fill = "White", size=1, color="Black")) +
  geom_label_repel(data=VIRplot.data %>%
                     filter(town_population>60000 |
                              incidence1day>30 | Town == "East Boston" | Town == "Jamaica Plain" |
                            Town == "Chelsea" | Town == "Mattapan" | VIR <5 & incidence1day>25 |
                              VIR >17), aes(x=VIR, y=incidence1day, 
                                label= Town),  size=2.5,
                   min.segment.length = 0, max.overlaps = 40)+
  labs(title= "Vaccine to Infection Risk and Covid-19 Incidence, Massachusetts",
       subtitle=paste0("Data current as of ", max(town.vax$date-2), " | Communities with population 5000 or greater"),
       x="Vaccine to Infection Risk (VIR) Ratio",
       y="Daily Incidence (per 100,000)") 
VIRplot
ggsave("VIRplot.pdf", width=12, height=8)


town.vax %>%
  filter(!is.na(vaccine_prev)) %>%
  filter(town_population>5000) %>%
  mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                     labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
  filter(!is.na(SES.cat))%>%
  ggplot(aes(y=vaccine_prev, x=cumulative_incidence, size=town_population)) +
  geom_point(aes(fill=SES.cat), shape=21) +
  geom_smooth(method=lm, color="#B24745FF", fill=NA)+
  theme_light() +
  labs(title= "Alignment of Covid-19 Burden, SES, and Vaccination in Massachusetts Communties",
       subtitle=paste0("Data current as of ", max(town.vax$date-2), " | Communities with population 5000 or greater"),
       x="Cumulative Covid-19 Incidence (percent)",
       y="Proportion of Community Fully-Vaccinated (percent)") +
  guides(size=FALSE) +
  geom_label_repel(data=town.vax %>%
                     mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                                        labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
                     filter(!is.na(SES.cat))%>%
                     filter(!is.na(vaccine_prev)) %>%
                     filter(town_population>10000) %>%
                     filter(cumulative_incidence<10 & vaccine_prev>60 | cumulative_incidence>15 |vaccine_prev < 40 |
                              town_population>40000 |Town=="Roxbury" |Town=="Dorchester Uphams"),
                   aes(x=cumulative_incidence, y=vaccine_prev,  label=Town, color=SES.cat),
                   min.segment.length = 0) +
  coord_cartesian(xlim=c(0,30), ylim=c(0,100))+
  scale_fill_manual(values=jama, name="Socioeconomic SVI\nDomain Percentile")  +
  scale_color_manual(values=jama, name="Socioeconomic SVI\nDomain Percentile")  +
  guides(color=FALSE) +
  theme(legend.position = c(.8, .8),
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        legend.background = element_rect(fill = "White", size=1, color="Black"))

ggsave("vaccine_incidenceplot.pdf", width = 12, height=12)

jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")
p <-town.vax.long %>% 
  filter(!is.na(vaccine_prev)) %>%
  filter(town_population>5000) %>%
  mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                     labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
  filter(!is.na(SES.cat))%>%
  ggplot(aes(y=vaccine_prev, x=cumulative_incidence, size=town_population)) +
  geom_point(aes(fill=SES.cat), shape=21) +
  #geom_smooth(method=lm, color="#B24745FF", fill=NA)+
  theme_light() +
  geom_abline(intercept = 70, slope = -2) +
  labs( #title= "Alignment of Covid-19 Burden, SES, and Vaccination in Massachusetts Communties",
  #      subtitle=paste0("Data current as of ", max(town.vax.long$reportdate-2), " | Communities with population 5000 or greater"),
        x="Cumulative Covid-19 Incidence (percent)",
       y="Proportion of Community Fully-Vaccinated (percent)") +
  guides(size=FALSE) +
  geom_label(data= town.vax.long %>% filter(!is.na(vaccine_prev)) %>%
               filter(town_population>5000) %>%
               mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                                  labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
               filter(!is.na(SES.cat))%>%
                     filter(Town=="Roxbury" |Town=="Dorchester Uphams" | Town=="Cambridge" | Town=="Brookline" |
                              Town=="Newton" | Town=="Lawrence" | Town=="Springfield" | Town=="New Bedford"|
                              Town=="Worcester" | Town=="Barnstable" | Town=="Needham"
                            | Town=="Hyde Park" | Town=="Chelsea" | Town=="Lynn" | Town=="Mattapan"
                            | Town== "Dorchester Uphams" |Town=="Lowell" |Town=="Lynn"),
                   aes(x=cumulative_incidence, y=vaccine_prev,  label=Town, color=SES.cat),
                   nudge_x = -0, nudge_y = -2, size=2) +
  coord_cartesian(xlim=c(0,35), ylim=c(0,100))+
  scale_fill_manual(values=jama, name="Socioeconomic\nVulnerabilty (CDC SVI)")  +
  scale_color_manual(values=jama, name="Socioeconomic SVI\nDomain Percentile")  +
  guides(color=FALSE) +
  theme(legend.position = c(.8, .8),
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        legend.background = element_rect(fill = "White", size=1, color="Black"))

p

anim<- p + transition_time(reportdate) +
  labs(title = "Date: {frame_time}") 

animate(anim, height = 6, width = 6, units = "in", res = 200)

anim_save("vaccine to risk.gif")

jama<-c("#80796BFF","#DF8F44FF", "#B24745FF")
  
p <-town.vax.long %>% 
  filter(!is.na(vaccine_prev)) %>%
  filter(town_population>5000) %>%
  filter(Town %in% boston) %>%
  mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                     labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
  filter(!is.na(SES.cat))%>%
  ggplot(aes(y=vaccine_prev, x=cumulative_incidence, size=town_population)) +
  geom_point(aes(fill=SES.cat), shape=21) +
  geom_abline(intercept = 70, slope = -2) +
 # geom_smooth(method=lm, color="#B24745FF", fill=NA)+
  theme_light() +
  labs( #title= "Alignment of Covid-19 Burden, SES, and Vaccination in Massachusetts Communties",
    #      subtitle=paste0("Data current as of ", max(town.vax.long$reportdate-2), " | Communities with population 5000 or greater"),
    x="Cumulative Covid-19 Incidence (percent)",
    y="Proportion of Community Fully-Vaccinated (percent)") +
  guides(size=FALSE) +
  geom_label(data= town.vax.long %>% filter(!is.na(vaccine_prev)) %>%
               filter(town_population>5000) %>%
               mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.25, 0.5, 0.75, Inf),
                                  labels=c("Low\n(0-25 percentile)", "Low to Moderate\n(25-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
               filter(!is.na(SES.cat))%>%
               filter(Town %in% boston),
             aes(x=cumulative_incidence, y=vaccine_prev,  label=Town, color=SES.cat),
             nudge_x = -0, nudge_y = -2, size=2) +
  coord_cartesian(xlim=c(0,35), ylim=c(0,100))+
  scale_fill_manual(values=jama, name="Socioeconomic\nVulnerabilty (CDC SVI)")  +
  scale_color_manual(values=jama, name="Socioeconomic SVI\nDomain Percentile")  +
  guides(color=FALSE) +
  theme(legend.position = c(.85, .75),
        legend.direction = "vertical", legend.key.size = unit(0.4, "in"), legend.text = element_text(size=10),
        legend.background = element_rect(fill = "White", size=1, color="Black"))



anim<- p + transition_time(reportdate) +
  labs(title = "Date: {frame_time}") 

animate(anim, height = 6, width = 6, units = "in", res = 200)

anim_save("vaccine to risk, Boston.gif")

# 
# ## composite plots
# 
# jama<-c( "#80796BFF","#DF8F44FF", "#B24745FF")
# p1<-ggplot()+
#   geom_polygon(data = TownVaxGeo %>% 
#                  mutate(SVI_SES=if_else(is.na(SVI_SES),0, SVI_SES)),  
#                aes(x = long, y = lat, group = group, 
#                    fill=cut(SVI_SES, breaks=c(-1, 0.5, 0.75,  Inf),
#                             labels=c("Low to Mod", "Mod to High",  "High" ) )
#                ), colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ theme(legend.position = c(.85, .6))+
#   scale_fill_manual(values=jama, name="Socioeconomic\nVulnerability")+
#   coord_cartesian(xlim=c(-71.509, -70.586), ylim=c(42.065,42.643))
# 
# p2<- ggplot()+
#   geom_polygon(data = TownVaxGeo %>% 
#                  mutate(cumulative_incidence=if_else(is.na(cumulative_incidence),0, cumulative_incidence)),  
#                aes(x = long, y = lat, group = group, 
#                    fill=cut(cumulative_incidence, breaks=c(-1, 5, 10,  Inf),
#                             labels=c("0 to 5%", "5 to 10%",  "> 10%" ) )
#                ), colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ theme(legend.position = c(.85, .6))+
#   scale_fill_manual(values=jama, name="Cumulative Incidence\nof Confirmed Covid-19")+
#   coord_cartesian(xlim=c(-71.509, -70.586), ylim=c(42.065,42.643))
# 
# p3<- ggplot()+
#   geom_polygon(data = TownVaxGeo %>% 
#                  mutate(vaccine_prev=if_else(is.na(vaccine_prev),0, vaccine_prev)),  
#                aes(x = long, y = lat, group = group, 
#                    fill=cut(vaccine_prev, breaks=c(-1, 25, 30,  Inf),
#                             labels=c("< 20%", "20 to 25%", "> 25%" ) )
#                ), colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ theme(legend.position = c(.85, .6))+
#   scale_fill_manual(values=jama, name="Cumulative\nSARS-CoV-2\nVaccination")+
#   coord_cartesian(xlim=c(-71.509, -70.586), ylim=c(42.065,42.643))
# 
# p4<-ggplot()+
#   geom_polygon(data = TownVaxGeo %>% 
#                  mutate(VIR=if_else(is.na(VIR),0, VIR)),  
#                aes(x = long, y = lat, group = group, 
#                    fill=cut(VIR, breaks=c(-1, 3, 6,  Inf),
#                             labels=c("0 to 3", "3 to 6", "> 6" ) )
#                ), colour = "grey", alpha = 0.9, size = 0.15) +
#   coord_map() + theme_void()+ theme(legend.position = c(.85, .6))+
#   scale_fill_manual(values=jama, name="Vaccine-to-urgency\nratio")+
#   coord_cartesian(xlim=c(-71.509, -70.586), ylim=c(42.065,42.643))
# 
# 
# 
# library(patchwork)
# 
# p1 + p2 + p3 + p4 + plot_layout(ncol=2) + plot_annotation(tag_levels = 'A')
# 
# ggsave("SES Covid VIR.pdf", width=12, height = 12)
# 
# 
# 
# #vaccine data MDPH https://www.mass.gov/info-details/covid-19-vaccination-program#weekly-covid-19-vaccination-report-
# 
# vaccine1 <-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
#   mutate(date=lubridate::ymd(date),
#          weekly.fullvax=full.vax.All-lag(full.vax.All,1)) %>%
#   dplyr::select(date, weekly.fullvax)
# 
# MAweekly<-MassCovid1 %>%
#   group_by(date) %>%
#   dplyr::summarise(MAcases=sum(cases)) %>%
#   mutate(MAweeklycases=MAcases-(lag(MAcases,7)),
#          CDCestMAweeklycases=round(4.6*MAweeklycases,0), 
#          CDCestLBcases=round(4*MAweeklycases, 0), 
#          CDCestUBcases=round(5.4*MAweeklycases,0)
#   )
# 
# vaccine<- full_join(MAweekly, vaccine1, by="date") 
# 
# MApopulation<-tribble(             #https://sites.tufts.edu/jamesjennings/files/2018/06/reportsBlackComparativeExperience2015.pdf
#   ~group, ~pop, ~label,
#   "AI.AN", 11600, "American Indian\nAlaska Native",
#   "Multi", 128000, "Multi-Racial",
#   "NH.PI", 2700, "Native Hawaiian\nPacific Islander",
#   "Asian", 492900, "Asian",
#   "Black", 509200, "Black",
#   "Hispanic", 859100, "Latinx",
#   "White", 4955500,  "White" 
# )
# vaccine2 <- left_join(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
#                         mutate(date=lubridate::ymd(date),
#                                weekly.fullvax=full.vax.All-lag(full.vax.All,1)) %>%
#                         dplyr::select(date, Asian, Black, Hispanic, AI.AN, White, NH.PI, Multi) %>%
#                         pivot_longer(!date, names_to = "group", values_to = "fullvaccinated"),
#                       MApopulation, by="group") %>%
#   mutate(vaccine_incidence=fullvaccinated*100/pop)
# 
library(ggsci)
vac1<-vaccine2 %>%
  filter(!is.na(vaccine_incidence)) %>%
  ggplot()+
  geom_line(aes(x=date, y=vaccine_incidence, group=as.factor(group), color=as.factor(group)), size=2)+
  theme_classic()+ theme(aspect.ratio = 1)+
  scale_colour_jama(palette = c("default"), alpha = 0.9)+
  labs(x="Date", y="Proportion of Estimated Population",
       title="SARS-CoV-2 Vaccination by Racial/Ethnic Group\nin Massachusetts | Fully-Vaccinated Individuals",
       caption="Source: MDPH utilizing population estimates from the Donahue Center at UMass")+
  guides(color=FALSE) +
  coord_cartesian(xlim = c(as.Date("2021-02-12"), as.Date(last(vaccine2$date)+30)),
                  ylim=c(0, NA))+
  scale_x_date(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  geom_label_repel(data=vaccine2 %>% filter(date == last(date)),
                   aes(label=paste0(label, "\n", round(vaccine_incidence,1),"%"), x=date, y = vaccine_incidence, color=as.factor(group)) ,
                   min.segment.length = 0,
                   xlim=c(Sys.Date(), as.Date(NA))
  )
# 
# MApopulationage<-tribble(            #https://www.infoplease.com/us/census/massachusetts/demographic-statistics
#   ~group, ~pop, ~label,
#   "a0to19", 1588300, "0 to 19",
#   "a20to29", 1026800, "20 to 29",
#   "a30to49", 1756000, "30 to 49",
#   "a50to64", 1417200, "50 to 64",
#   "a65to74", 682800,  "65 to 74", 
#   "a75over", 493300,  "75+",
# )
# 
# vaccine3 <- left_join(read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine")%>%
#                         mutate(date=lubridate::ymd(date)) %>%
#                         dplyr::select(date, a0to19, a20to29, a30to49, a50to64, a65to74, a75over) %>%
#                         pivot_longer(!date, names_to = "group", values_to = "fullvaccinated"),
#                       MApopulationage, by="group") %>%
#   mutate(vaccine_incidence=fullvaccinated*100/pop) %>%
#   filter(!is.na(vaccine_incidence))
# 
# vac2<-vaccine3 %>%
#   ggplot()+
#   geom_line(aes(x=date, y=vaccine_incidence, group=as.factor(group), color=as.factor(group)), size=2)+
#   theme_classic()+ theme(aspect.ratio = 1)+
#   scale_colour_jama(palette = c("default"), alpha = 0.9)+
#   labs(x="Date", y="Percent of Estimated Population",
#        title="SARS-CoV-2 Vaccination by Age Group\nin Massachusetts | Fully-Vaccinated Individuals",
#        caption="Source: MDPH utilizing population estimates the from Donahue Center at UMass")+
#   guides(color=FALSE) +
#   coord_cartesian(xlim = c(as.Date("2021-02-12"), as.Date(last(vaccine3$date)+30)),
#                   ylim=c(0, NA))+
#   scale_x_date(breaks = scales::pretty_breaks(8)) +
#   scale_y_continuous(breaks = scales::pretty_breaks(10)) +
#   geom_label_repel(data=vaccine3 %>% filter(date == last(date)), 
#                    aes(label=paste(label, "years\n", round(vaccine_incidence,1),"%"), x=date, y = vaccine_incidence, color=as.factor(group)) , 
#                    min.segment.length = 0,
#                    xlim=c(Sys.Date(), as.Date(NA))
#   )
# 
# library(patchwork)
# vac2 + vac1 + plot_layout(ncol=2)
# ggsave("vaccinationgroups.pdf", width =16, height=10)
# 
# vac1
# ggsave("~/Dropbox (Partners HealthCare)/R files/covid/vaccine by race.jpg", plot = last_plot(), device = "jpeg",
#        scale = 1,  units = c("in"),height = 8.67, width = 8.67,
#        dpi = 160, limitsize = TRUE)


# 
# 
# library(officer)
# 
# slides <- read_pptx("wide template.pptx")
# slides <- add_slide(slides)
# slides <- ph_with(slides, MA.incidence, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, v1, 
#                  location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, variant.incidence, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, v2, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, vaccine.doses, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, vaccinationgroups, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, VIR, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, metroboston, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, allma, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, metroboston.vax, 
#                   location = ph_location_fullsize() )
# 
# slides <- add_slide(slides)
# slides <- ph_with(slides, MA.vax, 
#                   location = ph_location_fullsize() )
# 
# print(slides, target = "20210631 MGH Town Hall.pptx")
# 
# 
