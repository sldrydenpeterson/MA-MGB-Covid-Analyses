library(tidyverse)
library(ggrepel)
library(viridis)
library(cowplot)
library(ggmap)
library(patchwork)

#read in .csv files of zipcodes and population
MAtowns <- read_csv("MAtowns_no_bosneighborhoods.csv")  #reading in with consolidated Boston
MAtownpoptable <- read_csv("MAtownpoptable.csv")

#read in updating (twice weekly) .csv files of cases/testing
# allcovidtowns <- read_csv("allcovidtowns.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26")) %>% # MDPH town testing data avial from 2020-05-27
#   mutate(date=(lubridate::ymd(date)))

allcovidtowns <- read_csv("allcovidtowns_no_bosneighborhoods.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26")) %>% # MDPH town testing data avaiable from 2020-05-27
  mutate(date=(lubridate::ymd(date)))

#Adding town_population to allcovidtowns
covidtowns1<-left_join(allcovidtowns, MAtownpoptable, by="Town")

#sorting by town and date, and incident cases in past week
covidtowns<-covidtowns1 %>% 
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% 
  mutate(Rate=(Count/town_population)*100000,
         Count1wk=Count-(lag(Count,1)),
         Count1wk=if_else(Count1wk<0, 0, Count1wk),
         incidence7day=Rate-lag(Rate,1), 
         incidence7day=if_else(incidence7day<0, 0, incidence7day), #some instances where cases removed in subsequent weeks (?changed patient residence info, errors)
         incidence1day=incidence7day/7,
         Tests1wk=Tests-(lag(Tests,1)),
         positivity1wk=ifelse(Tests1wk>0, Count1wk/Tests1wk, NA), #preventing NaN when Tests1wk are zero
         testingrate=100000*Tests1wk/town_population) %>%
  filter(date > as.Date("2020-05-27") & date < as.Date("2020-10-15"))  #first row is NA for all as calculations lagged 1 week
#allcovidtowns.long<-covidtowns #make a dataset with all observations


#getting overall testing statistics for MA
ALL.MA.testing<-covidtowns %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by()%>%
  dplyr::summarise(
    Tests.AllMA=sum(Tests1wk),
    Count.AllMA=sum(Count1wk),
    Positivity.AllMA=Count.AllMA/Tests.AllMA
  )

ALL.MA.testing

#community testing summary
town.summary<-covidtowns %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by(Town)%>%
  dplyr::summarise(
    Tests.towns=sum(Tests1wk),
    Count.towns=sum(Count1wk),
    Testing.towns=(sum(Tests1wk)/mean(town_population))*100000,
    Incidence.towns=(sum(Count1wk)/mean(town_population))*100000,
    Positivity.towns=Count.towns/Tests.towns) %>%
  group_by() %>%
  dplyr::summarise(
    median.testing=median(Testing.towns),
    lower.iqr.testing=quantile(Testing.towns, .25),
    upper.iqr.testing=quantile(Testing.towns, .75),
    min.testing=min(Testing.towns),
    max.testing=max(Testing.towns),
    median.positivity=median(Positivity.towns),
    lower.iqr.positivity=quantile(Positivity.towns, .25),
    upper.iqr.positivity=quantile(Positivity.towns, .75),
    min.positivity=min(Positivity.towns),
    max.positivity=max(Positivity.towns),
    min.incidence=min(Incidence.towns),
    max.incidence=max(Incidence.towns),
    median.incidence=median(Incidence.towns)
  )
town.summary

totalpop<-MAtownpoptable %>%
  dplyr::summarise(pop=(sum(town_population)))

testingmatch<-covidtowns %>%
  distinct(Town, date, .keep_all = TRUE) %>%
  group_by(date) %>%
  mutate(
    rankpositivity= dense_rank(desc(positivity1wk)),
    ranktesting= dense_rank(desc(testingrate)),
    testaccess.index=(rankpositivity-ranktesting)/n(),
    index.pct=cume_dist(testaccess.index),
    match=as.factor(if_else(index.pct <0.2, "Low testing mismatch", 
                            (if_else(index.pct >0.8, "High testing mismatch", "Relative match")) )),
    mismatch=abs(testaccess.index),
    pop.w=town_population/totalpop$pop, #population from totalpop above
    mismatch.w=mismatch * pop.w *n(),
    n=n(),
    index.pct=cume_dist(testaccess.index)
  ) %>%
  dplyr::select(Town, date, rankpositivity:n, positivity1wk, town_population, testingrate, Tests1wk, Count1wk)


#add testing target (testing rate for rankpositivity for that week)
testing.target<-inner_join(testingmatch %>%
                             dplyr::select(date, Town, rankpositivity, ranktesting, testingrate) %>%
                             filter(!is.na(rankpositivity) & !is.na(testingrate)), 
                           testingmatch %>% 
                             filter(!is.na(rankpositivity) & !is.na(testingrate)) %>%
                             rename(testingrate.target=testingrate) %>%
                             dplyr::select(date, testingrate.target, ranktesting),
                           by=c("date" ="date" , "rankpositivity"="ranktesting")
                        )

testingmatch<- left_join(testingmatch, testing.target  %>% 
                       dplyr::select(date, Town, testingrate.target), by= c("date", "Town")) %>%
  mutate(
    testingrate.target.cap=if_else(testingrate.target>10000, 10000, testingrate.target), #capping at 10% of population to reduce distortion of collegetown testing
    testingrate.delta=testingrate.target.cap-testingrate,
    testingrate.gap=if_else(testingrate.delta<0, 0, testingrate.delta), #excess testing set to 0, no benefit from earlier/later testing
    testingrate.excess=if_else(testingrate.delta<0, abs(testingrate.delta), 0),
    testingrate.excess=if_else(testingrate.excess>10000, 10000, testingrate.excess), #capping at 10% of population to reduce distortion of collegetown testing
    testing.gap=testingrate.gap*(town_population/100000),
    testing.excess=testingrate.excess*(town_population/100000),
    week=lubridate::week(date))

#creating dataset of mean mismatch over data period (june to present)
testingmatch.means <- testingmatch %>%
  filter(date >= as.Date("2020-06-03")) %>%
  mutate(testingrate.gap=ifelse(testingrate.gap>0, testingrate.gap, 0)) %>%
  group_by(Town) %>%
  dplyr::summarize(testaccess.mean=mean(testaccess.index, na.rm = TRUE),
                   testingrate.gap.mean=mean(testingrate.gap, na.rm = TRUE), 
                   testingrate.excess.mean=mean(testingrate.excess, na.rm = TRUE),
                   town_population=mean(town_population, na.rm = TRUE),
                   cum.cases=sum(Count1wk),
                   cum.incid.cases=(cum.cases/town_population)*100000,
                   cum.tests=sum(Tests1wk),
                   cum.incid.test=(cum.tests/town_population)*100000,
                   cum.incid.tests.mean=(mean(Tests1wk)/town_population)*100000
  ) %>%
  mutate(quintile.testaccess.mean=ntile(testaccess.mean, 5))

#creating dataset of weekly mean mismatch over data period (june to present)
testingmatch.weekly<- testingmatch %>% filter(date >= as.Date("2020-06-03")) %>%
  filter(!is.na(mismatch)) %>%
  group_by(date)%>%
  dplyr::summarise(sum=sum(pop.w), #check on weightings, should be ~1
                   number=n(), 
                   average=mean(mismatch.w),
                   mean.testing.gap=mean(testing.gap),
                   mean.testingrate.gap=mean(testingrate.gap),
                   median.testingrate.gap=median(testingrate.gap),
                   mean.testing.excess=mean(testing.excess),
                   mean.testingrate.excess=mean(testingrate.excess),
                   median.testingrate=median(testingrate),
                   median.positivity=median(positivity1wk)
                   )



#make SVI labels and create quartiles
MAtownSES<- read_csv("MAtownSES_no_bosneighborhoods.csv") %>%
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

#explore distribution of quartiles of SVI (pop not broken into equal parts)
MAtownSES %>%
  group_by(quartile.SVI_minority) %>%
  summarise(n=n(),
            pop=sum(pop.total))

#MA ACS SES-type data and testing summaries, over entire period
Equitytown<- left_join(MAtownSES, testingmatch.means, by="Town") 


#adding vacation towns to Equitytown
vacation<-c(#Berkshires along route 7
  "Sheffield", "Great Barrington", "Stockbridge", "Lee", "Lenox", 
  "Pittsfield",  "North Adams", "Williamstown", 
  #Cape
  "Provincetown", "Truro", "Falmouth", "Barnstable",
  "Chatham", "Orleans", "Eastham", "Wellfleet", "Harwich", "Barnstable", "Mashpee",
  "Yarmouth", "Dennis", "Brewster", "Sandwich", "Bourne",
  #Islands
  "Oak Bluffs","Aquinnah", "Tisbury", "West Tisbury", "Edgartown", "Nantucket", "Chilmark",
  #Buzzards Bay
  "Westport", "Dartmouth"
)
Equitytown<- Equitytown %>%
  mutate(vacationtown=ifelse(Town %in% vacation,1, 0),
         college=ifelse(college.pct>=0.1, 1, 0), 
         collegeorvacation=ifelse(vacationtown==1 | college==1,1, 0))


#college<- MAtownSES%>% filter(college.pct>0.1) %>% dplyr::select(Town)

# weekly measures to look change over time
Equitytown.weekly<- left_join(testingmatch, MAtownSES, by="Town")


############## and summarized by SVI quartiles

#SVI_SES
Equitytown.weekly.SVI_SES<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_SES) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testingrate.median=median(testingrate, na.rm = TRUE),
    positivity.median=median(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_SES, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Socioeconomic",
    SVI_quartile=quartile.SVI_SES) %>%
  dplyr::select(-testingrate.mean, -positivity.mean) %>%
  group_by(quartile.SVI_SES)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_SES<-Equitytown.weekly.SVI_SES %>%
  group_by(quartile.SVI_SES) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_ages_disability
Equitytown.weekly.SVI_ages_disability<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_ages_disability) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_ages_disability, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Household Composition and Disability",
    SVI_quartile=quartile.SVI_ages_disability ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_ages_disability)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_ages_disability<-Equitytown.weekly.SVI_ages_disability %>%
  group_by(quartile.SVI_ages_disability) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_minority
Equitytown.weekly.SVI_minority<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_minority) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_minority, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Minority Status and Language",
    SVI_quartile=quartile.SVI_minority) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_minority)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_minority<-Equitytown.weekly.SVI_minority %>%
  group_by(quartile.SVI_minority) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_house
Equitytown.weekly.SVI_house<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_house) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_house, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Housing Type and Transportation",
    SVI_quartile=quartile.SVI_house ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_house)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_house<-Equitytown.weekly.SVI_house %>%
  group_by(quartile.SVI_house) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_overall
Equitytown.weekly.SVI_overall<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_overall) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_overall, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Overall",
    SVI_quartile=quartile.SVI_overall ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean )%>%
  group_by(quartile.SVI_overall)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_overall<-Equitytown.weekly.SVI_overall %>%
  group_by(quartile.SVI_overall) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

SVI.combined.weekly<- rbind(Equitytown.weekly.SVI_SES, Equitytown.weekly.SVI_ages_disability, 
                            Equitytown.weekly.SVI_house, Equitytown.weekly.SVI_minority, Equitytown.weekly.SVI_overall) %>%
  dplyr::select(-c(quartile.SVI_SES, quartile.SVI_ages_disability:quartile.SVI_overall))



### Table 1
library(table1)

tab1<- Equitytown %>%
  mutate(collegetown=as.factor(ifelse(college.pct>0.1, "Yes", "No")),
         top.quartile.minority= as.factor(ifelse(quartile.SVI_minority>3, "Yes", "No")),
         SES= as.factor(recode(quartile.SVI_SES, "Q1", "Q2", "Q3", "Q4")),
         desc="Quartile of Socioeconomic Vulenerabilty"
  )
label(tab1$town_population) <- "Community Population"
label(tab1$household.income) <- "Community Household Income"
label(tab1$household.size) <- "Community Household Size"
label(tab1$SVI_overall) <- "Community Social Vulnerabilty Index"
label(tab1$top.quartile.minority) <- "Highest Minority/Language Vulnerability"
label(tab1$collegetown) <- "University Town (>10% of population)"
label(tab1$cum.incid.cases) <- "Cumulative Covid-19 Cases"
label(tab1$cum.incid.test) <- "Cumulative Covid-19 Tests"


units(tab1$household.size) <- "number of members"
units(tab1$household.income) <- "$"
units(tab1$SVI_overall) <- "percentile"
units(tab1$cum.incid.cases) <- "per 100,000"
units(tab1$cum.incid.test) <- "per 100,000"

Table1<-table1(~town_population + household.income + household.size + SVI_overall + 
         top.quartile.minority + collegetown +cum.incid.cases + cum.incid.test | desc*SES, data=tab1, 
       topclass="Rtable1-zebra" , overall=FALSE, render.continuous=c(.="Median [Q1,Q3]", .="[Min, Max]"))

library(gridExtra)
ggsave(grid.table(Table1), filename = "Table1.png")
Table1
#### Poisson modeling
library(sandwich)
library(pscl)
library(MASS)

## resources: https://stats.idre.ucla.edu/r/dae/poisson-regression/
##            https://stats.idre.ucla.edu/r/dae/zip/
##            https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
##            https://data.princeton.edu/wws509/r/overdispersion
##            https://rpubs.com/kaz_yos/poisson
##            https://stats.idre.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/


## Notes: Standard Poisson models were over-dispersed due to dispersed data and excess zeros.  Explored zero-inflated poisson/neg binomial
## models.  The ZIP with neg binomial describe data well, but had challenging interpretation with separate estimates for odds
## of being a 'never gap town' and count gap for 'sometimes gap towns'.  The neg binomial model returned similar estimates
## and had large advantage of accessibility of interpretation.  Literature seems to support avoidance of ZIP models for this reason


#removing those with NA testing.gap and uncertain how handled in modeling
Equitytown.weekly.poisson <-Equitytown.weekly %>% 
  filter(!is.na(testing.gap))  %>% 
  filter(!is.na(quartile.SVI_overall)) %>% 
  mutate(collegetown=if_else(date>as.Date("2020-08-11"), if_else(college.pct>0.11, 1, 0),0),
        #collegetown =if_else(college.pct>0.1, 1, 0),
          highest_minority=if_else(quartile.SVI_minority >= 4, 1, 0),
         highest_SES=if_else(quartile.SVI_SES >= 4, 1, 0),
         highest_house=if_else(quartile.SVI_house >= 4, 1, 0),
         highest_disability=if_else(quartile.SVI_ages_disability >= 4, 1, 0),
         )

summary(model.1<- glm.nb(formula = testing.gap ~ quartile.SVI_SES + highest_minority + week +
                         collegetown + offset(log(town_population)), data = Equitytown.weekly.poisson, control=glm.control(maxit=100),
       init.theta = 0.15, link = log))
cov.m1 <- vcovCL(model.1, cluster = ~ Town)
std.err <- sqrt(diag(cov.m1))
rr.m1.est <- cbind(Estimate= exp(coef(model.1)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.1)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.1) - 1.96 * std.err),
                   UL = exp(coef(model.1) + 1.96 * std.err))
model.1.estimates<-data.frame(rr.m1.est)
model.1.estimates

summary(model.2<- glm.nb(formula = testing.gap ~ quartile.SVI_overall + collegetown + week + offset(log(town_population)) , data = Equitytown.weekly.poisson, 
                         init.theta = 0.15, link = log, control=glm.control(maxit=100)))
cov.m2 <- vcovCL(model.2, cluster = ~ Town)
std.err <- sqrt(diag(cov.m2))
rr.m2.est <- cbind(Estimate= exp(coef(model.2)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.2)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.2) - 1.96 * std.err),
                   UL = exp(coef(model.2) + 1.96 * std.err))
model.2.estimates<-data.frame(rr.m2.est)
model.2.estimates

summary(model.3<- glm.nb(formula = testing.gap ~ quartile.SVI_minority + quartile.SVI_SES  + quartile.SVI_house + quartile.SVI_ages_disability
                         + collegetown + week + offset(log(town_population)), data = Equitytown.weekly.poisson, 
                         init.theta = 0.15, link = log))
cov.m3 <- vcovCL(model.3, cluster = ~ Town)
std.err <- sqrt(diag(cov.m3))
rr.m3.est <- cbind(Estimate= exp(coef(model.3)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.3)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.3) - 1.96 * std.err),
                   UL = exp(coef(model.3) + 1.96 * std.err))
model.3.estimates<-data.frame(rr.m3.est)
model.3.estimates

summary(model.4<- glm.nb(formula = testing.gap ~ highest_minority + highest_SES  + highest_house + highest_disability
                         + collegetown + week + offset(log(town_population)), data = Equitytown.weekly.poisson, 
                         init.theta = 0.15, link = log))
cov.m4 <- vcovCL(model.4, cluster = ~ Town)
std.err <- sqrt(diag(cov.m4))
rr.m4.est <- cbind(Estimate= exp(coef(model.4)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.4)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.4) - 1.96 * std.err),
                   UL = exp(coef(model.4) + 1.96 * std.err))
model.4.estimates<-data.frame(rr.m4.est)
model.4.estimates


## sensitivity analysis zero-inflated model, negative binomial
summary(zipb<- zeroinfl(round(testing.gap,0) ~ quartile.SVI_SES + highest_minority + collegetown + week + offset(log(town_population)),
                        dist="negbin", data = Equitytown.weekly.poisson))
cov.zipb <- vcovCL(zipb, cluster = ~ Town)
std.err <- sqrt(diag(cov.zipb))
rr.zipb.est <- cbind(Estimate= exp(coef(zipb)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(zipb)/std.err), lower.tail=FALSE),
                   LL = exp(coef(zipb) - 1.96 * std.err),
                   UL = exp(coef(zipb) + 1.96 * std.err))
zipb.estimates<-data.frame(rr.zipb.est)
zipb.estimates


#########################
##FIGURES
#Facet plot for longitudinal trends
p.SVIgap<-ggplot(SVI.combined.weekly, aes(x=date, group=as.factor(SVI_quartile), color=as.factor(abs(as.numeric(SVI_quartile)-5)))) +
  theme_classic() + 
  #theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
  #                       axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
  geom_line(aes(y=cum.testingrate.gap.byquartile), alpha=.7, size=1.5)+ 
  scale_color_viridis(discrete = TRUE, end=0.5, option = "D", direction=1, name="Vulnerability Quartile",
                      labels=c("High", "Moderate to High", "Low to Moderate", "Low"))+
  scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-10-07"))) +
  facet_wrap(vars(SVI_index), ncol = 3, as.table = FALSE) + 
  theme(legend.position = c(.95, .95), legend.justification = c(1, 1)) +
  labs(x="Date", y="Weekly Test Gap\n(tests per 100,000 residents)",
       title="Social Vulnerability and SARS-CoV-2 Testing Gap in\nMassachusetts Communities, 2020")
ggsave("SVI plots.pdf")

low.mod<-SVI.combined.weekly %>%
  filter(SVI_quartile ==1 |SVI_quartile ==2) %>%
  group_by(SVI_index, date)%>%
  dplyr::summarise(cum.testingrate.gap.byquartile=mean(cum.testingrate.gap.byquartile)) %>%
  mutate(SVI_quartile=2)


mod.high<-SVI.combined.weekly %>% group_by()%>%
  filter(SVI_quartile ==3 |SVI_quartile ==4) %>%
  dplyr::select(date, SVI_quartile, cum.testingrate.gap.byquartile, SVI_index)

gap.plot<-full_join(low.mod, mod.high)

jama<-c(  "#80796BFF","#DF8F44FF", "#B24745FF")
jama.rev<-c( "#B24745FF","#DF8F44FF",  "#80796BFF")
gap.ma.plot<-ggplot(gap.plot %>%
         mutate(quartile=factor(SVI_quartile, levels=c("4", "3", "2"),  labels=c("High\n(75-100 percentile)", 
                                                                                 "Moderate to High\n(50-75 percentile)", "Low/Low to Moderate\n(0-50 percentile)")))  %>%
         filter(SVI_index=="Socioeconomic" | SVI_index=="Minority Status and Language"),
         aes(x=date,y=cum.testingrate.gap.byquartile,  fill=quartile)) +
  theme_classic() + #theme( plot.background = element_rect(fill = "white", colour = "black"))+
  geom_area(alpha=.95, size=0.5, colour="black") +
facet_wrap(vars(SVI_index), ncol = 1, as.table = FALSE) +
  scale_fill_manual(values=jama.rev,  
                    name="SVI Domain Percentile")  +
  scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-10-07"))) +
  theme(legend.position = c(.45, .95), legend.justification = c(1, 1),legend.key.size = unit(0.3, "in")) +
  labs(x="Date", y="Cumulative Testing Gap\n(tests per 100,000 residents)",
       title="Massachusetts Cities and Towns")


#Town comparisons
SVI.p1<- ggplot(Equitytown %>% filter(town_population >15000), aes(x=reorder(Town, SVI_SES), 
                                                             y=testingrate.gap.mean, fill=as.factor(quartile.SVI_SES)))+
  geom_col()+ theme_classic() + 
  scale_fill_viridis(discrete = TRUE, end=0.5, option = "D", direction=-1, name="Vulnerability Quartile",
                     labels=c("Low", "Low to Moderate","Moderate to High",  "High"))+
  theme(legend.position = c(.8, .95), legend.justification = c(1, 1), 
        legend.direction = "vertical",axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Communities (ordered by vulnerabilty, CDC SVI)", y="Average Weekly Test Gap\n(tests per 100,000 residents)",
       title="Socioeconomic Vulnerability")
#ggsave("~/Desktop/SVI plots.pdf", units = "in", width = 18, height=6)

SVI.p2 <- ggplot(Equitytown %>% filter(town_population >15000), aes(x=reorder(Town, SVI_minority), 
                                                              y=testingrate.gap.mean, fill=as.factor(quartile.SVI_minority)))+
  geom_col()+ theme_classic() + 
  scale_fill_viridis(discrete = TRUE, end=0.5, option = "D", direction=-1, name="Vulnerability Quartile",
                     labels=c("Low", "Low to Moderate","Moderate to High",  "High"))+
  theme(legend.position = c(.8, .95), legend.justification = c(1, 1), 
        legend.direction = "vertical",axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Communities (ordered by vulnerabilty, CDC SVI)", y="Average Weekly Test Gap\n(tests per 100,000 residents)",
       title="Minority and Language Vulerability")

SVI.p1 / SVI.p2 + plot_annotation(title = 'Social Vulnerability and SARS-CoV-2 Testing Gap in Massachusetts Communities')
ggsave("SVI town plots.pdf", units = "in", width = 18, height=12)

#########################
# not used
# ggplot(Equitytown.weekly.SVI_minority, aes(x=date, group=as.factor(quartile.SVI_minority), color=as.factor(quartile.SVI_minority))) +
#   theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_line(aes(y=testingrate.gap.byquartile), alpha=.7, size=1.5)+ 
#   scale_color_viridis(discrete = TRUE, end=0.5, option = "D", direction=-1)+
#   guides(color=FALSE) +
#   labs(x="Date", y="Weekly Test Gap\n(Tests per 100,000 residents)",
#        title="Social Vulnerability and SARS-CoV-2 Testing in\nMassachusetts Communities, 2020")+
#   scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-11-01")))+
#   geom_label_repel(data = Equitytown.weekly.SVI_minority %>%
#                      filter(date == as.Date("2020-09-23")), aes(label=label, x = date ,  y = testingrate.gap.byquartile), 
#                    hjust=0, xlim=c(Sys.Date(), as.Date(NA))) 
# 
# ggplot(Equitytown.weekly.quartile, aes(x=date, group=as.factor(quartile.SVI_overall), color=as.factor(quartile.SVI_overall))) +
#   theme_classic() + theme(aspect.ratio=1, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
#   geom_line(aes(y=testingrate.excess.byquartile), alpha=.7, size=1.5)+ 
#   scale_color_viridis(discrete = TRUE, end=0.5, option = "D", direction=-1)+
#   guides(color=FALSE) +
#   labs(x="Date", y="Weekly Test Excess\n(Tests per 100,000 residents)",
#        title="Social Vulnerability and SARS-CoV-2 Testing in\nMassachusetts Communities, 2020")+
#   scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-11-01")))+
#   geom_label_repel(data = Equitytown.weekly.quartile %>%
#                      filter(date == as.Date("2020-09-23")), aes(label=label, x = date ,  y = testingrate.excess.byquartile), 
#                    hjust=0, xlim=c(Sys.Date(), as.Date(NA))) 



###MAPS
# zips_tidy<- read_csv( "zips_tidy.csv")
# zips_tidy$zipcode <- str_to_title(zips_tidy$id)
# 
# #create analysis dataset by zipcode not Town
# Equitytown.zip<-left_join(MAtowns,  Equitytown, by="Town")
# 
# #add to zipcode-based geographic dataset
# EquitytownGeo <-  right_join(Equitytown.zip, zips_tidy, by = "zipcode")

#read in geographic data set for all MA towns (include Boston as entire city, not neighborhoods)
townsgeo<-read.csv("townsgeo.csv")


EquitytownGeo <-  left_join(townsgeo, Equitytown, by = "Town")
jama<-c(  "#80796BFF","#DF8F44FF", "#B24745FF", "#B24745FF")
borders<-c( "#FFFFFF", "#26619c")

#getting centroids for location of town names
library(data.table)
library(geosphere)
cent<- EquitytownGeo %>% 
  dplyr::select(long, lat, Town)
setDT(cent)

#make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
findCentroid <- function(long, lat, ...){
  centroid(cbind(long, lat), ...)
}

centroids<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
centroids<-centroids %>%
  distinct(Town, xname, yname) %>%
  mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
         yname= ifelse(Town=="Nantucket", yname-0.02, yname),
         yname= ifelse(Town=="Chilmark", yname+0.02, yname),
         yname= ifelse(Town=="Boston", yname-0.02, yname),
         xname= ifelse(Town=="Boston", xname+0.04, xname),
         xname= ifelse(Town=="Salem", xname-0.02, xname),
         xname= ifelse(Town=="Westport", xname-0.02, xname),
         yname= ifelse(Town=="Provincetown", yname+0.01, yname))


collegeorvacation<-Equitytown%>%filter(collegeorvacation=="1") %>%  dplyr::select(Town, college, vacationtown)
centroids<-left_join(centroids, collegeorvacation, by="Town")

#SES categorical
SES.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo %>%
                 mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.50, 0.75, Inf),
                                    labels=c("Low/Low to Moderate\n(0-50 percentile)",
                                             "Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
                 filter(!is.na(SES.cat)),
               aes(x = long, y = lat, group = group, fill=SES.cat), 
               colour = "white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.3, .2), 
      legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
      panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=jama,  
                     name="Socioeconomic SVI\nDomain Percentile")  +
  geom_point(data=centroids %>% filter(college==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("A. Socioeconomic Vulnerability") , color="black", hjust=0, vjust=1, 
           size= 5)
#ggsave("test.jpg")
# Equitytown %>%
#   mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 6000, 40000, 50000, Inf),
#                            labels = c("0 to 15000","15000 to 30000", "30000 to 60000", "> 60000"))) %>%
#   distinct(Town,cum.test.cat)%>%
#   count(cum.test.cat)

#covid testing categorical
test.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo %>% 
                 mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 40000, 50000, Inf),
                 labels = c("5000 to 40,000","40,000 to 50,000",  "> 50,000")),
                 border=factor(collegeorvacation)
                 )  %>%
                 filter(!is.na(cum.test.cat)),
               aes(x = long, y = lat, group = group, fill=cum.test.cat), color="white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .2), legend.key.size = unit(0.3, "in"),
        legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=jama,  
                     name="Total Tests\nper 100,000") +
  geom_point(data=centroids %>% filter(college==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
 # geom_point(data=centroids %>% filter(vacationtown==1), aes(x = (xname+0.02), y = yname), color="#79AF97FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("C. Testing Intensity") , color="black", hjust=0, vjust=1, 
           size=5 )

# t1 %>%
#   group_by(testingrate.gap.cat) %>%
#   summarise(n=n())


#gap categorical
gap.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo  %>%
                  mutate(testingrate.gap.cat=cut(testingrate.gap.mean, 
                                              breaks=c(-1, 500, 1000, Inf), 
                                              labels=c("0 to 500", "500 to 1000",  "> 1000"))),
               aes(x = long, y = lat, group = group, fill=as.factor(testingrate.gap.cat)), 
               colour = "white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .2), legend.key.size = unit(0.3, "in"),
          legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=jama,  
                     name="Testing Gap\nper 100,000")  +
  geom_point(data=centroids %>% filter(college==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("E. Average Weekly Testing Gap") , color="black", hjust=0, vjust=1, 
           size= 5)



# #create separate dataset limited to metro Boston
# metrotowns<-c("Chelsea", "Allston Brighton", "Charlestown","Back Bay Downtown", "East Boston", "Dorchester Uphams", "Dorchester Codman", "Fenway", "Hyde Park",
#               "Jamaica Plain", "Mattapan", "Roslindale", "Roxbury", "South Boston", "South End", "West Roxbury", "Brookline", "Newton", "Watertown",
#               "Cambridge", "Somerville", "Medford", "Everett", "Winthrop", "Revere", "Weston", "Wellesley", "Waltham", "Belmont", "Arlington", "Malden",
#               "Needham", "Dover", "Dedham", "Milton", "Quincy", "Nahant", "Lynn", "Saugus", "Melrose", "Woburn", "Lexington", "Lincoln", "Concord",
#               "Lincoln", "Sudbury", "Wayland", "Natick", "Sherborn", "Westwood", "Norwood", "Canton", "Randolph",
#               "Weymouth", "Hingham", "Hull", "Braintree", "Winchester", "Wakefield", "Stoneham", "Salem", "Swampscott",
#               "Marblehead", "Framingham", "Bedford", "Burlington", "Hanscom Afb", "Medfield", "Sherborn", "Walpole", "Boston")
# MetroEquitytownGeo<-dplyr::filter(EquitytownGeo, EquitytownGeo$Town %in% metrotowns)
# 


# #SES categorical
# p4.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>%
#                  mutate(SES.cat=factor(quartile.SVI_SES, levels=c("1", "2", "3", "4"),
#                                        labels=c("Low", "Low to Moderate","Moderate to High", "High"))
#                  ) %>%
#                  filter(!is.na(SES.cat)),
#                aes(x = long, y = lat, group = group, fill=SES.cat), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap() + theme_void() +
#   theme(legend.position = c(.3, .2), 
#         legend.direction = "vertical", 
#         panel.background = element_rect(fill = "White", size=1, color="Black")) +
#   scale_fill_viridis(option="D", na.value="#FDE725FF", discrete=TRUE,
#                      name="Quartiles")  +
#   annotate("Text", x = -73.6, y = 42.96, 
#            label = c("Socioeconomic Vulnerability") , color="black", hjust=0, vjust=1, 
#            size= 5)
# 
# # Equitytown %>%
# #   mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 6000, 40000, 50000, Inf),
# #                            labels = c("0 to 15000","15000 to 30000", "30000 to 60000", "> 60000"))) %>%
# #   distinct(Town,cum.test.cat)%>%
# #   count(cum.test.cat)
# 
# #covid testing categorical
# p5.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>% 
#                  mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 30000, 40000, 50000, Inf),
#                                          labels = c("5000 to 30000","30000 to 40000", "40000 to 50000", "> 50000")))  %>%
#                  filter(!is.na(cum.test.cat)),
#                aes(x = long, y = lat, group = group, fill=cum.test.cat), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .2), 
#         legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
#   scale_fill_viridis(option="D", na.value="#FDE725FF", discrete=TRUE,
#                      name="Molecular Tests\nper 100,000") +
#   annotate("Text", x = -73.6, y = 42.96, 
#            label = c("Testing Intensity") , color="black", hjust=0, vjust=1, 
#            size=5 )
# 
# #Boston only
# #gap categorical
# p6.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>% filter(zipcode !="12125"),
#                aes(x = long, y = lat, group = group, fill=as.factor(testingrate.gap.cat)), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = c(.3, .2), 
#         legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
#   scale_fill_viridis(option="D", na.value="#FDE725FF", discrete=TRUE,
#                      name="Testing Gap\nper 100,000")  +
#   annotate("Text", x = -73.6, y = 42.96, 
#            label = c("Average Weekly Testing Gap") , color="black", hjust=0, vjust=1, 
#            size= 5)
# 
# p4.map / p5.map  / p6.map
# ggsave("MA map mean gap.pdf", units = "in", width = 6, height=12)
# (p1.map + p2.map) / p3.map +
#   plot_layout(heights= c(1,2)) + plot_annotation(
#     title = 'Covid-19 Incidence, Testing, and Testing Gap',
#     subtitle = 'June 29 to October 7, 2020',
#     caption = ''
#   )
#   
# ggsave("MA map mean gap.pdf", units = "in", width = 12, height=12)
# 

# load (file="basemapma.RData")
# load (file="basemapbos.RData")
# load (file="basemapbosw.RData")
# 
# mapTheme <- function(base_size = 12) {
#   theme(
#     text = element_text( color = "black"),
#     plot.title = element_text(size = 18,colour = "black"),
#     plot.subtitle=element_text(face="italic"),
#     plot.caption=element_text(hjust=0),
#     axis.ticks = element_blank(),
#     panel.background = element_blank(),
#     panel.grid.major = element_line("grey80", size = 0),
#     strip.text = element_text(size=12),
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background = element_rect(fill = "grey80", color = "white"),
#     plot.background = element_blank(),
#     legend.background = element_blank(),
#     legend.title = element_text(colour = "black", face = "italic"),
#     legend.text = element_text(colour = "black", face = "italic"))
# }



# p1.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>% 
#                  mutate(cum.incid.cases=(ifelse(cum.cases <10, 100, cum.incid.cases)*1.5),
#                         cum.incid.cases=(ifelse(is.na(cum.incid.cases), 100, cum.incid.cases)*1.5)  #if number of cases <10 or NA incidence, plot at min
#                         ),
#                aes(x = long, y = lat, group = group, fill=cum.incid.cases), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap() + theme_void() + 
#   theme(legend.position = "none", panel.background = element_rect(fill = "White", size=1, color="Black"))  +
#   scale_fill_viridis(option="D",  limits=c(0,2500), na.value="#FDE725FF", breaks = c(0,1000, 2000), 
#                      name="Cases/Test/Gap\nper population") +
#   annotate("Text", x = -73.6, y = 42.96, 
#            label = c("Cumulative Covid-19 Incidence,\nper 150,000 population") , color="black", hjust=0, vjust=1, 
#            size=5 )

# p2.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>% 
#                  mutate(cum.incid.tests.mean=cum.incid.tests.mean*.5), #no transformations
#                aes(x = long, y = lat, group = group, fill=cum.incid.tests.mean), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap()  + theme_void() + 
#     theme(legend.position = "none", panel.background = element_rect(fill = "White", size=1, color="Black"))  +
#   scale_fill_viridis(option="D",  limits=c(0,2500), na.value="#FDE725FF", breaks = c(0,1000, 2000), 
#                      name="Cases/Test/Gap\nper population") +
#   annotate("Text", x = -73.6, y = 42.96, 
#            label = c("Average Weekly SARS-CoV-2 Tests,\nper 50,000 population") , color="black", hjust=0, vjust=1, 
#            size=5 )

# p3.map<-ggplot()+
#   geom_polygon(data = EquitytownGeo %>%
#                  mutate(testingrate.gap.mean=ifelse(cum.cases <10, 100, testingrate.gap.mean), #if number of cases <10 or NA incidence, plot at min
#                         testingrate.gap.cat=as.factor(cut_number(testingrate.gap.mean,4)),
#                         testingrate.gap.mean=testingrate.gap.mean*1), #multi by 1 to make per 100,000
#                aes(x = long, y = lat, group = group, fill=testingrate.gap.cat), 
#                colour = "white", alpha = 0.75, size = 0.05) +
#   coord_quickmap() + theme_void() + theme(legend.position = c(.3, .2), legend.key.width=unit(2,"cm"), legend.key.height=unit(1,"cm"), 
#                                           legend.direction = "horizontal", panel.background = element_rect(fill = "White", size=1, color="Black")) +
#   scale_fill_viridis(option="D", na.value="#FDE725FF", discrete=TRUE,
#                      name="Cases, Test, or Gap\nper population")  +
#   annotate("Text", x = -73.6, y = 42.96, 
#                     label = c("Average Weekly Testing Gap,\nper 100,000 population") , color="black", hjust=0, vjust=1, 
#                  size= 5)
