library(tidyverse)
library(readxl)
library(broom)
library(sf)
library(lubridate)
library(utils)
library(janitor)
`%!in%` = Negate(`%in%`)

#library(foreign)

setwd("~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses")


#MDPH covid case file,  reads in most recent case/testing excel file from their website
download.file("https://www.mass.gov/doc/covid-19-raw-data-september-22-2022/download", "mdphcase_recent.xlsx")
mdphfile<- "mdphcase_recent.xlsx"


boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

# bphc.link<- "https://bphc-dashboard.shinyapps.io/BPHC-dashboard/_w_e904087e/session/5bc8e0dc4e4c4f892866332e58afad68/download/"
# 
# #load information from BPHC on ED visits, the session key needs to updated on each run, https://analytics.boston.gov/app/boston-covid
# bphc_metrics<-full_join(as.data.frame(
#   read_csv(paste0(bphc.link,"ch_metric_ed_visits-plotlyData.csv?w=6cde616f"))) %>%
#   rename("date"="Category1",
#          "ed.cases"="Value") %>%
#   mutate(date=ymd(as.Date(mdy(date)))) %>%
#   select(date, ed.cases),
# 
# as.data.frame(
#   read_csv(paste0(bphc.link,"ch_metric_positives-plotlyData.csv?w=6cde616f"))) %>%
#    rename("date"="Category1",
#          "bos.cases"="Value") %>%
#   mutate(date=ymd(as.Date(mdy(date))))%>%
#     mutate(bos.cases = if_else(is.na(bos.cases), 0, bos.cases)) %>%
#   select(date, bos.cases) %>% filter(date<max(date-1)), by="date")  # the last two dates are suppressed due to incompleteness
# 
# write_csv(bphc_metrics, "bphc_metrics.csv")
# write_csv(bphc_metrics, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/bphc_metrics.csv")

# Read in US county data compiled by NYTimes
# options(timeout = max(6000, getOption("timeout")))
# 
# download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", "nytcounts.csv")
# nytcounts<- read_csv("nytcounts.csv",local = locale(encoding = "latin1"))
# 
download.file("https://data.cdc.gov/api/views/2ew6-ywp6/rows.csv?accessType=DOWNLOAD", "cdcwastewater.csv")


# download.file("https://data.cdc.gov/api/views/jr58-6ysp/rows.csv?accessType=DOWNLOAD", "nowcast.csv")
# CDCnowcast<-read_csv("nowcast.csv",local = locale(encoding = "latin1")) %>%
#   filter(modeltype == "smoothed") %>%
#  mutate(date = date(mdy_hms(week_ending))) %>%
#   select(date, usa_or_hhsregion, variant, share, share_hi, share_lo)

# nytcounts<-nytcounts %>% filter(!is.na(date))  %>% filter(!is.na(cases)) %>% #filter(!is.na(fips)) %>% # removes erroneous rows
#   mutate(cases=as.integer(cases),
#          date=lubridate::ymd(as.character(date)))
# save(nytcounts, file="nytcounts.Rdata")


# download.file("https://data.cdc.gov/api/views/2ew6-ywp6/rows.csv?accessType=DOWNLOAD", "cdcwastewater.csv")


download.file("https://data.cdc.gov/api/views/jr58-6ysp/rows.csv?accessType=DOWNLOAD", "nowcast.csv")
CDCnowcast<-read_csv("nowcast.csv",local = locale(encoding = "latin1")) %>%
  mutate(date = date(mdy_hms(week_ending)),
         published_date = date(mdy_hms(published_date)),
         share = as.numeric(share),
         share_lo = as.numeric(share_lo),
         share_hi = as.numeric(share_hi)) %>%
  filter(published_date == max(published_date)) %>%
  select(date, usa_or_hhsregion, variant, share, share_hi, share_lo)

save(CDCnowcast, file="CDCnowcast.Rdata")


# #creating population table from cmbination of UMass Donahue Institute and direct from ACS
# UMDI.poptable<-read_excel("UMDI_Appendix_A_MA_Subcounty_Pop_2010-2019.xlsx")
# UMDI.poptable<- UMDI.poptable[-c(1,2), c(1,14)] # remove heading rows, keeping 2019 estimates
# names(UMDI.poptable)[1] <- "Town"
# names(UMDI.poptable)[2] <- "town_population"
# 
# boston.neighborhood.pop<- tribble(  #data from ACS 2018
#   ~Town, ~town_population,
#   "Back Bay Downtown", 55698,
#   "Allston Brighton", 66585,
#   "Charlestown", 19414,
#   "Dorchester Codman",80880,
#   "Dorchester Uphams", 64277,
#   "East Boston", 46927,
#   "Fenway", 54727,
#   "Hyde Park", 68446,
#   "Jamaica Plain", 40377,
#   "Mattapan", 29591,
#   "Roslindale", 33771,
#   "Roxbury", 43127,
#   "South Boston", 40094,
#   "South End", 35756,
#   "West Roxbury", 28487
# )
# 
# MAtownpoptable<-full_join(UMDI.poptable, boston.neighborhood.pop) %>%
#   mutate(Town=if_else(Town == "Manchester-by-the-Sea", "Manchester", Town))  #%>%
#   #filter(Town != "Boston") #remove Boston as included as neighborhoods
# 
# write_csv(MAtownpoptable, "MAtownpoptable.csv")
# 
# ## MA zipcode listing
# library(rgdal)
# library(rgeos)
# zips<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/zipcodes_nt/ZIPCODES_NT_POLY.shp")
# zips <- gBuffer(zips, byid=TRUE, width=0) #fixing errors related to junction errors
# 
# #grab the zips and town/county names
# ziptownnames<-zips@data
# 
# #change case and rename to permit join later
# ziptownnames$PC_NAME<-str_to_title(ziptownnames$PC_NAME)
# ziptownnames<-dplyr::rename(ziptownnames, Town = PC_NAME, zipcode = POSTCODE)
# 
# #replace zipcode names with town names for several
# ziptownnames<-ziptownnames %>%
#   mutate(Town = case_when(
#     Town == "Attleboro Falls" ~ "North Attleborough",
#     Town == "North Dighton" ~ "Dighton",
#     Town == "Westport Point" ~ "Westport",
#     Town == "New Lebanon" ~ "Hancock",
#     Town == "South Dartmouth" ~ "Dartmouth",
#     Town == "North Dartmouth" ~ "Dartmouth",
#     Town == "South Yarmouth" ~ "Yarmouth",
#     Town == "North Yarmouth" ~ "Yarmouth",
#     Town == "South Chatham" ~ "Chatham",
#     Town == "South Dennis" ~ "Dennis",
#     Town == "South Harwich" ~ "Harwich",
#     Town == "South Wellfleet" ~ "Wellfleet",
#     Town == "Marstons Mills" ~ "Barnstable",
#     Town == "Hyannis Port" ~ "Barnstable",
#     Town == "Harwich Port" ~ "Harwich",
#     Town == "Forestdale" ~ "Sandwich",
#     Town == "East Dennis" ~ "Dennis",
#     Town == "Dennis Port" ~ "Dennis",
#     Town == "Cummaquid" ~ "Barnstable",
#     Town == "Cotuit" ~ "Barnstable",
#     Town == "Centerville" ~ "Barnstable",
#     Town == "Hyannis" ~ "Barnstable",
#     Town == "West Wareham" ~ "Wareham",
#     Town == "Vineyard Haven" ~ "Oak Bluffs",
#     Town == "Siaconset" ~ "Nantucket",
#     Town == "East Wareham" ~ "Wareham",
#     Town == "East Sandwich" ~ "Sandwich",
#     Town == "East Falmouth" ~ "Falmouth",
#     Town == "Newton Highlands" ~ "Newton",
#     Town == "Newton Center" ~ "Newton",
#     Town == "South Easton" ~ "Easton",
#     Town == "Middleboro" ~ "Middleborough",
#     Town == "North Weymouth" ~ "Weymouth",
#     Town == "Foxboro" ~ "Foxborough",
#     Town == "East Walpole" ~ "Walpole",
#     Town == "Brant Rock" ~ "Marshfield",
#     Town == "South Hamilton" ~ "Hamilton",
#     Town == "Prides Crossing" ~ "Beverly",
#     Town == "North Billerica" ~ "Billerica",
#     Town == "Cherry Valley" ~ "Leicester",
#     Town == "Whitinsville" ~ "Northbridge",
#     Town == "South Grafton" ~ "Grafton",
#     Town == "Rochdale" ~ "Leicester",
#     Town == "Fiskdale" ~ "Sturbridge",
#     Town == "South Deerfield" ~ "Deerfield",
#     Town == "Shelburne Falls" ~ "Shelburne",
#     Town == "Millers Falls" ~ "Montague",
#     Town == "Southfield" ~ "New Marlborough",
#     Town == "Haydenville" ~ "Williamsburg",
#     Town == "Feeding Hills" ~ "Agawam",
#     Town == "Pocasset" ~ "Bourne",
#     Town == "Onset" ~ "Wareham",
#     Town == "North Falmouth" ~ "Falmouth",
#     Town == "Woods Hole" ~ "Falmouth",
#     Town == "Buzzards Bay" ~ "Bourne",
#     Town == "Osterville" ~ "Barnstable",
#     Town == "Attleboro Falls" ~ "North Attleborough",
#     Town == "North Attleboro" ~ "North Attleborough",
#     Town == "East Taunton" ~ "Taunton",
#     Town == "Cuttyhunk" ~ "Gosnold",
#     Town == "Assonot" ~ "Freetown",
#     Town == "Yarmouth Port" ~ "Yarmouth",
#     Town == "West Yarmouth" ~ "Yarmouth",
#     Town == "West Harwich" ~ "Harwich",
#     Town == "West Dennis" ~ "Dennis",
#     Town == "West Barnstable" ~ "Barnstable",
#     Town == "Cataumet" ~ "Bourne",
#     Town == "Chestnut Hill" ~ "Newton",
#     Town == "Auburndale" ~ "Newton",
#     Town == "Newton Lower Falls" ~ "Newton",
#     Town == "North Easton" ~ "Easton",
#     Town == "Cataumet" ~ "Bourne",
#     Town == "Monponsett" ~ "Hanson",
#     Town == "South Weymouth" ~ "Weymouth",
#     Town == "East Weymouth" ~ "Weymouth",
#     Town == "Byfield" ~ "Newbury",
#     Town == "North Chelmsford" ~ "Chelmsford",
#     Town == "South Lancaster" ~ "Lancaster",
#     Town == "North Oxford" ~ "Oxford",
#     Town == "North Grafton" ~ "Grafton",
#     Town == "Jefferson" ~ "Holden",
#     Town == "West Townsend" ~ "Townsend",
#     Town == "East Weymouth" ~ "Weymouth",
#     Town == "Baldwinville" ~ "Templeton",
#     Town == "Devens" ~ "Harvard",
#     Town == "Turner Falls" ~ "Montague",
#     Town == "Drury" ~ "Florida",
#     Town == "West Hatfield" ~ "Hatfield",
#     Town == "Turner Falls" ~ "Montague",
#     Town == "South Egremont" ~ "Mount Washington",
#     Town == "West Chesterfield" ~ "Chesterfield",
#     Town == "Chestfield" ~ "Chesterfield",
#     Town == "Three Rivers" ~ "Palmer",
#     Town == "West Newton" ~ "Newton",
#     Town == "Newton Upper Falls" ~ "Newton",
#     Town == "Waban" ~ "Newton",
#     Town == "Newtonville" ~ "Newton",
#     Town == "Newton Upper Falls" ~ "Newton",
#     Town == "Wellesley Hills" ~ "Wellesley",
#     Town == "Needham Heights" ~ "Needham",
#     Town == "Hanscom Afb" ~ "Bedford",
#     Town == "North Truro" ~"Truro",
#     Town == "Turners Falls" ~ "Montague",
#     Town == "Lake Pleasant" ~ "Montague",
#     Town == "Leeds" ~ "Northampton",
#     Town == "Florence" ~ "Northampton",
#     Town == "East Freetown" ~ "Freetown",
#     Town == "Assonet" ~ "Freetown",
#     Town == "Tyngsboro" ~ "Tyngsborough",
#     Town == "Siasconset" ~ "Nantucket",
#     Town == "Wendell Depot" ~ "Wendell",
#     Town == "Ashley Falls" ~ "Sheffield",
#     Town == "West Warren" ~ "Warren",
#     Town == "Sagamore" ~ "Bourne",
#     Town == "Sagamore Beach" ~ "Bourne",
#     Town == "Indian Orchard" ~ "Springfield",
#     Town == "North Chatham" ~ "Chatham",
#     Town == "East Otis" ~ "Otis",
#     Town == "South Barre" ~ "Barre",
#     Town == "Gilbertville" ~ "Hardwick",
#     Town == "North Attleboro" ~ "North Attleborough",
#     Town == "South Walpole" ~ "Walpole",
#     Town == "Tyco" ~ "Westminster",
#     Town == "South Lee" ~ "Lee",
#     Town == "North Hatfield" ~ "Hatfield",
#     Town == "Wheelwright" ~ "Hardwick",
#     Town == "Berkshire" ~ "Lanesborough",
#     Town == "Fayville" ~ "Southborough",
#     Town == "Monument Beach" ~ "Bourne",
#     Town == "Stonehill College" ~ "Easton",
#     Town == "Lenox Dale" ~ "Lenox",
#     Town == "Smith College" ~ "Northampton",
#     Town == "Northhampton" ~ "Northampton",
#     Town == "Humarock" ~ "Scituate",
#     Town == "Monroe Bridge" ~ "Monroe",
#     Town == "Housatonic" ~ "Great Barrington",
#     Town == "East Templeton" ~ "Templeton",
#     Town == "Bondsville" ~ "Palmer",
#     Town == "West Hyannisport" ~ "Barnstable",
#     TRUE ~ Town
#   )) %>%
#   dplyr::select(Town, zipcode) %>%
#   filter(Town != 'Brighton' & Town != 'Boston' & Town != 'East Boston' &
#            Town != 'Roxbury Crossing' & Town != 'Dorchester' & Town != 'Dorchester Center'
#          & Town != 'Allston' & Town != "South Boston"  & Town != "Jamaica Plain"  & Town != "Roslindale"
#          & Town != "West Roxbury"  & Town != "Charlestown"  & Town != "Hyde Park"
#          & Town != "Mattapan"  & Town != "Roxbury") %>%
#   mutate(zipcode=if_else(Town =="Hancock", "01237", as.character(zipcode)))
# 
# bosziptownnames<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Boston") %>%
#   dplyr::select(Town, zipcode) %>% distinct(Town, zipcode)
# maziptownnames<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Towns") %>%
#   dplyr::select(Town) %>% distinct(Town)
# 
# bosziptownnames$zipcode <- as.character(bosziptownnames$zipcode)
# bosziptownnames$zipcode <-sapply(bosziptownnames$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
# 
# missingzips<-tribble(
#   ~Town, ~zipcode,
#   "Alford","01266",
#   "Aquinnah", "02535",
#   "Clarksburg",	"01247",
#   "Egremont","01230",
#   "Hawley", "01339",
#   "Leyden", "01337",
#   "Middlefield", "01243",
#   "Montgomery",	"01050",
#   "New Ashford",	"01237",
#   "Pelham", "01002",
#   "Peru", "01235",
#   "Phillipston", "01331",
#   "Tisbury", "02568",
#   "Tolland", "01034",
#   "Washington", "01223",
#   "West Tisbury",	"02575",
#   "Westhampton","01027"
# )
# 
# 
# #read in EOHHS zones
# #https://docs.digital.mass.gov/dataset/massgis-data-ma-executive-office-health-human-services-regions
# write_csv(read.dbf("~/Dropbox (Partners HealthCare)/R files/covid/reg_eohhs/REGEOHHS_POLY_TOWNS.dbf")%>%
#             mutate(Town=str_to_title(TOWN),
#                    Town=ifelse(Town=="North Attleboro", "North Attleborough", Town)) %>%
#             rename(eohhs=REG_NUM, eohhs_name=REG_NAME) %>%
#             dplyr::select(eohhs, eohhs_name, Town), "eohhs.csv" )
# 
# MAtowns1<-full_join(ziptownnames, bosziptownnames)
# MAtowns2<-full_join(MAtowns1, missingzips)
# 
# eohhs<-read_csv("eohhs.csv")
# MAtowns3<-left_join(MAtowns2, eohhs, by="Town") %>%
#   mutate(eohhs=ifelse(is.na(eohhs), 6, eohhs),
#          eohhs_name=ifelse(is.na(eohhs_name), "Boston", eohhs_name))
# MAtowns<-full_join(MAtowns3, MAtownpoptable %>% filter(Town !="Boston"))
# 
# rm(MAtowns1, MAtowns2, MAtowns3)
# write_csv(MAtowns,"MAtowns.csv")
# #MAtowns %>% distinct(Town)
# 
# #code to include all Boston zips under single Boston, no neighborhoods
# bosziptownnames_no_bosneighborhoods<-bosziptownnames
# bosziptownnames_no_bosneighborhoods$Town<- "Boston"  #change all zipcodes to Boston, no neighborhoods
# MAtowns1_no_bosneighborhoods<-full_join(ziptownnames, bosziptownnames_no_bosneighborhoods)  #add together for complete zipcode listing
# MAtowns2_no_bosneighborhoods<-full_join(MAtowns1_no_bosneighborhoods, missingzips) #adding a few additional small town zips
# MAtowns3_no_bosneighborhoods<-left_join(MAtowns2_no_bosneighborhoods, eohhs, by="Town") #eohhs zones
# MAtowns_no_bosneighborhoods<- full_join(MAtowns3_no_bosneighborhoods, MAtownpoptable) # adding population column
# 
# MAtowns_no_bosneighborhoods<-MAtowns_no_bosneighborhoods %>% filter(!is.na(zipcode)) # remove the Boston neighborhoods that get added from poptable
# rm(MAtowns1_no_bosneighborhoods, MAtowns2_no_bosneighborhoods, MAtowns3_no_bosneighborhoods)
# write_csv(MAtowns_no_bosneighborhoods,"MAtowns_no_bosneighborhoods.csv")
# #MAtowns_no_bosneighborhoods %>% distinct(Town)
# 
# # comfirming complete list and no additional towns
# # `%!in%` = Negate(`%in%`)
# # t1<-MAtowns %>% filter(Town %!in% maziptownnames$Town)
# 
# #MAtowns<-read_csv("MAtowns.csv")

## Unified MDPH and BPHC data, longitudinal
#MDPH data
covidtowns <-rbind(
  read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Towns", guess_max = 20000) %>%
  mutate( date = ymd(as.Date(date)),
          Count = as.numeric(Count),
          Count = replace_na(Count, 3),   #MDPH does not report case counts between 1 and 5, impute as 3
          Tests = as.numeric(Tests),
          Start_Date = date-18,
          End_Date = date-5) %>%
  select(date, Town, Count, Tests, Start_Date, End_Date) %>%
  filter(date < as.Date("2020-12-24")), # MDPH started producing longitudinal report on 24 Dec 2020, rather than only one week reports tallied here
          
  read_excel(sheet = "Weekly_City_Town", 
          mdphfile) %>%  #using longitudinual report when available
  rename(Town = 'City/Town',
         Count = 'Total Case Counts',
         Tests = 'Total Tests',
         date = 'Report Date',
         MDPHpopulation = Population) %>%
  mutate(date = ymd(as.Date(date)),
         Start_Date = ymd(as.Date(Start_Date)),
         End_Date = ymd(as.Date(End_Date)), 
         Count = if_else(Count == "<5", 3, as.numeric(Count))) %>%
  select(date, Town, Count, Tests, Start_Date, End_Date)
)


#Importing BPHC case data from revised dashboard

#adapted from https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/
data_path <- "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/BPHC neighborhood" 
files <-  dir(data_path , pattern = "*.csv")

# gathering weekly files downloaded from: https://analytics.boston.gov/app/boston-covid
data <- data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path(data_path, .))) # a new data column
  )  
boston.case.temp <- 
  unnest(data, cols = c(file_contents)) %>%
  rename(Town = Category1, 
         Cases2wk = Value_1) %>%
  mutate(Start_Date = mdy(str_sub(filename, 70, 79 )), 
         End_Date = mdy(str_sub(filename, 84, 93 )),
         date =  End_Date +5,
         Town = fct_collapse(Town, 
                             "Allston Brighton" = "Allston/Brighton",
                             "Back Bay Downtown"= "Backbay/North End/Downtown",
                             "Dorchester Uphams" = "Dorchester (02121, 02125)",
                             "Dorchester Codman" = "Dorchester (02122, 02124)" )) %>%
  select(date, Start_Date, End_Date, Town, Cases2wk)


boston.case<- #bind_rows(
  boston.case.temp %>%
  arrange(Start_Date) %>%
  group_by(Town) %>%
  mutate(id = row_number(),   #need to sum every other as data comes as 2-weekly numbers, 
         row_odd = id %% 2,    #first getting odd and even sepately then joining, so not double counting
         Cases2wk.odd = if_else(row_odd == 1, Cases2wk, as.numeric(NA)),
         Cases2wk.even = if_else(row_odd == 0, Cases2wk, as.numeric(NA)),
         odd = cumsum(if_else(is.na(Cases2wk.odd), 0, Cases2wk.odd)), 
         odd = if_else(row_odd==1, odd, as.numeric(NA)),
         even = cumsum(if_else(is.na(Cases2wk.even), 0, Cases2wk.even)), 
         even = if_else(row_odd==0, even, as.numeric(NA)),
         Count = if_else(is.na(even), odd, even),
         Tests = as.numeric(NA)
  ) #%>%
  
#   #chunk written when delayed reporting of neighbhoorhood cases, keeking in case need to use, bind_rows to join data sources
#   
#   read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="BPHC current positivity") %>%
#   rename(
#     cases1wk = positive, 
#     Start_Date = first.testdate.included,
#     End_Date = last.testdate.included) %>%
#   mutate(date = ymd(date),
#          Start_Date = ymd(Start_Date),
#          End_Date = ymd(End_Date),
#          Count = as.numeric(NA),
#          Tests= as.numeric(NA)) %>%
#   select(date, Town, cases1wk, Start_Date, End_Date, Count, Tests) %>%
#   filter(date >= as.Date("2022-02-24")) %>%  #last from BPHC of neighborhood cases, needed to include testing data
#   distinct(Town, date, .keep_all = TRUE) %>%
#   mutate( cases1wk = if_else(date == as.Date("2022-03-03") & Town == "Fenway", 291, cases1wk),  #accounting for large university positive tests
#           cases1wk = if_else(date == as.Date("2022-03-03") & Town == "Allston Brighton", 86, cases1wk))
# ) 
  # 
  # 
  # filter(Town != "Boston") %>%
  # select(date, Town, Count, Tests, Start_Date, End_Date) %>%
  # group_by(Town) %>%
  # arrange((date)) %>%
  # mutate (cases1wk = if_else(is.na(cases1wk), Count- lag(Count,1), cases1wk),
  #         totalcases = cumsum(replace_na(cases1wk, 0)), 
  #         Count = totalcases) %>%
  # select(date, Town, Count, Tests, Start_Date, End_Date)

  






#BPHC data  (obtained from BPHC weekly reports only through 2020-11-11, then adding in currentpositivity tab to obtain
# more comparable testing data to the overall MA reports)
boscovidtowns <-left_join(
  # read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Boston") %>%
  # mutate(date = ymd(date),
  #        Count = as.numeric(Count), 
  #        Tests = as.numeric(Tests),
  #        Tests = if_else(date> as.Date("2021-04-29"), as.numeric(NA), Tests) ) %>%
  # distinct(date, Town, .keep_all = TRUE) %>%
  # mutate(Start_Date = date-18,
  # End_Date = date-5) %>%
  # select(date, Town, Count, Tests, Start_Date, End_Date), 
  
  boston.case,
  
rbind(
 read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="Boston") %>%
    mutate(date = ymd(date),
           Count = as.numeric(Count), 
           Tests = as.numeric(Tests)) %>%
    distinct(date, Town, .keep_all = TRUE)  %>% 
    filter(date == as.Date("2020-11-04")) %>%
  rename(currenttests=Tests) %>%
  mutate(date=as.Date("2020-11-10"),
         currentpositivity = as.numeric(NA)) %>%
    select(date, Town, currenttests, currentpositivity),
  read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="BPHC current positivity") %>%
    select(date, Town, currenttests, currentpositivity) %>%
  distinct(date, Town, .keep_all = TRUE) %>%
  mutate(date = ymd(date))%>%
  filter(date %in% covidtowns$date)) %>%  # limit twice weekly BPHC positivity to weekly MDPH schedule
  arrange(date) %>%
  group_by(Town) %>%
  mutate(Tests.current = cumsum(currenttests)) %>%
  select(date, Town, Tests.current) %>%
  ungroup(), by= c("date", "Town")) %>%
  mutate(Tests = if_else(is.na(Tests.current), Tests, Tests.current)) %>%
  select(-Tests.current)

allcovidtowns<- rbind(covidtowns %>%
                            filter(Town %!in% boston) %>%
                            select(date, Town, Count, Tests),  #remove Boston, as included as neighborhoods instead
                           boscovidtowns %>% filter(date <= max(covidtowns$date) ) %>%
                            select(date, Town, Count, Tests)) %>%
                        mutate(Count = round(Count, 0),
                               Tests = round(Tests, 0))
write_csv(allcovidtowns, "allcovidtowns.csv")
write_csv(allcovidtowns, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/allcovidtowns.csv")

#create dataset for consolidated Boston
allcovidtowns_no_bosneighborhoods <- covidtowns
write_csv(allcovidtowns, "allcovidtowns_no_bosneighborhoods.csv")
write_csv(allcovidtowns_no_bosneighborhoods, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/allcovidtowns_no_bosneighborhoods.csv")


#MDPH data by race
mdph_race <- read_excel(mdphfile, sheet="RaceEthnicityLast2Weeks") %>%
  filter(!is.na(Population_Estimate_N))

write_csv(mdph_race, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/mdph_race.csv")
write_csv(mdph_race, "mdph_race.csv")

#MDPH data by age
mdph_age <- read_excel(mdphfile, sheet="CasesbyAge") %>%
  clean_names()

write_csv(mdph_age, "mdph_age.csv")
write_csv(mdph_age, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/mdph_age.csv")

#MDPH data by date of test
mdph_testingbydate <- read_excel(mdphfile, sheet="TestingByDate (Test Date)") %>%
  janitor::clean_names() %>%
  mutate(date=as.Date(date),
         newpositive=molecular_positive_new + antigen_positive_new) %>%
  select(date, newpositive) %>%
  filter(date < max(date)-1) %>% #remove last day given delayed reported and resultant downward bias
  mutate(incidence1day = newpositive*100000/7029917,
         incidence7day = zoo::rollsumr(newpositive, k=7, fill=NA)*100000/7029917,
         rolling7day = incidence7day/7)

save(mdph_testingbydate, file="mdph_testingbydate.Rdata")



write_csv(mdph_age, "mdph_age.csv")
write_csv(mdph_age, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/mdph_age.csv")



#####ONLY NEED TO RUN WHEN UPDATING CENSUS VARIABLES, OTHERWISE STATIC WEEK TO WEEK
# library(tidycensus)
# ##get zcta5 and census tract file for MA
# zcta5<- read_csv(url("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt"))
# ma_zcta5<-zcta5 %>% filter(STATE == "25")
# write_csv(ma_zcta5, "ma_zcta5.csv")
# 
# ## Obtaining zipcode level ACS data from US Census Dept, 
# 
# #to explore variables
# #v17 <- load_variables(2018, "acs5", cache = TRUE)



# library(tidycensus)
# vars <- c(
#   "B19013_001", "B02001_002", "B03002_004", "B03002_006", "B03001_003", "B25010_001", "S1401_C01_010E", "B01001_001") # median household income estimate,  white, black, asian, latino, average household size
# ALLzips<-get_acs(geography = "zcta",
#                  variables = vars,
#                  #state = "25", # cannot limit to state in ZCTA
#                  year = 2018)
#
# # write_csv(ALLzips, "ALLzips.csv")
# 
# ALLzips<-read_csv("ALLzips.csv")
# MAtowns<-read_csv("MAtowns.csv")
# #reformat ACS data into by zipcode
# MAzipSES<-ALLzips %>%
#   mutate(zipcode=as.factor(str_remove_all(NAME, "ZCTA5 ")))  %>%
#   filter(zipcode %in% MAtowns$zipcode)  %>%
#   group_by(zipcode) %>%
#   dplyr::select(variable, estimate, zipcode) %>%
#   pivot_wider(names_from = variable, values_from = estimate) %>%
#   rename(household.income.zip=B19013_001, pop.white.zip=B02001_002, pop.black.zip=B03002_004, pop.asian.zip=B03002_006,
#          pop.latino.zip=B03001_003, household.size.zip=B25010_001, pop.college.zip=S1401_C01_010, pop.total.zip=B01001_001)
# 
# #code that considers Boston and single town
# MAzipSES_no_bosneighborhoods<-ALLzips %>%
#   mutate(zipcode=as.factor(str_remove_all(NAME, "ZCTA5 ")))  %>%
#   filter(zipcode %in% MAtowns_no_bosneighborhoods$zipcode)  %>%
#   group_by(zipcode) %>%
#   dplyr::select(variable, estimate, zipcode) %>%
#   pivot_wider(names_from = variable, values_from = estimate) %>%
#   rename(household.income.zip=B19013_001, pop.white.zip=B02001_002, pop.black.zip=B03002_004, pop.asian.zip=B03002_006,
#          pop.latino.zip=B03001_003, household.size.zip=B25010_001, pop.college.zip=S1401_C01_010, pop.total.zip=B01001_001)
# 
# 
# MAtownSES<-left_join(MAtowns, MAzipSES, by="zipcode") %>%
#   replace(is.na(.), 0)  %>%
#   group_by(Town) %>%
#   dplyr::summarise(
#     pop.total=sum(pop.total.zip),
#     pop.white=sum(pop.white.zip),
#     pop.black=sum(pop.black.zip),
#     pop.asian=sum(pop.asian.zip),
#     pop.latino=sum(pop.latino.zip),
#     pop.college=sum(pop.college.zip),
#     household.income=sum(household.income.zip*pop.total.zip)/pop.total,
#     household.size=sum(household.size.zip*pop.total.zip)/pop.total
#   ) %>%
#   mutate(
#     white.pct=pop.white/pop.total,
#     black.pct=pop.black/pop.total,
#     asian.pct=pop.asian/pop.total,
#     latino.pct=pop.latino/pop.total,
#     college.pct=pop.college/pop.total
#   )
# 
# #code that considers Boston and single town
# MAtownSES_no_bosneighborhoods<-left_join(MAtowns_no_bosneighborhoods, MAzipSES_no_bosneighborhoods, by="zipcode") %>%
#   replace(is.na(.), 0)  %>%
#   group_by(Town) %>%
#   dplyr::summarise(
#     pop.total=sum(pop.total.zip),
#     pop.white=sum(pop.white.zip),
#     pop.black=sum(pop.black.zip),
#     pop.asian=sum(pop.asian.zip),
#     pop.latino=sum(pop.latino.zip),
#     pop.college=sum(pop.college.zip),
#     household.income=sum(household.income.zip*pop.total.zip)/pop.total,
#     household.size=sum(household.size.zip*pop.total.zip)/pop.total
#   ) %>%
#   mutate(
#     white.pct=pop.white/pop.total,
#     black.pct=pop.black/pop.total,
#     asian.pct=pop.asian/pop.total,
#     latino.pct=pop.latino/pop.total,
#     college.pct=pop.college/pop.total
#   )
# 
# 
# #read in CDC SVI file for MA census districts
# MA2018_SVI <- read_csv("MA2018_SVI.csv")
# 
# # library(tidycensus)
# # ##get zcta5 and census tract file for MA
# # zcta5<- read_csv(url("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt"))
# # ma_zcta5<-zcta5 %>% filter(STATE == "25")
# # write_csv(ma_zcta5, "ma_zcta5.csv")
# 
# #read in file of ZCTA5 to census tract
# ma_zcta5 <- read_csv("ma_zcta5.csv")  %>% rename(FIPS = GEOID) %>% dplyr::select(FIPS, ZCTA5)  #new_name = old_name
# 
# MA_SVI<- full_join(MA2018_SVI, ma_zcta5) %>%
#   filter(!is.na(ZCTA5)) %>%
#   filter(RPL_THEMES > 0) %>%
#   dplyr::select(E_TOTPOP,RPL_THEME1,RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES, ZCTA5)%>%
#   rename(zipcode = ZCTA5)
# 
# MA_SVI.zip<- MA_SVI %>%
#   group_by(zipcode) %>%
#   dplyr::summarise(
#       E_TOTPOP.zip=sum(E_TOTPOP),
#       RPL_THEME1.zip=(sum(RPL_THEME1*E_TOTPOP)/E_TOTPOP.zip),
#       RPL_THEME2.zip=(sum(RPL_THEME2*E_TOTPOP)/E_TOTPOP.zip),
#       RPL_THEME3.zip=(sum(RPL_THEME3*E_TOTPOP)/E_TOTPOP.zip),
#       RPL_THEME4.zip=(sum(RPL_THEME4*E_TOTPOP)/E_TOTPOP.zip),
#       RPL_THEMES.zip=(sum(RPL_THEMES*E_TOTPOP)/E_TOTPOP.zip)
#       )
# 
# #creating dataset for mAb distribution
# 
# mAb_SVI<-left_join(MA_SVI.zip, MAtowns, by="zipcode") %>%
#   rename(SVI_SES=RPL_THEME1.zip,
#          SVI_household.disability=RPL_THEME2.zip,
#          SVI_minority.language=RPL_THEME3.zip,
#         SVI_house.transport=RPL_THEME4.zip,
#         SVI_overall=RPL_THEMES.zip)
# 
# write_csv(mAb_SVI, "mAb_SVI.csv")
# # 
# 
# MA_SVI_town<-left_join(MAtowns, MA_SVI.zip, by="zipcode")%>%
#   filter(!is.na(RPL_THEMES.zip))%>%
#   group_by(Town) %>%
#   dplyr::summarise(
#     E_TOTPOP.town=sum(E_TOTPOP.zip),
#     RPL_THEME1.town=(sum(RPL_THEME1.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME2.town=(sum(RPL_THEME2.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME3.town=(sum(RPL_THEME3.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME4.town=(sum(RPL_THEME4.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEMES.town=(sum(RPL_THEMES.zip*E_TOTPOP.zip)/E_TOTPOP.town)
#     )
# 
# # Socioeconomic – RPL_THEME1
# # Household Composition & Disability – RPL_THEME2
# # Minority Status & Language – RPL_THEME3
# # Housing Type & Transportation – RPL_THEME4
# # Overall tract rankings:  RPL_THEMES.
# 
# #code that considers Boston and single town
# MA_SVI_town_no_bosneighborhoods<-left_join(MAtowns_no_bosneighborhoods, MA_SVI.zip, by="zipcode")%>%
#   filter(!is.na(RPL_THEMES.zip))%>%
#   group_by(Town) %>%
#   dplyr::summarise(
#     E_TOTPOP.town=sum(E_TOTPOP.zip),
#     RPL_THEME1.town=(sum(RPL_THEME1.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME2.town=(sum(RPL_THEME2.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME3.town=(sum(RPL_THEME3.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEME4.town=(sum(RPL_THEME4.zip*E_TOTPOP.zip)/E_TOTPOP.town),
#     RPL_THEMES.town=(sum(RPL_THEMES.zip*E_TOTPOP.zip)/E_TOTPOP.town)
#   )
# 
# 
# 
# # Socioeconomic – RPL_THEME1
# # Household Composition & Disability – RPL_THEME2
# # Minority Status & Language – RPL_THEME3
# # Housing Type & Transportation – RPL_THEME4
# # Overall tract rankings:  RPL_THEMES.
# 
# 
# #joining SVI with ACS data
# MAtownSES<-full_join(MAtownSES, MA_SVI_town)
# MAtownSES_no_bosneighborhoods<-full_join(MAtownSES_no_bosneighborhoods, MA_SVI_town_no_bosneighborhoods)
# 
# write_csv(MAtownSES, "MAtownSES.csv")
# write_csv(MAtownSES_no_bosneighborhoods, "MAtownSES_no_bosneighborhoods.csv")
# 
# 
# #geographic datasets
# library(tidyverse)
# library(readxl)
# library(broom)
# library(sf)
# library(rgdal)
# library(rgeos)
# 
# #useful code chunk
# `%!in%` = Negate(`%in%`)
# 
# #read in shapefile of zipcodes from Mass GIS
# zips<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/zipcodes_nt/ZIPCODES_NT_POLY.shp")
# zips <- gBuffer(zips, byid=TRUE, width=0) #fixing errors related to junction errors
# 
# #convert from spatial polygon DF to sf
# zips.sf<-st_as_sf(zips, coords = c("lon", "lat"), crs = 4326)
# 
# #read in Town names
# MAtowns<-read_csv("MAtowns.csv")
# 
# t1<-left_join(zips.sf, MAtowns, by= c("POSTCODE" ="zipcode"))
# 
# #collapse zipcodes into towns (for town with multiple)
# t2<-t1 %>%
#   group_by(Town) %>%
#   dplyr::summarise(geometry=st_union(geometry))
# 
# #convert back to spatial polygon df
# t3 <- as_Spatial (t2, IDs=t2$Town)
# #project
# t4 <- spTransform(t3, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# #make tidy for ggplot
# t5<-tidy(t4, region = c("Town"))
# 
# ggplot()+
#   geom_polygon(data = t5 ,
#                aes(x = long, y = lat, group = group, fill="grey80"),
#                colour = "white", alpha = 0.95, size = 0.05) +
#   coord_quickmap() + theme_void()
# 
# 
# #read in town shapefile from Mass GIS
# towns<-readOGR("~/Dropbox (Partners HealthCare)/R files/covid/townssurvey_shp/TOWNSSURVEY_POLY.shp")
# towns <- gBuffer(towns, byid=TRUE, width=0) #fixing errors related to junction errors
# 
# towns.sp <- spTransform(towns, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# towns.df<-tidy(towns.sp, region = c("TOWN"))
# 
# 
# #read in BOS neighborohood shape, but admin districts rather the public health units, using to have improved plot for Brighton
# cityofboston<-readOGR("~/Dropbox (Partners HealthCare)/R files/Boston nightlight/Boston_Neighborhoods_StatAreas+2017/Boston_Neighborhoods.shp")
# cityofboston <- gBuffer(cityofboston, byid=TRUE, width=0) #fixing errors related to junction errors
# cityofboston.sf<-st_as_sf(cityofboston, coords = c("lon", "lat"), crs = 4326) %>%
#   filter(Name=="Allston" | Name=="Brighton" | Name== "West Roxbury") %>%
#   mutate(Town=Name,
#          Town=ifelse(Town=="Brighton" | Town== "Allston", "Allston Brighton", "West Roxbury")) %>%
#   group_by(Town)%>%
#   dplyr::summarise(geometry=st_union(geometry))
# 
# cityofboston.3 <- as_Spatial(cityofboston.sf, IDs=cityofboston.sf$Town)
# cityofboston.4 <- spTransform(cityofboston.3 , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# Allston.WestRox<-tidy(cityofboston.4, region = c("Town"))
# 
# 
# 
# #remove boston from town map file to permit replacing with neighborhoods
# towns.noboston<- towns.df %>% filter(id != "BOSTON")
# 
# #taking zipcode maps and extracting boston neighbohoords
# towns.onlyboston<- t5 %>% filter(
#   id == "East Boston" |
#     id == "Charlestown" |
#     id == "Back Bay Downtown" |
#     id == "South Boston" |
#     id == "South End" |
#     id == "Fenway" |
#     id == "Roxbury" |
#     id == "Dorchester Uphams" |
#     id == "Dorchester Codman" |
#     id == "Mattapan" |
#     id == "Jamaica Plain" |
#     id == "Roslindale" |
#     id == "Hyde Park" |
#     id == "West Roxbury" #|  #issues with zipcode geography around BC/brookline makes these inaccurate.
#   # id == "Allston Brighton" |
# )
# 
# #merging to create full boston data set (West Rox still missing chunk bordering brookline)
# townsgeo_onlybostonneigh <-full_join(towns.onlyboston, Allston.WestRox %>% filter(id !="West Roxbury")) %>%#adding in allston/brighton, West Rox still partially inaccurate
#   #filter( piece== "1")%>%
#   mutate(Town=str_to_title(id, locale = "en")) #remove non-populated islands and Fenway chunk in the 'neck'
# write_csv(townsgeo_onlybostonneigh, "townsgeo_onlybostonneigh.csv")
# 
# #MA towns with boston neigh  ## does not include Allston Brighton
# townsgeo_withbostonneigh <-full_join(towns.noboston,towns.onlyboston) %>%
#   mutate(Town=str_to_title(id, locale = "en"))
# write_csv(townsgeo_withbostonneigh, "townsgeo_withbostonneigh.csv")
# 
# #MA towns with boston as whole city
# townsgeo<-towns.df %>%
#   mutate(Town=str_to_title(id, locale = "en"))
# write_csv(townsgeo %>% filter(order<250000), "townsgeo1.csv")  #splitting to two files to keep <25mb
# write_csv(townsgeo %>% filter(order>=250000), "townsgeo2.csv") #splitting to two files to keep <25mb
# 
# #past code
# # zips_WSG84 <- spTransform(zips, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# # zips_tidy <- tidy(zips_WSG84, region = c('POSTCODE'))
# # write_csv(zips_tidy, "zips_tidy.csv")

# ## geograhic labels
# 
# townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
# townsgeo<-read.csv("townsgeo.csv") 
# matownsgeo<-rbind(townsgeo, townsgeobos)
# 
# #get centroids
# library(data.table)
# library(geosphere)
# cent<- matownsgeo %>% 
#   dplyr::select(long, lat, Town)
# setDT(cent)
# 
# #make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
# findCentroid <- function(long, lat, ...){
#   centroid(cbind(long, lat), ...)
# }
# 
# centroids<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
# centroids<-centroids %>%
#   distinct(Town, xname, yname) %>%
#   mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
#          yname= ifelse(Town=="Nantucket", yname-0.02, yname),
#          yname= ifelse(Town=="Chilmark", yname+0.02, yname),
#          yname= ifelse(Town=="Boston", yname-0.02, yname),
#          xname= ifelse(Town=="Boston", xname+0.04, xname),
#          xname= ifelse(Town=="Dorchester Uphams", xname-0.04, xname),
#          xname= ifelse(Town=="South Boston", xname-0.03, xname),
#          xname= ifelse(Town=="Hull", xname-0.05, xname),
#          xname= ifelse(Town=="Salem", xname-0.02, xname),
#          xname= ifelse(Town=="Westport", xname-0.02, xname),
#          yname= ifelse(Town=="Provincetown", yname+0.01, yname),
#          yname= ifelse(Town=="South Boston", yname-0.01, yname))
# 
# write_csv(centroids, "town.neighborhood.labels.csv")
# write_csv(centroids, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/town.neighborhood.labels.csv")

## vaccination reports from MDPH

# vaccine by zipcode by age group table
vaccine.anydose.age<-(bind_rows(
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-21-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-09-22")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-14-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-09-15")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-7-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-09-08")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-31-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-09-01")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-24-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-08-25")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-17-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-08-18")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-10-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-08-11")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-3-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-08-04")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-27-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip", "skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-07-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-20-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                           "skip","skip", "skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                           "age50to59", "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                           "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-07-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-14-2022.xlsx",
             skip = 2, 
             col_types = c("text", 
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                                      "numeric","numeric", "numeric",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric",
                           "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip",
                                      "skip","skip", "skip",
                           "numeric", "numeric", "numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  
                           "numeric","numeric", "numeric"),
             col_names = c("zipcode", 
                           "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", 
                                  "age50to59", "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age0to4.full", "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", 
                                  "age50to59.full", "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age0to4.boost", "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                                  "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-07-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-7-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-07-07")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-30-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age0to4","age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-06-30")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-23-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-06-23")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-16-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-06-16")) %>% relocate(reportdate),

  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-9-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-06-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-2-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-06-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-26-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-05-26")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-19-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-05-19")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-12-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-05-12")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - Zipcode", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-5-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-05-05")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-28-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-04-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-21-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-04-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-14-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-04-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-7-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-04-07")) %>% relocate(reportdate),
  
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-31-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-03-31")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-24-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-03-24")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-17-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-03-17")) %>% relocate(reportdate),

  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-10-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-03-10")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-3-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-03-03")) %>% relocate(reportdate),
    
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-24-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-02-24")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-17-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-02-17")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-10-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-02-10")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-3-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-02-03")) %>% relocate(reportdate),
  
   read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-27-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-01-27")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-20-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-01-20")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-13-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-01-13")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-6-2022.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2022-01-06")) %>% relocate(reportdate),

  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-30-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-12-30")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-23-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric","numeric","numeric", "numeric","numeric",  "numeric","numeric"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full",
                           "age5to11.boost", "age12to15.boost", "age16to19.boost", "age20to29.boost", "age30-39.boost", "age40to49.boost", "age50to59.boost",
                           "age60to64.boost", "age65-69.boost", "age70to74.boost", "age75to79.boost", "age80plus.boost")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-12-23")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-16-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-12-16")) %>% relocate(reportdate),
   
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-9-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-12-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-2-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-12-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-25-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-11-25")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-18-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-11-18")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-11-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age5to11", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age5to11.full", "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           reportdate=as.Date("2021-11-11")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-4-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-11-04")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-28-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-10-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-21-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-10-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-14-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-10-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-7-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-10-07")) %>% relocate(reportdate),
   
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-30-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-09-30")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-23-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-09-23")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-16-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-09-16")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-9-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-09-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-2-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-09-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-26-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-08-26")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-19-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-08-19")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-12-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-08-12")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-5-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-08-05")) %>% relocate(reportdate),
  
  
    read_excel(sheet = "Age – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-29-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-07-29")) %>% relocate(reportdate),
    
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-22-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-07-22")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-15-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-07-15")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-8-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-07-08")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-1-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-07-01")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-24-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-06-24")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-17-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-06-17")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-10-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-06-10")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-3-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-06-03")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-27-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-05-27")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-20-2021.xlsx",
             skip = 2, 
             col_types = c("text", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip","skip","skip","skip","skip", "skip","skip","skip","skip","skip","skip"),
             col_names = c("zipcode", "age12to15", "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age12to15.full", "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           reportdate=as.Date("2021-05-20")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-13-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-05-13")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-06-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-05-06")) %>% relocate(reportdate), 
    
    read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-29-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-04-29")) %>% relocate(reportdate), 
  
     read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-22-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-04-22")) %>% relocate(reportdate), 
  
    read_excel(sheet = "Age - zip code", 
                        "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-15-2021.xlsx",
                        skip = 2, 
               col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                             "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                             "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
               col_names = c("zipcode", 
                             "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                             "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                             "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                             "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
         reportdate=as.Date("2021-04-15")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-08-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-04-08")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-01-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-04-01")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-25-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-03-25")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-18-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-03-18")) %>% relocate(reportdate), 
  
  read_excel(sheet = "Age - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-11-2021.xlsx",
             skip = 2, 
             col_types = c("text",  "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "numeric","numeric",  "numeric","numeric", "numeric","numeric",  "numeric","numeric",
                           "numeric","numeric", "skip", "skip","skip","skip","skip","skip", "skip","skip","skip","skip"),
             col_names = c("zipcode", 
                           "age16to19", "age20to29", "age30-39", "age40to49", "age50to59",
                           "age60to64", "age65-69", "age70to74", "age75to79", "age80plus",
                           "age16to19.full", "age20to29.full", "age30-39.full", "age40to49.full", "age50to59.full",
                           "age60to64.full", "age65-69.full", "age70to74.full", "age75to79.full", "age80plus.full"))%>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           age5to11 = as.numeric(NA), age5to11.full = as.numeric(NA),
           age12to15 = NA,age12to15.full = NA,
           reportdate=as.Date("2021-03-11")) %>% relocate(reportdate)) %>% 
    rowwise %>% filter(!is.na(zipcode)) %>%
    ungroup()
  )
save(vaccine.anydose.age, file="vaccine.anydose.age.Rdata")

save(vaccine.anydose.age, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/vaccine.anydose.age.Rdata")

########################################

########################################

########################################

########################################

########################################

# vaccine by zipcode by sex/gender table (including other)
vaccine.sex<-(bind_rows(
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-21-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-09-22")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-14-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-09-15")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-7-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-09-08")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-31-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-09-01")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-24-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-08-25")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-17-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-08-18")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-10-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-08-11")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-3-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-08-04")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-27-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-07-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-20-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-07-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-14-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-07-14")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-7-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-07-07")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-30-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-06-30")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-23-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-06-23")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-16-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-06-16")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-9-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-06-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-2-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-06-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-26-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-05-26")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-19-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-05-19")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-12-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-05-12")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - Zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-5-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-05-05")) %>% relocate(reportdate),
    
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-28-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-04-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-21-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-04-21")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-14-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-04-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-7-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-04-07")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-31-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-03-31")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-24-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-03-24")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-17-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-03-17")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-10-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-03-10")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-3-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-03-03")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-24-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-02-24")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-17-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-02-17")) %>% relocate(reportdate),

  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-10-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-02-10")) %>% relocate(reportdate),

  read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-3-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-02-03")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-27-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-01-27")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex – zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-20-2022.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip",
                                     "numeric", "numeric", "numeric"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other",
                           "boost.female", "boost.male", "boost.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2022-01-20")) %>% relocate(reportdate),
  
      read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-13-2022.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip",
                                       "numeric", "numeric", "numeric"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other",
                             "boost.female", "boost.male", "boost.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2022-01-13")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-6-2022.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip",
                                       "numeric", "numeric", "numeric"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other",
                             "boost.female", "boost.male", "boost.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2022-01-06")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-30-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip",
                                       "numeric", "numeric", "numeric"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other",
                             "boost.female", "boost.male", "boost.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-12-30")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-23-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip",
                                       "numeric", "numeric", "numeric"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other",
                             "boost.female", "boost.male", "boost.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             boostvax.total=sum(c(boost.female, boost.male, boost.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-12-23")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-16-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-12-16")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-9-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-12-09")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-2-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-12-02")) %>% relocate(reportdate),
    
      read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-25-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-11-25")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-18-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-11-18")) %>% relocate(reportdate),
    
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-11-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-11-11")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-4-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-11-04")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-28-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-10-28")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-21-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-10-21")) %>% relocate(reportdate),
    
        read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-14-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-10-14")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-7-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-10-07")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-30-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-09-30")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-23-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-09-23")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-16-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-09-16")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-9-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-09-09")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-2-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-09-02")) %>% relocate(reportdate),
     
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-26-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-08-26")) %>% relocate(reportdate),
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-19-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-08-19")) %>% relocate(reportdate), 
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-12-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-08-12")) %>% relocate(reportdate), 

    read_excel(sheet = "Sex - zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-5-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-08-05")) %>% relocate(reportdate), 
    
    read_excel(sheet = "Sex – zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-29-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-07-29")) %>% relocate(reportdate), 
    
        read_excel(sheet = "Sex - zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-22-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-07-22")) %>% relocate(reportdate), 

    read_excel(sheet = "Sex - zip code", 
               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-15-2021.xlsx", 
               skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "skip", "skip", "skip"),
               col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                             "fullvax.female", "fullvax.male", "fullvax.other")) %>%
      rowwise(zipcode) %>%
      mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
             fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
             anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
             reportdate=as.Date("2021-07-15")) %>% relocate(reportdate),  
    
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-8-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-07-08")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-1-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-07-01")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-24-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-06-24")) %>% relocate(reportdate),
  
    read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-17-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-06-17")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-10-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-06-10")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-3-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-06-03")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-27-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-05-27")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-20-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-05-20")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-13-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-05-13")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-06-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-05-06")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-29-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-04-29")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-22-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-04-22")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
                        "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-15-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
         reportdate=as.Date("2021-04-15")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-08-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-04-08")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-01-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-04-01")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-25-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-03-25")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-18-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-03-18")) %>% relocate(reportdate),
  
  read_excel(sheet = "Sex - zip code", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-11-2021.xlsx", 
             skip = 2, col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "skip", "skip"),
             col_names = c("zipcode", "anyvax.female", "anyvax.male", "anyvax.other",
                           "fullvax.female", "fullvax.male", "fullvax.other")) %>%
    rowwise(zipcode) %>%
    mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode),
           fullvax.total=sum(c(fullvax.female, fullvax.male, fullvax.other), na.rm = TRUE),
           anyvax.total=sum(c(anyvax.female, anyvax.male, anyvax.other), na.rm = TRUE),
           reportdate=as.Date("2021-03-11")) %>% relocate(reportdate)
  ) %>% 
  rowwise %>% filter(!is.na(zipcode)) %>%
    ungroup())

save(vaccine.sex, file="vaccine.sex.Rdata")


save(vaccine.sex, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/vaccine.sex.Rdata")



########################################

########################################

########################################

########################################

########################################


vaccine.town<-bind_rows(
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-21-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-09-22")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-14-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-09-15")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-7-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-09-08")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-31-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-09-01")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-24-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-08-25")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-17-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-08-18")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-10-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-08-11")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-3-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-08-04")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-27-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-07-28")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-20-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-07-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-14-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-07-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-7-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-07-07")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-30-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-06-30")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-23-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-06-23")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-16-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-06-16")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-9-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-06-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-2-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-06-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-26-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-05-26")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-19-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-05-19")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-12-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-05-12")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-5-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-05-05")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-28-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-04-28")) %>% relocate(reportdate),

  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-21-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-04-21")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-14-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-04-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-7-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-04-07")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-31-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-03-31")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-24-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-03-24")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-17-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-03-17")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-10-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-03-10")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-3-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-03-03")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-24-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-02-24")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-17-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-02-17")) %>% relocate(reportdate),

  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-10-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-02-10")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-2-3-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-02-03")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - muncipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-27-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-01-27")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-20-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-01-20")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-13-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-01-13")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-1-6-2022.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2022-01-06")) %>% relocate(reportdate),
  
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-30-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2021-12-30")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-23-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip", "numeric","skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax", "boostvax")) %>%
    mutate(reportdate=as.Date("2021-12-23")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-16-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-12-16")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-9-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-12-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-12-2-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-12-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-25-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-11-25")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-18-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-11-18")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-11-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-11-11")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-11-4-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-11-04")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-28-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-10-28")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-21-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-10-21")) %>% relocate(reportdate),
  
    read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-14-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-10-14")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-10-7-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-10-07")) %>% relocate(reportdate),

  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-30-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-09-30")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-23-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-09-23")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-16-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-09-16")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-9-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-09-09")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-2-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-09-02")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-26-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-08-26")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-19-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-08-19")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age – municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-12-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-08-12")) %>% relocate(reportdate),
  
  read_excel(sheet = "Age - municipality", 
             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-8-5-2021.xlsx", 
             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
    mutate(reportdate=as.Date("2021-08-05")) %>% relocate(reportdate),
    
            read_excel(sheet = "Age – municipality", 
                       "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-29-2021.xlsx", 
                       skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                       col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
              mutate(reportdate=as.Date("2021-07-29")) %>% relocate(reportdate),
              
              read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-22-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                mutate(reportdate=as.Date("2021-07-22")) %>% relocate(reportdate),
  
                read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-15-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                mutate(reportdate=as.Date("2021-07-15")) %>% relocate(reportdate),
  
                read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-8-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                mutate(reportdate=as.Date("2021-07-08")) %>% relocate(reportdate),
  
                read_excel(sheet = "Age - municipality", 
                           "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-1-2021.xlsx", 
                           skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                           col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                  mutate(reportdate=as.Date("2021-07-01")) %>% relocate(reportdate),
  
                  read_excel(sheet = "Age - municipality", 
                           "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-24-2021.xlsx", 
                           skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                           col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                  mutate(reportdate=as.Date("2021-06-24")) %>% relocate(reportdate),
  
                  read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-17-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                mutate(reportdate=as.Date("2021-06-17")) %>% relocate(reportdate),
  
                read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-10-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                mutate(reportdate=as.Date("2021-06-10")) %>% relocate(reportdate),
    
                read_excel(sheet = "Age - municipality", 
                           "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-6-3-2021.xlsx", 
                           skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                           col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                  mutate(reportdate=as.Date("2021-06-03")) %>% relocate(reportdate),
  
                read_excel(sheet = "Age - municipality", 
                           "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-27-2021.xlsx", 
                           skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                           col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                  mutate(reportdate=as.Date("2021-05-27")) %>% relocate(reportdate), 
  
                read_excel(sheet = "Age - municipality", 
                           "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-20-2021.xlsx", 
                           skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                           col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                  mutate(reportdate=as.Date("2021-05-20")) %>% relocate(reportdate), 
  
                  read_excel(sheet = "Age - municipality", 
                             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-13-2021.xlsx", 
                             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                    mutate(reportdate=as.Date("2021-05-13")) %>% relocate(reportdate), 
  
                    read_excel(sheet = "Age - municipality", 
                             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-5-06-2021.xlsx", 
                             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                    mutate(reportdate=as.Date("2021-05-06")) %>% relocate(reportdate),     
            
                     read_excel(sheet = "Age - municipality", 
                             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-29-2021.xlsx", 
                             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                             mutate(reportdate=as.Date("2021-04-29")) %>% relocate(reportdate),
            
                    read_excel(sheet = "Age - municipality", 
                             "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-22-2021.xlsx", 
                             skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                             col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                              mutate(reportdate=as.Date("2021-04-22")) %>% relocate(reportdate),
            
                    read_excel(sheet = "Age - municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-15-2021.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                         col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                          mutate(reportdate=as.Date("2021-04-15")) %>% relocate(reportdate),
                    
                    read_excel(sheet = "Age - municipality", 
                               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-08-2021.xlsx", 
                               skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                               col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                             mutate(reportdate=as.Date("2021-04-08")) %>% relocate(reportdate),
                    
                    read_excel(sheet = "Age - municipality", 
                               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-4-01-2021.xlsx", 
                               skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                               col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                              mutate(reportdate=as.Date("2021-04-01")) %>% relocate(reportdate),
                    
                    read_excel(sheet = "Age - municipality", 
                               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-25-2021.xlsx", 
                               skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                               col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                              mutate(reportdate=as.Date("2021-03-25")) %>% relocate(reportdate),
                    
                    read_excel(sheet = "Age - municipality", 
                               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-18-2021.xlsx", 
                               skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                               col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                              mutate(reportdate=as.Date("2021-03-18")) %>% relocate(reportdate),
                    
                    read_excel(sheet = "Age - municipality", 
                               "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-3-11-2021.xlsx", 
                               skip = 2, col_types = c("skip", "text", "text", "numeric", "numeric", "numeric", "skip", "skip", "numeric","skip", "skip","skip", "skip","skip"),
                               col_names = c("Town","Age_group", "pop", "proportion_pop", "onedose", "fullvax")) %>%
                              mutate(reportdate=as.Date("2021-03-11")) %>% relocate(reportdate)) 
save(vaccine.town, file="vaccine.town.Rdata")

save(vaccine.town, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/vaccine.town.Rdata")


vaccine.race<-read_excel(sheet = "Race & Ethnicity – municipality", 
                         "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-9-21-2022.xlsx", 
                         skip = 2, col_types = c("skip", "text", "text", "numeric", "skip", "skip", "skip", 
                                                 "skip", "numeric","skip", "skip","skip", "skip","skip","numeric", "skip","skip"),
                         col_names = c("Town","Race_eth", "pop", "fullvax", "boostvax")) %>%
  mutate(Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")) #remove parenthetic phrase

save(vaccine.race, file="vaccine.race.Rdata")

# vaccine.race<- vaccine.race %>%
#   filter(Race_eth=="Total" | Race_eth=="Black" | Race_eth=="Hispanic") %>%
#   pivot_wider(names_from = Race_eth, values_from = pop) %>%
#   mutate(blacklatinx.pct=(Black + Hispanic)/ Total,
#          Town=str_replace(Town, " \\s*\\([^\\)]+\\)", ""))  %>% #remove parenthetic phrase
#   dplyr::select(Town, blacklatinx.pct)
# save(vaccine.race, file="vaccine.race.Rdata")

# save(vaccine.race, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/vaccine.race.Rdata")


#vaccine by race and zip code
  #these do not really work as small numbers are suppressed for confidentiality so column totals are much smaller than actual totals
      # vaccine.race.zip<-read_excel(sheet = "Race and Ethnicity - zip code", 
      #          "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-15-2021.xlsx", 
      #          skip = 3, col_types = c("text", "numeric", "numeric", "numeric", "numeric", 
      #                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
      #                                  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
      #                                  "numeric", "numeric", "numeric", "numeric", "numeric"),
      #          col_names = c('zipcode', 
      #           'anydose.AIAN',	'anydose.Asian',	'anydose.Black',	'anydose.Hispanic',	'anydose.Multi',	'anydose.NHPI',	'anydose.Other', 'anydose.White', 'anydose.Unknown',
      #           'fullvax.AIAN',	'fullvax.Asian',	'fullvax.Black',	'fullvax.Hispanic',	'fullvax.Multi',	'fullvax.NHPI',	'fullvax.Other', 'fullvax.White', 'fullvax.Unknown',	
      #           'partvax.AIAN',	'partvax.Asian',	'partvax.Black',	'partvax.Hispanic',	'partvax.Multi',	'partvax.NHPI',	'partvax.Other', 'partvax.White', 'partvax.Unknown')) %>%
      #     mutate(zipcode= if_else(nchar(zipcode)<5, paste0(0,zipcode), zipcode))
      #   
      # vaccine.race.town<-read_excel(sheet = "Race and Ethnicity - muni.", 
      #    "~/Dropbox (Partners HealthCare)/GitHub/MA-MGB-Covid-Analyses/weekly-covid-19-municipality-vaccination-report-7-9-2021.xlsx", 
      #    skip=1, col_types = c("text", "text","text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      #                          "numeric", "numeric", "numeric", "numeric", "numeric")) %>% janitor::clean_names() %>%
      #   group_by(race_ethnicity) %>%
      #   summarise(anydose= sum(individuals_with_at_least_one_dose, na.rm = TRUE))

# # library(readxl)
# # library(lubridate)
# # # cases by race/ethnicity from weekly .xlsx
# # CasesRace_long <-read_excel(sheet = "RaceEthnicityLast2Weeks", 
# #            mdphfile) %>%  #using longitudinal report when available
# #   rename(race = 'Race/Ethnicity',
# #          Count = 'All Cases',
# #          Death = 'Deaths',
# #          Hospitalized = 'Ever Hospitaltized',
# #          date = 'Date',
# #          MDPHpopulation = Population_Estimate_N) %>%
# #   mutate(date = ymd(as.Date(date)),
# #          Start_Date = ymd(as.Date(Start_Date)),
# #          End_Date = ymd(as.Date(End_Date)), 
# #          Count = if_else(Count == "<5", 3, as.numeric(Count)),
# #          dayofweek = lubridate::wday(date),
# #          race = fct_collapse(race,
# #                              "other.multi" = c("American Indian/Alaskan Native, non-Hispanic",
# #                                                "Unknown, missing, or refused",
# #                                                "Native Hawaiian/ Pacific Islander, non-Hispanic",
# #                                                "Unknown, missing, or refused",
# #                                                "Unknown or missing",
# #                                                "Other race, non-Hispanic"),
# #                              "asian" = c("Asian, non-Hispanic"),
# #                              "black" = c("Black or African American, non-Hispanic"),
# #                              "latinx" = c("Hispanic"),
# #                              "white" = c("White, non-Hispanic"))
# #                         ) %>%
# #   filter(dayofweek == 4) %>%  #selecting weekly reports, prior to 4 Nov 2020 produced daily
# #   group_by(date, race) %>%
# #     summarize( Count = sum(Count),
# #                Hospitalized = sum(Hospitalized),
# #                MDPHpopulation = mean(MDPHpopulation),
# #                Start_Date = min(Start_Date),
# #                End_Date = min(End_Date)) %>%
# #   select(date, race, Count, Hospitalized, MDPHpopulation, Start_Date, End_Date)
# 
# CasesRace <- CasesRace_long %>% select(date, race, Count) %>%
#   pivot_wider(names_from = race, values_from = Count)
# 
# ## read in data from weekly printed report, used in vaccine equity analysis
#  CasesRace.equity<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="CasesRace") %>%
#    mutate(date=lubridate::ymd(date)) 
# 
# VaxRace<-read_excel("~/Dropbox (Partners HealthCare)/R files/covid/MGB census.xlsx", sheet="MDPHvaccine") %>%
#   mutate(date=lubridate::ymd(date)) %>%
#   dplyr::select(date, AI.AN:Unknown) %>%
#   filter(!is.na(AI.AN))
# 
# save(CasesRace, file="CasesRace.Rdata")
# save(CasesRace_long, file="CasesRace_long.Rdata")
# write_csv()
# save(VaxRace, file="VaxRace.Rdata")
# 
# save(CasesRace.equity, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/CasesRace.Rdata")
# save(VaxRace, file="~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/VaxRace.Rdata")


## vaccination by children


# School age population by zip code
# library(tidycensus)
# v2019 <- load_variables(2019, "acs5", cache = TRUE)
# 
# vars <- c(
#     "B01001_003",  #male, 0-4
#   "B01001_004",  #male, 5-9
#   "B01001_005",  #male, 10-14
#   "B01001_006",  #male, 15-17
#   "B01001_007",  #male, 18-19
#   "B01001_027",  #female, 0-4
#   "B01001_028",  #female, 5-9
#   "B01001_029",  #female, 10-14
#   "B01001_030",  #female, 15-17
#   "B01001_031"  #female, 18-19
# )
# ALLzips.age <-get_acs(geography = "zcta",
#                       variables = vars,
#                       #state = "25", # cannot limit to state in ZCTA
#                       year = 2019)
# 
# save(ALLzips.age , file="ALLzips.age.Rdata")

## get vaccinations for school age for MA
load("ALLzips.age.Rdata")
boston.zips<-c("02163",
               "02135","02134","02129","02108","02114","02116","02199",
               "02109","02110","02103","02113","02128","02121","02125","02122","02124","02115","02215",
               "02136","02130","02126","02131","02119","02120","02127","02210","02111","02118","02132")
 

 # non-Boston towns
vaccine.schoolage<- rbind(  vaccine.town %>%
    filter(Town !=" Boston") %>%
    filter(Age_group == "0-4 Years" | Age_group == "5-11 Years" | Age_group == "12-15 Years" | Age_group == "16-19 Years" | Age_group == "0-19 Years" ) %>%
  select(-proportion_pop),
  #Boston neighborhoods

  left_join(
  left_join(
 vaccine.anydose.age %>%
      filter(zipcode %in% boston.zips) %>%
      pivot_longer(cols = c(age5to11:age80plus.boost), names_to = "Age_group", values_to = "vaxnum") %>%
      mutate(onedose = if_else(str_detect(Age_group, ".full"), as.numeric(NA), vaxnum),
             fullvax = if_else(str_detect(Age_group, ".full"), vaxnum, as.numeric(NA)),
             boostvax = if_else(str_detect(Age_group, ".boost"), vaxnum, as.numeric(NA)),
             Age_group = case_when(
               Age_group == "age0to4" ~ "age0to4",
               Age_group == "age0to4.full" ~ "age0to4",
               Age_group == "age0to4.boost" ~ "age0to4",
               Age_group == "age5to11" ~ "age5to11",
               Age_group == "age5to11.full" ~ "age5to11",
               Age_group == "age5to11.boost" ~ "age5to11",
               Age_group == "age12to15" ~ "age12to15",
               Age_group == "age12to15.full" ~ "age12to15",
               Age_group == "age12to15.boost" ~ "age12to15",
               Age_group == "age16to19" ~ "age16to19",
               Age_group == "age16to19.full" ~ "age16to19",
               Age_group == "age16to19.boost" ~ "age16to19")) %>% select(reportdate, zipcode,  Age_group, onedose, fullvax, boostvax) %>%
      filter(!is.na(Age_group)) %>%
      group_by(reportdate, zipcode, Age_group) %>% 
      summarize(onedose = sum(onedose, na.rm=TRUE), 
                fullvax = sum(fullvax, na.rm=TRUE),
                boostvax = sum(boostvax, na.rm=TRUE)) %>%
      ungroup(),
ALLzips.age %>%
            rename(zipcode=GEOID,
                   pop = estimate)%>%
            filter(zipcode %in% boston.zips) %>%
            ungroup() %>%
            # mutate(pop = if_else(variable == "B01001_005" |variable == "B01001_029", pop*2/5, pop), # splitting age group 10-14 into two years 
            #        pop = if_else(variable == "B01001_006" |variable == "B01001_030", pop*2/3, pop)) %>%  # splitting age group 15-17 into two years 
            filter(variable != "B01001_007" ) %>%  # not using 18 to 19 age estimates
            filter(variable != "B01001_031" ) %>%  # not using 18 to 19 age estimates
            mutate(
              age5to11 = case_when(
                variable == "B01001_004" ~ pop,  #5to9, 
                variable == "B01001_005" ~ 0.4*pop,  #10to11, 2/5 of 10to14
                variable == "B01001_028" ~ pop, #same for girls
                variable == "B01001_029" ~ 0.4*pop), #same for girls
              age12to15 = case_when(
                variable == "B01001_005" ~ 0.8*pop, #4 years from 10to14 
                variable == "B01001_029" ~ 0.8*pop), #same for girls
              age16to19 = case_when(
                variable == "B01001_006" ~ (4/3)*pop, # 4 years of 15-17,(mitigate effect of excess 18/19 college students)
                variable == "B01001_030" ~ (4/3)*pop)  ) %>% #same for girls
              group_by(zipcode) %>%
                summarise(age5to11 = round(sum(age5to11, na.rm = TRUE),0),
                          age12to15 = round(sum(age12to15, na.rm = TRUE),0),
                          age16to19 = round(sum(age16to19, na.rm = TRUE),0)) %>%
              pivot_longer(cols = c(age5to11:age16to19), names_to = "Age_group",values_to = "pop"), by=c("zipcode", "Age_group")),
      read_csv("MAtowns.csv") %>% 
              select(Town, zipcode) %>%
              mutate(zipcode = sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})),
            by = "zipcode") %>%
      group_by(Town, reportdate, Age_group) %>%
      summarise(pop = sum(pop, na.rm = TRUE),
                onedose = sum(onedose, na.rm = TRUE),
                fullvax = sum(fullvax, na.rm = TRUE),
                boostvax = sum(boostvax, na.rm = TRUE)) %>%
        ungroup()) %>%
      mutate(anydose.pct = onedose/pop, 
             fullvax.pct = fullvax/pop,
             boostvax.pct = boostvax/pop,
             Age_group = case_when(
               Age_group == "5-11 Years" ~ "5-11 Years",
               Age_group == "12-15 Years" ~ "12-15 Years",
               Age_group == "16-19 Years" ~ "16-19 Years",
               Age_group == "age5to11" ~ "5-11 Years",
               Age_group == "age12to15" ~ "12-15 Years",
               Age_group == "age16to19" ~ "16-19 Years"), 
       Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")) #remove parenthetic phrase) 
    
write_csv(vaccine.schoolage, "~/Dropbox (Partners HealthCare)/GitHub/MA-SARSCoV2-Epidemic-Response/vaccine.schoolage.csv")
save(vaccine.schoolage, file = "vaccine.schoolage.Rdata")

## Pediatric covid incidence
kids0to19 <-rbind(
  read_excel(sheet = "AgeLast2Weeks", 
                            mdphfile) %>%
  filter(Age == "5-9") %>%
  rename(date=Date) %>%
  mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
         date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
          as.Date("2022-01-06"), ymd(date)) ) %>%
  group_by(date) %>%
  summarise(cases2wk=sum(Cases_Last2Weeks),
            pop = sum(Population_Estimate_N)) %>%
  mutate(date = ymd(date),
         incidence1day= (cases2wk*100000/pop)/14,
         agecat = "Age 5 to 9"),
  read_excel(sheet = "AgeLast2Weeks", 
  mdphfile) %>%
  filter(Age == "10-14") %>%
  rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
  group_by(date) %>%
  summarise(cases2wk=sum(Cases_Last2Weeks),
            pop = sum(Population_Estimate_N)) %>%
  mutate(date = ymd(date),
         incidence1day= (cases2wk*100000/pop)/14,
         agecat = "Age 10 to 14"),
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "<5") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 0 to 4"),
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "15-19") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 15 to 19"))
save(kids0to19, file="kids0to19.Rdata")

## By Age covid incidence
age0to100 <-rbind(
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "<5" | Age == "5-9" | Age == "10-14" |Age == "15-19") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 0 to 19"),
  
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "20-29") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 20 to 29"),
  
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "30-39" | Age == "40-49") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 30 to 49"),
  
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "50-59" | Age == "60-69") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 50 to 69"),
  
  read_excel(sheet = "AgeLast2Weeks", 
             mdphfile) %>%
    filter(Age == "70-79" | Age == "80+") %>%
    rename(date=Date) %>%
    mutate(date = if_else(date== as.Date("2022-01-07") | date== as.Date("2022-01-08") |
                            date== as.Date("2022-01-09") | date== as.Date("2022-01-10") | date== as.Date("2022-01-09"), 
                          as.Date("2022-01-06"), ymd(date)) ) %>%
    group_by(date) %>%
    summarise(cases2wk=sum(Cases_Last2Weeks),
              pop = sum(Population_Estimate_N)) %>%
    mutate(date = ymd(date),
           incidence1day= (cases2wk*100000/pop)/14,
           agecat = "Age 70+"))

save(age0to100, file="age0to100.Rdata")

## breakthrough hosp
mdph_hosp <-left_join(read_excel(sheet = "Hospitalization from Hospitals", 
                       mdphfile) %>%
    clean_names() %>%
  rename(inpatient.total=total_number_of_covid_patients_in_hospital_today, inpatient.vax=vaccinated_covid_hospitalizations,
         inpatient7d.avg = x7_day_average_of_covid_hospitalizations) %>%
  mutate(inpatient.unvax = inpatient.total-inpatient.vax,
         pct.vax = inpatient.vax/inpatient.total,
         date = ymd(date)) %>%
  filter(date != as.Date("2020-07-22")) %>%
  select(date, inpatient7d.avg,inpatient.unvax, inpatient.vax, inpatient.total, pct.vax, intubated),
  read_excel(sheet = "HospBedAvailable-Regional", 
             mdphfile) %>%
    clean_names() %>%
    rowwise()%>%
    mutate(date = ymd(date),
           inpatients = sum(c(occupied_icu, occupied_medical_surgical, na.rm = TRUE))) %>%
    group_by(date) %>%
    summarize(inpatients= sum(inpatients)) %>% ungroup(), by="date") %>% ungroup() %>%
  mutate(pct.covid = inpatient.total*100/inpatients,
         pct.btcovid = inpatient.vax*100/inpatients)
save(mdph_hosp, file="mdph_hosp.Rdata") 

## deaths
mdph.deaths <-read_excel(sheet = "DateofDeath", 
                                 mdphfile) %>%
  janitor::clean_names() %>%
  mutate(date= ymd(date_of_death),
         deaths7d.avg = x7_day_confirmed_death_average) %>%
  select(date, confirmed_deaths, deaths7d.avg) %>%
  filter(date < max(date-1))
save(mdph.deaths, file="mdph.deaths.Rdata") 

## Wastewater
sewage <-rbind(read_excel("~/Dropbox (Partners HealthCare)/R files/Covid/MGB census.xlsx", sheet="MWRA")%>%
                 select(c("date", "South", "North", "B117")) %>%
                 mutate(date=lubridate::ymd(date)) %>%
                 rowwise() %>% 
                 mutate(mean=if_else(is.na(South) & is.na(North), as.numeric(NA), mean(c(South, North), na.rm=TRUE)),
                        year = "Current") %>%
                 ungroup() %>%
                 select(date, year, mean),
               read_excel("~/Dropbox (Partners HealthCare)/R files/Covid/MGB census.xlsx", sheet="MWRA")%>%
                 select(c("date", "South", "North", "B117")) %>%
                 mutate(date=lubridate::ymd(date)+365) %>%
                 rowwise() %>% 
                 mutate(mean=if_else(is.na(South) & is.na(North), as.numeric(NA), mean(c(South, North), na.rm=TRUE)),
                        year = "Last Year") %>%
                 ungroup() %>%
                 select(date, year, mean)) %>%
  mutate(year = fct_relevel(year, "Last Year", "Current"))
save(sewage, file="mwra.wastewater.covid.Rdata") 

#clear environment
rm(list=ls())

#if need to update GitHub access token
# library("credentials")
# library("askpass")
# askpass("Please enter your password")
# credentials::set_github_pat()
# gitcreds::gitcreds_set()
# /Library/Frameworks/R.framework/Versions/4.1.1-arm64/Resources/library/credentials/ask_token.sh
# 
# getOption()
# Sys.getenv()
# ?rpostback-askpass()
###end

