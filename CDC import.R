library(tidyjson)

https://data.cdc.gov/resource/8xkx-amqh.json

download.file("https://data.cdc.gov/resource/8xkx-amqh.json", "cdc_county_vax.json")

cdc.county<-"cdc_county_vax.json"

cdc.county %>% as.tbl_json

cdc.county %>% spread_all

## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")

df <- read.socrata(
  "https://data.cdc.gov/resource/8xkx-amqh.json",
  app_token = "YOURAPPTOKENHERE",
  email     = "user@example.com",
  password  = "fakepassword"
)