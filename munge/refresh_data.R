# Purpose: Go get the data from the WSPD link
library(tidyverse)
library(lubridate)
library(RCurl)


# go to web ---------------------------------------------------------------

base_url <- "http://www.cityofws.org/crimestats/txt/WSPD"

dates_to_get <-expand.grid(mon = 1:12, day = 1:31) %>% 
  mutate(mon = ifelse(mon < 10, paste0("0",mon), mon),
         day = ifelse(day < 10, paste0("0",day), day),
         mon_day = paste0(mon,day))


urls_to_parse <- paste0(base_url,dates_to_get$mon_day,".txt")

collector <- list()

for(i in seq_along(urls_to_parse)){
  if(!url.exists(urls_to_parse[[i]])){
    next(i)
  }
  collector[[i]] <-read_fwf(urls_to_parse[[i]], 
                            fwf_widths(c(13, 7, 5, 31, 45), c("case", "date", "time", "call_type", "address"))) %>% 
    filter(!is.na(case))
  Sys.sleep(.1)
}

clean_up_dates <- function(df){
  df %>% 
    mutate(date = str_trim(date, side = "both")) %>% 
    mutate(time = str_trim(time, side = "both")) %>%
    mutate(address = str_trim(address, side = "both")) %>%
    mutate(call_type = str_trim(call_type, side = "both")) %>%
    mutate(incident_date = lubridate::mdy(date)) %>% 
    mutate(incident_time = format(strptime(time, format="%H%M"), format = "%H:%M")) %>% 
    mutate(incident_date_time = ymd_hm(paste(incident_date, incident_time))) %>% 
    select(-date, - time)
}

collector_1 <-plyr::compact(collector)

safely(clean_up_dates)->safe_clean

clean_df <- map_df(collector_1, clean_up_dates)

write_csv(clean_df, paste0("data/",Sys.Date(),"_wspd_crime_report.csv"))


# make census call --------------------------------------------------------

clean_df %>% 
  mutate(address = str_trim(address, "both")) %>% 
  dplyr::mutate(`Unique ID` = row_number()) %>% 
  dplyr::select(`Unique ID`, address) %>% 
  rename(`Street address` = address) %>% 
  mutate(city = "Winston-Salem", state = "NC", Zip = "") %>% 
  filter(between(`Unique ID`, left = 70000, 79000))-> census_format

write_csv(census_format, "data/census_format.csv")

clean_df %>% 
  mutate(address = str_trim(address, "both")) %>% 
  dplyr::mutate(`Unique ID` = row_number()) %>% 
  dplyr::select(`Unique ID`, address) %>% 
  rename(`Street address` = address) %>% 
  mutate(city = "Winston-Salem", state = "NC", Zip = "") %>% 
  filter(between(`Unique ID`, left = 60000, 69000))-> census_format_1

write_csv(census_format_1, "data/census_format_1.csv")

my_files <- list.files(path = "data", pattern = "geocoder", full.names = T)

geocoded_address <- map(my_files, read_csv, skip = 1, 
                           col_names = c("id", "address", "status", "type", "match_add", "latlon", "num", "let"),
                        col_types = cols(
                          id = col_integer(),
                          address = col_character(),
                          status = col_character(),
                          type = col_character(),
                          match_add = col_character(),
                          latlon = col_character(),
                          num = col_integer(),
                          let = col_character()
                        ))

