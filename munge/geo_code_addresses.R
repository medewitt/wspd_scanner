# Geocode Addresses

#Libraries

library(tidyverse)
library(httr)

# Bring in the data

df_1 <- read_csv("data/2018-10-21_wspd_crime_report.csv")

# Call

apiurl <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

#E Establish a temo file
file <- tempfile(fileext = ".csv")

addresses <- df_1 %>% 
  mutate(address = str_trim(address, "both")) %>% 
  dplyr::mutate(`Unique ID` = row_number()) %>% 
  dplyr::select(`Unique ID`, address) %>% 
  rename(`Street address` = address) %>% 
  mutate(city = "Winston-Salem", state = "NC", Zip = "")

seq_to_get <-seq(1, 160000,9500)
collector <- list()
for( i in 13:16){
  # Format for census API
  addresses %>% 
    filter(between(x = `Unique ID`, 
                   left = seq_to_get[i], 
                   right = seq_to_get[i+1]))-> census_format
  
  write_csv(census_format, "temp.csv")
  
  # Post the Call
  req <- POST(apiurl, body=list(
    addressFile = upload_file("temp.csv"), 
    benchmark = "Public_AR_Current",
    vintage = "Current_Current"
  ), 
  encode="multipart"
  )
  
  print(content(req, "text", encoding = "UTF-8"))
  # Receive Returned Call
  collector[[i]] <- read_csv(content(req, "text", encoding = "UTF-8"),
           col_names = c("unique_id", "address", "response", "type",
                         "return_address", "lat_lon", "tiger_id", "side",
                         "state_fips", "county_fips", "tract", "block"),
           col_types = cols(.default = col_character()))
  Sys.sleep(1)
}
# Save the Output
write_rds(collector, "data/one_17.RDS")
# Read back the output
collector <- readr::read_rds("data/one_17.RDS")
#Bind it
collector_flat <- dplyr::bind_rows(collector)
data.table::fwrite(collector_flat,"data/2018-10-22_geocoded_incidents.csv")

head(collector_flat, 5)->a
