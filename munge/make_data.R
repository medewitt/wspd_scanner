# Purpose: Make the Final Data Set


# libraries ---------------------------------------------------------------


library(tidyverse)
library(sf)
library(ggmap)
library(rgdal)
# bring in files ----------------------------------------------------------
#bring together

geocoded_address <- read_csv("data/2018-10-22_geocoded_incidents.csv",
                             col_types = cols(unique_id = col_integer(),
                                              .default = col_character()))

geocoded_address <- geocoded_address %>%
  bind_rows() %>% 
  unique() %>% 
  filter(!is.na(type)) %>% 
  separate(lat_lon, c("lon", "lat"),sep = "," )

# Tie back to crime data

incidents <- read_csv("data/2018-10-21_wspd_crime_report.csv") %>% 
  mutate(unique_id = row_number())

# Tie to Census Information
incident_geocoded <- incidents %>% 
  left_join(geocoded_address %>% 
              select(-address, -response, -type), by = "unique_id")

# Tie to Ward Information
wards <- st_read("data/wards_20150327.shp")

wards_2 <- st_transform(wards, crs = 4326)

wards_2 %>% 
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  )->wards_3

pnts <- incident_geocoded %>% 
  select(lon, lat) %>% 
  mutate_if(is.character, as.numeric)

map2(pnts$lon, pnts$lat, ~st_point(c(.x, .y))) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(pnts[,-(1:2)], .) -> centers_sf

bind_cols(
  incident_geocoded,
  wards_3[as.numeric(st_within(centers_sf, wards_3)),]
) %>% 
  select(incident_geocoded,lon, lat, Ward) %>% 
  mutate(Ward_name = str_to_title(Ward))->out_with_ward

incident_geocoded <- incident_geocoded %>%
  bind_cols(out_with_ward)


# bring in census data ----------------------------------------------------

library(tidycensus)
tidycensus::census_api_key(census_key)

county_pov <- get_acs(geography = "tract",
                      variables = "B17001_002",
                      summary_var = "B17001_001",
                      state = "NC",
                      geometry = FALSE) %>% 
  mutate(pctpov = 100* estimate/ summary_est)

white <- get_acs(geography = "tract",
                      variables = "B01001A_001",
                      state = "NC",
                 summary_var = "B01003_001",
                      geometry = FALSE) %>% 
  rename(n_white = estimate) %>% 
  mutate(pctwhite = n_white/summary_est*100) %>% 
  rename(population = summary_est)

names(county_pov)
tidycensus::load_variables(year = "2016", dataset = "acs5")->b
incident_geocoded %>% 
  mutate(GEOID = paste0(state_fips,county_fips,tract)) %>% 
  left_join(county_pov) %>% 
  left_join(white %>% select(GEOID, n_white, pctwhite, population)) %>% 
  filter(!is.na(return_address))->a

write_csv(a, "data/geocoded_incidents_with_census_data.csv")

            