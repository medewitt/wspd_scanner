## Data Dictionary

These data represent a combination of data from the Winston-Salem Police Department

## Data From Winston-Salem Police Department

* `case` - WSPD defined case ID number
* `call_type`- Type of call
* `address` - Address of the call
* `incident_date` - Date of the call
* `incide_time` - Time of the call
* `incident_date_time` - Combined date/ time 

## Geocoding Information from the Census

* `unique_id` - Unique ID used for census geocoding
* `return_address` - Address returned from US census geocoding
* `lon` - Longitude of the call
* `lat` - Latitude of the call
* `tiger_id` - Topologically Integrated Geographic Encoding and Referencing ID
* `side` - TIGER side of the road of the event
* `state_fips` - State FIPS code (all 37 for NC)
* `county_fips` - County FIPS code
* `tract` - Census Tract
* `block` - Census Block
* `lon1` - Longitude of the call
* `lat1` - Latitude of the call
* `Ward` - Winston-Salem Ward name in which the call occurred
* `Ward_name` - Duplicate Winston-Salem Ward name
* `GEOID` - GEOID from US Census
* `NAME` - Returned call from US Census Geocode

## American Community Survey Data

* `variable` - The number of people in poverty in NC (B17001_001)
* `estimate` - Estimated number of people living in poverty
* `moe` - Margin of Error for number of people living in poverty
* `summary_est` - Estimated number of people living in a given tract
* `summary_moe` - Estimated margin of error for people living in a given tract
* `pctpov` - Percentage of people living in poverty in a given tract
* `n_white` - Number of people identifying as White in a given tract
* `pctwhite` - Percentage of people identifying as white in a given tract
* `population` - Population estimate for a given tract

## Derived Data 
* `day_of_week` - Day of the week on which a call occurs
* `month` - The month that the incident occurred

