library(tidyverse)
library(purrr)

url <- "http://www1.aoc.state.nc.us/www/data/FORSYTH/calendar/DISTRICT.DISTRICT_COURT_.09.25.18.AM.001A.CAL.txt"


read_lines(file = url, skip = 20, n_max = 200)->a
a

str_extract_all(string = a, pattern = "\\s(\\w{4}\\s\\d{6})\\s")->b
str_extract_all(string = a, pattern = "\\s(\\w{6})\\s")->c
str_extract_all(string = a, pattern = "\\((.)+PLEA")->d

bind_rows(b[!is.na(b)])
b[!is.na(b)]
lapply(b, function(x) x[!is.na(x)])
case_num <-purrr::compact(b)
case_num <- map(case_num, str_trim)

enframe(case_num) %>% 
  unnest()
