library(tidyverse)
library(purrr)


url <- "http://www1.aoc.state.nc.us/www/data/FORSYTH/calendar/DISTRICT.DISTRICT_COURT_.09.26.18.AM.0001.CAL.txt"

a <- read_lines(file = url, skip = 20)


files <- list.files("data/", full.names = T, pattern = ".txt")
a <- NA
for(i in seq_along(files)){
  a <- c(a, read_lines(file = files[[i]]))
}

a

str_extract_all(string = a, pattern = "\\s(\\w{4}\\s\\d{6})\\s")->b
str_extract_all(string = a, pattern = "\\s(\\w{6})\\s")->c
str_extract_all(string = a, pattern = "\\((.)+PLEA")->d
str_extract_all(string = a, pattern = "CLS:\\b[\\d]")->class
str_extract_all(string = a, pattern = "ATTY:\\w+")->plaint_attorney
str_extract_all(string = a, pattern = "\\d{6}\\s([A-z,]+)")->defendant_name
str_extract_all(string = a, pattern = "\\s\\s(\\w+,\\w+|\\w+,\\w+,\\w+)")->plaintiff_name
plaintiff_name[[84]]
a[45:55]
# Add a helper function for cleaning purposes
clean_nas <- function(x){
  if(identical(x, character(0))) NA_character_ else x
} 


# clean the regex ---------------------------------------------------------

case_num <-b%>%
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(case_num = value)

charge <-d%>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(charge = value) %>% 
  mutate(charge = str_remove(charge, "PLEA"),
         charge = str_trim(charge, "both"))

class <- class %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(class = value) %>% 
  mutate(class = str_remove(class, "CLS:"),
         class = str_trim(class, "both"))

plaint_attorney <- plaint_attorney %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(plaint_attorney = value) %>% 
  mutate(plaint_attorney = str_remove(plaint_attorney, "CLS:"),
         plaint_attorney = str_trim(plaint_attorney, "both"))

defendant_name <- defendant_name %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(defendant_name = value) %>% 
  mutate(defendant_name = str_remove(defendant_name, "\\d{6}\\s"))

plaintiff_name <- plaintiff_name %>% 
  map(clean_nas) %>% 
  map(str_trim) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(plaintiff_name = value) %>% 
  mutate(plaintiff_name = str_remove(plaintiff_name, "\\d{6}\\s"))

total_summary <- data_frame(
  case_num = case_num$case_num, 
  charge = charge$charge,
  class = class$class,
  plaint_attorney = plaint_attorney$plaint_attorney,
  defendant_name=defendant_name$defendant_name,
  plaintiff_name=plaintiff_name$plaintiff_name)

complete_data <- total_summary %>% 
  tidyr::fill(case_num, .direction = "down") %>% 
  tidyr::fill(defendant_name, .direction = "down") %>% 
  tidyr::fill(plaintiff_name, .direction = "down") %>% 
  tidyr::fill(plaint_attorney, .direction = "down") %>% 
  tidyr::fill(charge, .direction = "up") %>% 
  filter(!is.na(charge), !is.na(class))

complete_data %>% 
  count(case_num) %>% 
  summarise(mean = mean(n),
            median = median(n),
            max = max(n))
