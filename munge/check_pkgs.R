sessionInfo()-> info

packages_loaded <- info$loadedOnly

packages_loaded %>% 
  map_df(., ~sum(grepl(., pattern = "CRAN"))) %>% 
  gather(package, status) %>% 
  filter(status !=1)
