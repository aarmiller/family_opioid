library(tidyverse)
library(bit64)
library(lubridate)
library(smallDB)

rm(list = ls())
gc()

#load data
enroll_collapse_overdose_fam <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_collapse_overdose_fam.RDS")
enrolees <- enroll_collapse_overdose_fam %>% distinct(enrolid) %>% .$enrolid
# connect to opioid database
db_con<- src_sqlite("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/opioids_claims.db")

overdose_fams_opioids <- collect_table(years = str_pad(c(1:20),2,pad="0")) %>% distinct(year) %>% 
  mutate(data = map(year, ~tbl(db_con, paste0("opioids_rx_core_", .)) %>%
                      filter(enrolid %in% enrolees) %>% 
                      collect(n = Inf) %>% 
                      select(-source))) %>% 
  unnest(data) %>% 
  select(year, month, everything()) %>% 
  group_by(year, month) %>% 
  nest() %>% 
  ungroup()

overdose_fams_opioids <- overdose_fams_opioids %>% unnest()

#save
save(overdose_fams_opioids, 
     file = "/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/overdose_fams_opioids.RData")



