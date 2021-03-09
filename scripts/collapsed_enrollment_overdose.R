library(bit64)
library(tidyverse)
library(lubridate)
library(smallDB)

rm(list=ls())
gc()

# load index info for all enrollees that had an overdose code
enrollees <- readRDS("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose_index_dates.RDS")

# connect to database
db_con<- src_sqlite("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose.db")

# get collapsed enrollment
collapsed_enroll <- smallDB::collapse_enrollment(enrolid_list = unique(enrollees$enrolid),
                                                 db_con = db_con,
                                                 vars = c("egeoloc", "msa"),
                                                 collect_tab = collect_table(years = str_pad(c(1:20),2,pad="0")))

# get dobyr and sex
core_data <- collect_table(years = str_pad(c(1:20),2,pad="0")) %>% distinct(year, source) %>% 
  mutate(data = map2(year, source, ~tbl(db_con, paste0("enrollees_", .y, "_", .x)) %>%
                       select(enrolid, dobyr, sex) %>% collect(n = Inf))) %>% 
  unnest(data) %>% 
  distinct(enrolid, dobyr, sex)

# add dobyr and sex to collapsed enrollment data
collapsed_enroll <- collapsed_enroll %>% inner_join(core_data)

# load crosswalk
enrollees_crosswalk <- readRDS("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose_enrolid_crosswalk.RDS")

# add crosswalk enrolid to collapsed enrollment
collapsed_enroll_overdose <- collapsed_enroll %>% 
  rename(new_enrolid = enrolid) %>% 
  inner_join(enrollees_crosswalk) %>% 
  select(enrolid,  everything()) %>% 
  mutate(efamid=as.integer(enrolid %/% 100))

# save data
saveRDS(collapsed_enroll_overdose, "/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_collapse_overdose_fam.RDS")


