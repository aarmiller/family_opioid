rm(list=ls())
library(tidyverse)
library(icd)


# load functions and define path
source("/Volumes/Statepi_Diagnosis/params/index_condition_codes/make_codes/export_index_codes_function.R")


# define codes
codes <- list(dx_name = "opioid_overdose",
              dx_name_long = "Opioid Overdose",
              icd9_codes = c(children("9650"), "E8500", "E8501", "E8502"),
              icd10_codes = children(c("T400", "T401", "T402", "T403", "T404")))

tmp <- rbind(tibble(dx_name_long = codes$dx_name_long,
                    icd_group=codes$dx_name,
                    code=codes$icd9_codes,
                    icd_version=9L),
             tibble(dx_name_long = codes$dx_name_long,
                    icd_group=codes$dx_name,
                    code=codes$icd10_codes,
                    icd_version=10L))

write_csv(tmp,path = "/Volumes/Statepi_Marketscan/atlan/opioid_overdose/family_opioid/params/index_codes/index_dx_codes.csv")
