rm(list = ls(all = TRUE))
library(tidyverse)
library(haven)

## Pull in opioid NDC codes that Aaron provided 
load("/Volumes/Statepi_Marketscan/atlan/opioid_overdose/opioid_codes/opioid_codes.RData")

opioid_codes %>% distinct(NDCNUM)

## Pull in opioid NDC codes that Ryan provided 
opioid_info<- haven::read_sas("/Volumes/Statepi_Marketscan/atlan/opioid_overdose/opioid_ndc_str_equiv_formcdc.sas7bdat")
opioid_codes_ryan <- opioid_info %>% select(ndcnum = PKG_PRODUCT_ID) %>% distinct() %>% .$ndcnum

common <- opioid_info %>% select(NDCNUM = PKG_PRODUCT_ID)%>% inner_join(., opioid_codes %>% distinct(NDCNUM))
only_in_ryan <- opioid_info %>% select(NDCNUM = PKG_PRODUCT_ID)%>% anti_join(., opioid_codes %>% distinct(NDCNUM))
only_in_aaron <- opioid_info %>% select(NDCNUM = PKG_PRODUCT_ID)%>% anti_join(opioid_codes %>% distinct(NDCNUM), .)


# load red book
temp <- read_csv("/Volumes/Statepi_Marketscan/databases/Truven/redbook.csv")
extra_in_aaron <- temp %>% filter(NDCNUM %in% only_in_aaron$NDCNUM) %>% select(NDCNUM, GENPRODUCT_ID = GENERID, 
                                                             GENERIC_NAME = GENNME,
                                                             PROCUCT_NAME = PRODNME)
write_csv(extra_in_aaron, "/Volumes/Statepi_Marketscan/atlan/opioid_overdose/opioid_codes/extra_codes_not_in_ryan.csv")

extra_in_ryan<- temp %>% filter(NDCNUM %in% only_in_ryan$NDCNUM) %>% select(NDCNUM, GENPRODUCT_ID = GENERID, 
                                                                               GENERIC_NAME = GENNME,
                                                                               PROCUCT_NAME = PRODNME)
write_csv(extra_in_aaron, "/Volumes/Statepi_Marketscan/atlan/opioid_overdose/opioid_codes/extra_codes_not_in_aaron.csv")
