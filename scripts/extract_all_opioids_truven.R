rm(list = ls(all = TRUE))
library(tidyverse)
library(lubridate)
library(bit64)
library(haven)
library(parallel)

########################################################################
#### Create function that given a specific year's database it pulls #### 
#### it pulls info from rx_core_ccae and rx_core_mdcr for a given   ####
#### set of ndc_codes                                               ####
########################################################################

get_opioids <- function(year, db_path, ndc_codes, n_collect = Inf){
  
  year <- year

  con <- paste0(db_path,"truven_", year, ".db") %>% src_sqlite()

  ccae <- tbl(con, paste0("rx_core_ccae_", year)) %>% 
    select(enrolid, ndcnum, svcdate, month, daysupp, qty, metqty, refill, pddate) %>% 
    filter(ndcnum %in% ndc_codes) %>% 
    collect(n = n_collect) %>% 
    mutate(source = "ccae")
  
   mdcr <- tbl(con, paste0("rx_core_mdcr_", year)) %>% 
    select(enrolid, ndcnum, svcdate, month, daysupp, qty, metqty, refill, pddate) %>% 
    filter(ndcnum %in% ndc_codes) %>% 
    collect(n = n_collect) %>% 
    mutate(source = "mdcr")
   
   out <- bind_rows(ccae, mdcr)
   
   return(out)
}

#### Run function over all years ####

## Pull in opioid NDC codes that Ryan provided 
opioid_info<- haven::read_sas("/Shared/Statepi_Marketscan/atlan/opioid_overdose/opioid_ndc_str_equiv_formcdc.sas7bdat")
opioid_codes_ryan <- opioid_info %>% select(ndcnum = PKG_PRODUCT_ID) %>% distinct() %>% .$ndcnum

## Pull in opioid NDC codes that Aaron provided 
load("/Shared/Statepi_Marketscan/atlan/opioid_overdose/opioid_codes/opioid_codes.RData")
opioid_codes_aaron <- opioid_codes %>% distinct(NDCNUM) %>% .$NDCNUM

## Combine all NDC codes
opioid_codes <- unique(c(opioid_codes_ryan, opioid_codes_aaron))

## Set Truven database path 
db_path <- "/Shared/Statepi_Marketscan/databases/Truven/"

## Specify years
years <- str_pad(c(1:20),2,pad="0")

## Specify number to collect
num_to_collect <- Inf

## Setup Cluster 
cl <- makeCluster(20)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(bit64))
clusterExport(cl, "num_to_collect")
clusterExport(cl, "opioid_codes")
clusterExport(cl, "db_path")
clusterExport(cl, "get_opioids")

temp <-  parLapply(cl,years,
                              function(x) {get_opioids(year = x,
                                                       db_path = db_path,
                                                       ndc_codes = opioid_codes,
                                                       n_collect = num_to_collect)})
gc()

# opioids_all_years <- tibble()
# for (i in 1:length(temp)){
#   temp1 <- temp[[i]] %>% mutate(year = i)
#   opioids_all_years <- bind_rows(opioids_all_years, temp1)
# }

## save 
# saveRDS(opioids_all_years, paste0("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/opioids_claims_",
#                                  month(lubridate::today()), "_", day(lubridate::today())))


#### Make opioid database ####

# Create db
write_db <- src_sqlite("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/opioids_claims.db",
                       create = TRUE)


# Write to DB 
map2(temp, years,
     ~copy_to(dest = write_db,
              df = .x,
              name = paste0("opioids_rx_core_", .y),
              temporary = FALSE,
              indexes = list("enrolid","ndcnum"),
              analyze = TRUE))

# Add redbook to database 
redbook <- read_csv("/Shared/Statepi_Marketscan/databases/Truven/redbook.csv")
redbook_opioids <- redbook %>% filter(NDCNUM %in% opioid_codes)
names(redbook_opioids) <- tolower(names(redbook_opioids))

copy_to(dest = write_db,
        df = redbook_opioids,
        name = paste0("redbook_opioids"),
        temporary = FALSE,
        indexes = list("ndcnum"))

