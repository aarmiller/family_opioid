library(bit64)
library(tidyverse)
library(lubridate)
library(smallDB)

rm(list=ls())
gc()

# load datasets
load("/Shared/AML/truven_data/temp/enroll_collapse_new.RData")
load("/Shared/AML/truven_data/temp/enroll_collapse_rx_new.RData")
load("/Shared/AML/truven_data/enrollment_info/emprel/emprel_child_depend.RData")
enroll_collapse_overdose_fam <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_collapse_overdose_fam.RDS")

# connect to opioid database
db_con_opioid_path <- "/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/opioids_claims.db"

# overdose families and cases to remove
exclude_enrolids <- enroll_collapse_overdose_fam %>% distinct(enrolid)

# remove overdose and create famid
collapse_enrolid <- collapse_enrolid %>%  
  anti_join(exclude_enrolids, by="enrolid") %>% 
  mutate(efamid=as.integer(enrolid %/% 100))

# add dependent info
collapse_enrolid <- collapse_enrolid %>% 
  left_join(emprel_child_depend %>% 
              select(enrolid) %>% 
              mutate(dependent=1L), by="enrolid") %>% 
  mutate(dependent=ifelse(is.na(dependent),0L,dependent))

# remove overdose from rx enrolids
collapse_enrolid_rx <- collapse_enrolid_rx %>%  
  anti_join(exclude_enrolids, by="enrolid")

# create month bins
month_bins <- tibble(year=2001:2020) %>% 
  mutate(month=map(year,~1:12)) %>% 
  unnest() %>% 
  mutate(first_date=ymd(paste0(year,"/",month,"/","01"))) %>% 
  mutate(first_date=as.integer(first_date),
         last_date=lead(first_date)-1) 

# time before
time_before_var <- 30L

# filter to enrollees continuously enrolled
filter_enroll <- function(start,end){
  collapse_enrolid %>% 
    filter(dtstart<=start &
             dtend>=end) %>% 
    distinct(enrolid,efamid,sex,dobyr,dependent) 
}
# filter_enroll(11354,11381)

# filter just the cases in the rx enroll
# filter_enroll_rx <- function(start,end){
#   collapse_enrolid_rx %>% 
#     filter(dtstart<=start &
#              dtend>=end) %>% 
#     distinct(enrolid,sex,dobyr) 
# }
#filter_enroll_rx(11354,11381)

# find patients that they themselves had opioid prescription in 30 days of start 
filter_opioid <- function(start, time_before_var){
  temp_month <- month(as_date(start))
  temp_year <- year(as_date(start))
  
  db_con_opioid<- src_sqlite(db_con_opioid_path)
  
  dataset <- tbl(db_con_opioid, paste0("opioids_rx_core_", str_sub(temp_year, 3))) %>% 
    filter(svcdate<=start &
             svcdate>=(start-time_before_var)) %>% 
    collect(n = Inf)
  
  if (nrow(dataset)>=1){
    
    out <- dataset %>%
      unnest() %>%
      distinct(enrolid) %>%
      mutate(prior_opioid = 1L) %>% 
      mutate(enrolid = as.integer64(enrolid))
    
  } else {
    out <- tibble(enrolid = integer64(), prior_opioid = integer())
  }
  return(out)
}
# filter_opioid(11381)

filter_opioid_fam <- function(start, time_before_var){
  temp_month <- month(as_date(start))
  temp_year <- year(as_date(start))
  
  db_con_opioid<- src_sqlite(db_con_opioid_path)
  
  dataset <- tbl(db_con_opioid, paste0("opioids_rx_core_", str_sub(temp_year, 3))) %>% 
    filter(svcdate<=start &
             svcdate>=(start-time_before_var)) %>% 
    collect(n = Inf) 
  
  if (nrow(dataset)>=1){
    
    out <- dataset %>%
      unnest() %>%
      mutate(efamid=as.integer(enrolid %/% 100)) %>% 
      select(efamid, hosp_enrolid=enrolid, svcdate) %>% 
      mutate(hosp_enrolid = as.integer64(hosp_enrolid)) %>% 
      distinct() 
    
  } else {
    out <- tibble(efamid = integer(), 
                  hosp_enrolid = integer64(), 
                  svcdate = integer(),
                  prior_opioid = integer())
  }
  return(out)
}

# filter_opioid_fam(11381)

# count enrollment info by period
count_enroll <- function(start,end,time_before_var){
  
  #find all enrolled in period
  temp_enrollees <- filter_enroll(start,end)
  
  #identify families
  multi_fams <- temp_enrollees %>% 
    count(efamid) %>% 
    rename(fam_size=n)
  
  #find enrollees that had opioid prescription on or within 30 days prior to start day
  temp_prior_opioid <- filter_opioid(start, time_before_var)
  
  #find enrollees that had family memebrs that had opioid prescription on or within 30 days prior to start day
  temp_prior_opioid_fam <- filter_opioid_fam(start, time_before_var)
  
  if (nrow(temp_prior_opioid_fam) > 0){
    temp_fam_opioid_ind <- temp_enrollees %>% 
      inner_join(temp_prior_opioid_fam, by = "efamid") %>% 
      filter(enrolid!=hosp_enrolid) %>% 
      distinct(enrolid) %>% 
      mutate(prior_opioid_fam_expose = 1L)
   
  } else {
    temp_fam_opioid_ind <- tibble(enrolid = integer64(),
                                 prior_opioid_fam_expose = integer())
  }
  
  
  # count the number of distinct fam_size,dobyr,sex,dependent,prior_opioid in period
  out <- temp_enrollees %>% 
    inner_join(multi_fams,by="efamid") %>%
    left_join(temp_prior_opioid,by="enrolid") %>%
    left_join(temp_fam_opioid_ind,by="enrolid") %>%
    mutate_at(vars(prior_opioid, prior_opioid_fam_expose),funs(ifelse(is.na(.),0L,.))) %>%
    count(fam_size,dobyr,sex,dependent,prior_opioid,prior_opioid_fam_expose) %>% 
    mutate(dobyr = as.integer(dobyr),
           sex = as.integer(sex))
  
  return(out)
}

# temp <- count_enroll(month_bins$first_date[[3]],month_bins$last_date[[3]], time_before_var = time_before_var)

enroll_counts <- month_bins

cluster <- parallel::makeCluster(10)
parallel::clusterCall(cluster, function() library(tidyverse))
parallel::clusterCall(cluster, function() library(bit64))
parallel::clusterCall(cluster, function() library(lubridate))
parallel::clusterExport(cluster, varlist = c("count_enroll","filter_enroll","filter_opioid", "filter_opioid_fam",
                                             "collapse_enrolid", "db_con_opioid_path", "time_before_var",
                                             "enroll_counts"))

out <- parallel::parLapply(cl = cluster, 1:nrow(enroll_counts), function(x) {
  count_enroll(enroll_counts$first_date[x],enroll_counts$last_date[x], time_before_var = time_before_var)
})

parallel::stopCluster(cluster)
gc()

tmp <- tibble()
for (i in 1:length(out)) {
  tmp1 <- tibble(enroll_count = out[i])
  tmp <- tmp %>% bind_rows(tmp1)
}

enroll_counts <- enroll_counts %>% bind_cols(tmp)

saveRDS(enroll_counts,"/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_counts_non_overdose_30_days_prior.RDS")

