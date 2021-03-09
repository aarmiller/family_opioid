library(tidyverse)
library(bit64)
library(lubridate)
library(smallDB)

rm(list = ls())
gc()

#load datasets
opioid_enrolid <- readRDS("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose_index_dates.RDS")
load("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose_dx_visits.RData")
enrolid_cross_walk <- readRDS("/Shared/Statepi_Diagnosis/collab_projects/opioid_overdose/data/truven/opioid_overdose_enrolid_crosswalk.RDS")
overdose_dx_codes <- read_csv("/Shared/Statepi_Marketscan/atlan/opioid_overdose/family_opioid/params/index_codes/index_dx_codes.csv")

overdose_events <- bind_rows(all_inpatient_visits %>%
                               select(enrolid, dx, admdate, disdate) %>% 
                               mutate(inpatient = 1L),
                    all_outpatient_visits %>% 
                      mutate(disdate = svcdate) %>% 
                      select(enrolid, dx, admdate = svcdate, disdate)%>% 
                      mutate(inpatient = 0L)) %>% 
  filter(dx %in% overdose_dx_codes$code) %>% 
  rename(new_enrolid = enrolid) %>% 
  inner_join(enrolid_cross_walk)%>% 
  mutate(admdate=as.integer(admdate),
         disdate=as.integer(disdate),
         efamid=as.integer(enrolid %/% 100)) %>% 
  select(enrolid,efamid, admdate, disdate, inpatient) %>% 
  distinct() %>% 
  arrange(enrolid, admdate) %>% 
  group_by(enrolid) %>% 
  mutate(days_since = admdate - lag(disdate, n = 1L, default = NA),
         days_since = ifelse(is.na(days_since), 0L, days_since)) %>% 
  ungroup()

#### Setup month bins for monthly counts ####
month_bins <- tibble(year=2001:2020) %>% 
  mutate(month=map(year,~1:12)) %>% 
  unnest() %>% 
  mutate(first_date=ymd(paste0(year,"/",month,"/","01"))) %>% 
  mutate(first_date=as.integer(first_date),
         last_date=lead(first_date)-1)

#### Filter to enrollees continuosly enrolled for a given month ####
# load datasets
# load("/Shared/AML/truven_data/temp/enroll_collapse_rx_new.RData")
enroll_collapse_overdose_fam <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_collapse_overdose_fam.RDS")
collapse_enrolid <- enroll_collapse_overdose_fam 
load("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/overdose_fams_opioids.RData")

# # filter to those that had overdose dx
# collapse_enrolid_rx <- collapse_enrolid_rx %>%  
#   inner_join(enroll_collapse_overdose_fam %>% 
#                 distinct(enrolid), by="enrolid")

# time before
time_before_var <- 30L

# filter to enrollees continuously enrolled
filter_enroll <- function(start,end){
  collapse_enrolid %>% 
    filter(dtstart<=start &
             dtend>=end) %>% 
    distinct(enrolid,efamid,sex,dobyr) 
}

# find patients that they themselves had opioid prescription in 30 days of start 
filter_opioid <- function(start, time_before_var){
  temp_month <- month(as_date(start))
  temp_year <- year(as_date(start))
  
  dataset <- overdose_fams_opioids %>% 
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
# filter_opioid(11381, time_before_var)

filter_opioid_fam <- function(start, time_before_var){
  temp_month <- month(as_date(start))
  temp_year <- year(as_date(start))
  
  dataset <- overdose_fams_opioids %>% 
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

# filter_opioid_fam(11381, time_before_var)

# find overdose enroll
filter_enroll_overdose <- function(start,end){
  
  #find all enrolled in period
  temp_enrollees <- filter_enroll(start,end)
  
  #identify families
  multi_fams <- temp_enrollees %>% 
    count(efamid) %>% 
    rename(fam_size=n)
  
  #find enrollees that had opioid use on or within 30 days prior to start day
  temp_prior_opioid <- filter_opioid(start, time_before_var)
  
  #find enrollees that had family memebrs that had opioid prescription on or within 30 days prior to start day
  temp_prior_opioid_fam <- filter_opioid_fam(start, time_before_var)
  
  if (nrow(temp_enrollees %>% 
           inner_join(temp_prior_opioid_fam, by = "efamid")) > 0){
    temp_fam_opioid_ind <- temp_enrollees %>% 
      inner_join(temp_prior_opioid_fam, by = "efamid") %>% 
      filter(enrolid!=hosp_enrolid) %>% 
      distinct(enrolid) %>% 
      mutate(prior_opioid_fam_expose = 1L)
    
  } else {
    temp_fam_opioid_ind <- tibble(enrolid = integer64(),
                                  prior_opioid_fam_expose = integer())
  }
  
  out <- temp_enrollees %>% 
    inner_join(multi_fams,by="efamid") %>%
    left_join(temp_prior_opioid,by="enrolid") %>%
    left_join(temp_fam_opioid_ind,by="enrolid") %>%
    mutate_at(vars(prior_opioid, prior_opioid_fam_expose),funs(ifelse(is.na(.),0L,.))) %>%
    mutate(dobyr = as.integer(dobyr),
           sex = as.integer(sex))

  return(out)
}

month_bins <- month_bins %>% 
  mutate(enrollees=map2(first_date,last_date,~filter_enroll_overdose(.x,.y)))

# filter to overdose events that occur in a given window
washout_period <- 365L #recurrent overdose >365 days apart

filter_overdose <- function(start,end,washout_period){
  
  # find opioid events in the given window
  temp_opioid_cases1 <- overdose_events %>% 
    filter(start<=admdate &
             admdate<=end) %>%   # CDI occured in that month
    filter(days_since>washout_period | days_since == 0)
  
  if (nrow(temp_opioid_cases1) >0){
    temp_opioid_cases <- temp_opioid_cases1 %>%   # More than time_before_var/washout since last CDI episode
      group_by(enrolid) %>% 
      filter(admdate == min(admdate)) %>% 
      ungroup() %>% 
      distinct(enrolid,efamid,admdate,inpatient) %>% 
      mutate(overdose_date=admdate)
    } else{
      temp_opioid_cases <-  tibble(enrolid = integer64(),
                                   efamid = integer(),
                                   admdate = integer(),
                                   inpatient = integer(),
                                   overdose_date = integer())
    }
  
  #find enrollees that had opioid use on or within 30 days prior to start day
  temp_prior_opioid <- filter_opioid(start, time_before_var)
  
  #find enrollees that had family memebrs that had opioid prescription on or within 30 days prior to start day
  temp_prior_opioid_fam <- filter_opioid_fam(start, time_before_var)
  
  if (nrow(temp_opioid_cases %>% 
           inner_join(temp_prior_opioid_fam, by = "efamid")) > 0){
    temp_fam_opioid_ind <- temp_opioid_cases %>% 
      inner_join(temp_prior_opioid_fam, by = "efamid") %>% 
      filter(enrolid!=hosp_enrolid) %>% 
      distinct(enrolid) %>% 
      mutate(prior_opioid_fam_expose = 1L)
    
  } else {
    temp_fam_opioid_ind <- tibble(enrolid = integer64(),
                                  prior_opioid_fam_expose = integer())
  }
  
  out_cdi_cases <- temp_opioid_cases %>% 
    left_join(temp_prior_opioid,by="enrolid") %>% 
    left_join(temp_fam_opioid_ind,by="enrolid") %>% 
    mutate_at(vars(prior_opioid, prior_opioid_fam_expose),funs(ifelse(is.na(.),0L,.))) %>% 
    select(-admdate)
  
  return(out_cdi_cases)
}

overdose_month_bins <- month_bins %>% 
  mutate(overdose_cases=map2(first_date,last_date,~filter_overdose(.x,.y,washout_period = washout_period)))

#### find possible prior exposure ####

#### Filter to two groups ####

#### REMOVE overdose CASES
overdose_month_bins <- overdose_month_bins %>% 
  mutate(non_overdose=map2(enrollees,overdose_cases,~anti_join(.x,.y,by="enrolid")))

###### REMOVE THE overdose CASES time_before_var DAYS AFTER or DURING 

find_overdose_exclude <- function(start,end){
  overdose_events %>% 
    filter(disdate>=start-time_before_var &
             admdate<=end) %>% 
    distinct(enrolid)
}

overdose_month_bins <- overdose_month_bins %>% 
  mutate(overdose_exclude=map2(first_date,last_date,~find_overdose_exclude(.x,.y)))

overdose_month_bins <- overdose_month_bins %>% 
  mutate(non_overdose=map2(non_overdose,overdose_exclude,~anti_join(.x,.y,by="enrolid")))

##### Reduce to final cases ####

overdose_month_bins <- overdose_month_bins %>% 
  mutate(overdose_fin=map2(enrollees,overdose_cases,
                      ~inner_join(select(.x,enrolid,fam_size),
                                  .y,by="enrolid")))

fin_overdose_bins <- overdose_month_bins %>% 
  select(year:last_date,non_overdose,overdose_fin)

tmp1 <- fin_overdose_bins %>% 
  select(year:last_date,non_overdose) %>% 
  unnest() %>% 
  mutate(overdose_case=0L) %>% 
  select(-dobyr, -sex)

tmp2 <- fin_overdose_bins %>% 
  select(year:last_date,overdose_fin) %>% 
  unnest() %>% 
  select(year:last_date,enrolid,efamid,fam_size, prior_opioid, prior_opioid_fam_expose) %>% 
  mutate(overdose_case=1L)

names(tmp1)[!(names(tmp1) %in% names(tmp2))]

fin_overdose_data <- bind_rows(tmp1,
                          tmp2)

saveRDS(fin_overdose_data,
        "/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/temp_fin_overdose_data_30_days_prior.RDS")

rm(list=ls())
gc()

#### Add other info  ####
load("/Shared/AML/truven_data/enrollment_info/enroll_counts_all_age_sex_hosp.RData")
enroll_collapse_overdose_fam <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/enroll_collapse_overdose_fam.RDS")
load("/Shared/AML/truven_data/enrollment_info/emprel/emprel_child_depend.RData")

fin_overdose_data <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/temp_fin_overdose_data_30_days_prior.RDS")

all_demo <- enroll_collapse_overdose_fam %>% 
  distinct(enrolid,dobyr,sex) %>% 
  group_by(enrolid) %>% 
  summarise(dobyr=min(dobyr),
            sex=min(sex))

temp <- fin_overdose_data %>% 
  inner_join(all_demo) 

temp <- temp %>% 
  mutate(age=year-dobyr) %>% 
  left_join(select(emprel_child_depend,enrolid) %>% 
              mutate(dependent=1L), by="enrolid") %>% 
  mutate(dependent=ifelse(is.na(dependent),0L,dependent))

fin_overdose_data <- temp

saveRDS(fin_overdose_data, "/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/temp_fin_overdose_data_30_days_prior.RDS")
# fin_overdose_data <- readRDS("/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/temp_fin_overdose_data_30_days_prior.RDS") 

fin_overdose_counts <- fin_overdose_data %>% 
  group_by(year,month,fam_size,sex,age,dependent,prior_opioid, prior_opioid_fam_expose) %>% 
  summarise(n_overdose=sum(overdose_case),
            n_enroll=n()) %>% 
  ungroup()

save(fin_overdose_counts,
     file="/Shared/Statepi_Marketscan/atlan/opioid_overdose/data/overdose_counts_30_days_prior.RData")

