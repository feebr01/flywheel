##flywheel scripts
##code written for PC - file paths will need updated if run on Mac

##  libraries ------------------------------------------------------------------
## load needed libraries
library(ggplot2)

library(scales)

library(dplyr)

library(lazyeval)

library(lubridate)

library(stringr)

library(tidyr)

library(purrr)

library(readr)


##  global date definitions ---------------------------------------------------------------
##set working directory ~ defaults to users
setwd("~/flywheel/zz_source_files")

## one day after the end of current month - need to filter out any activity that starts later than this
cur_close_month <- as.Date("2018-11-01")
##for certain charts will determine what date we should start at
fw_start_date <- as.Date("2014-03-01")

##start of fiscal year - manually update for new fiscal year
fy0 <- as.Date("2018-03-01")

cur_dt <- as.Date("2018-11-07")

cur_fw <- cur_close_month - months(1)

#  various flywheel functions ---------------------------------------------------------------

fmt_dt <- function(x) as.Date(x, origin = "1970-01-01") 

my_dt <- function(x) floor_date(as.Date(x, origin = "1970-01-01"), "month")

cur_mon <- function() floor_date(Sys.Date(), "month")

rnd0 <- function(x) round(x, 0)

rnd1 <- function(x) round(x, 1)

rnd2 <- function(x) round(x, 2) 

rnd3 <- function(x) round(x, 3) 

rnd4 <- function(x) round(x, 4) 

znull <- function(x) ifelse(is.na(x), 0, x)

my_fyq <- function(x, y) quarter(floor_date(as.Date(x), "month") + months(y), with_year = TRUE)

my_lvls <- function(x, y) factor(x, levels = y)

land <- function(x, y) ifelse(x == 0, y, 0)

retain <- function(x, y) ifelse(y < x, y, x)

added <- function(x, y) ifelse(y > x, y - x, 0)

xpand <- function(x, y) ifelse(x > 0 & y > x, y - x, 0)

netren <- function(x, y) ifelse(x == 0, 0, y)

lost <- function(x, y) ifelse(y < x, x - y, 0)

term <- function(x, y) ifelse(y == 0, x, 0)

term_z <- function(x, y, z) ifelse(y <= z, x - y, 0)

dg <- function(x, y) ifelse(y < x & y > 0, x - y, 0)

## this is the standard Rally seat size segment scheme - <100, 100-999, >= 1000
size_seg <- function(x){
  d <- factor(ifelse(x < 100, "<100", 
                     ifelse(x < 1000, "<1000", ">=1000")),
              # levels = rev(c("<100", "<1000", ">=1000"))),
              levels = c(">=1000", "<1000", "<100"))
  d
}

## this is a modifed Rally seat size scheme to give better detail for customers < 1000 seats
csm_size_seg <- function(x){
  d <- factor(ifelse(x < 50, "<50", 
                     ifelse(x < 100, "50-99",
                            ifelse(x < 500, "100-499",
                                   ifelse(x < 1000, "500-999", ">=1000")))),
              # levels = rev(c("<100", "<1000", ">=1000"))),
              levels = c(">=1000", "500-999", "100-499", "50-99", "<50"))
  d
}

## following is key function that allows creation of monthly flywheel detail
## expansion of each order across schedule based on start, end dates
## using excel date bug correction for day origin
fw_expand_ODB <- function(oli_seqid, start_month, end_month){
  
  start_month <- as.Date(start_month, origin = "1970-01-01")
  
  end_month <- as.Date(end_month, origin = "1970-01-01") - months(1) ##Rally data requires this logic to keep historical reporting
  
  expand.grid(oli_seqid = oli_seqid,
              fw_month = seq.Date(from = start_month,
                                  to = end_month,
                                  by = "month"))
}

fw_expand_EIW <- function(oli_seqid, start_month, end_month){
  
  start_month <- as.Date(start_month, origin = "1970-01-01")
  
  end_month <- as.Date(end_month, origin = "1970-01-01") ##need different date logic for SAP, keep actual end date
  
  expand.grid(oli_seqid = oli_seqid,
              fw_month = seq.Date(from = start_month,
                                  to = end_month,
                                  by = "month"))
}


## key function that creates all flywheel summary info
## flywheel outputs don't generate correctly if csm_size_cat + account_segment + geo all are passed to function (haven't resolved why)
## to keep things clear there is a separate function to generate standard flywheel outputs + "csm" outputs - "NEW" function
## to generate the account_segment and geo views separate churn functions are used "SEG" and "GEO", respectively
fx_calc_churn_NEW <- function(df, x, y, ...) {
  
  ##full_join(filter(d_fw, fw_month == x), filter(d_fw, fw_month == y), by = "odbid") %>% 
  full_join(filter(df, fw_month == x), filter(df, fw_month == y), by = "odbid") %>% 
    ##WK edit to pass df variable into function rather than hardcode d_fw    
    # testing just entering arbitrary dates
    # d_test <- full_join(filter(d_fw, fw_month == "2014-07-01"), filter(d_fw, fw_month == "2015-07-01"), by = "odbid")
    mutate(fw_month = min(fw_month.x, fw_month.y, na.rm = TRUE),
           seats.x = znull(seats_odb.x),
           seats.y = znull(seats_odb.y),
           size_cat = size_seg(seats_odb.x),
           csm_size_cat = csm_size_seg(seats_odb.x), ##added CSM size bands
           ##account_segment = znull(account_segment.x), ## added account_segment
           ##geo = znull(geo.x),
           arr.y = znull(arr_odb.y),
           arr.x = znull(arr_odb.x),
           seats_lost = lost(seats.x, seats.y),
           arr_lost = lost(arr.x, arr.y),
           seats_retain = retain(seats.x, seats.y),
           arr_retain = retain(arr.x, arr.y),
           seats_dg = dg(seats.x, seats.y),
           arr_dg = dg(arr.x, arr.y),
           seats_term = term(seats.x, seats.y),
           arr_term = term(arr.x, arr.y),
           seats_add = added(seats.x, seats.y),
           arr_add = added(arr.x, arr.y),
           seats_xpand = xpand(seats.x, seats.y),
           arr_xpand = xpand(arr.x, arr.y),
           seats_land = land(seats.x, seats.y),
           arr_land = land(arr.x, arr.y),
           seats_netren = netren(seats.x, seats.y),
           arr_netren = netren(arr.x, arr.y),
           cust_cohort = arr.x > 0,
           cust_land = arr.y > 0 & arr.x == 0,
           cust_xpand = arr.y > arr.x & arr.x > 0,
           cust_decl = arr.y < arr.x,
           cust_term = arr.x > 0 & arr.y == 0,
           cust_lost10 = seats.x > 10 & seats.y <= 10) %>%
    data.frame() %>%
    group_by_(...) %>%
    summarise(start_dt = min(fw_month.x, na.rm = TRUE),
              end_dt = max(fw_month.y, na.rm = TRUE),
              ##n = length(odbid),
              n = n_distinct(odbid),
              cohort_n = sum(cust_cohort),
              cust_land = sum(cust_land),
              cust_xpand = sum(cust_xpand),
              cust_decl = sum(cust_decl),
              cust_term = sum(cust_term),
              cust_lost10 = sum(cust_lost10),
              p_cust_xpand = rnd3(sum(cust_xpand) / sum(cust_cohort)),
              avg_arr_xpand = rnd1((sum(arr_xpand) / sum(cust_xpand)) / 10^3),
              p_cust_decl = rnd3(sum(cust_decl) / sum(cust_cohort)),
              avg_arr_decl = rnd1((sum(arr_lost) / sum(cust_decl)) / 10^3),
              agg_seats_begin = sum(seats.x),
              agg_seats_end = sum(seats.y),
              agg_seats_lost = sum(seats_lost),
              agg_seats_dg = sum(seats_dg),
              agg_seats_term = sum(seats_term),
              agg_seats_add = sum(seats_add),
              agg_seats_xpand = sum(seats_xpand),
              agg_seats_land = sum(seats_land),
              agg_seats_netren = sum(seats_netren),
              agg_arr_begin = rnd3(sum(arr.x) / 10^6),
              agg_arr_end = rnd3(sum(arr.y) / 10^6),
              agg_arr_lost = rnd3(sum(arr_lost) / 10^6),
              agg_arr_dg = rnd3(sum(arr_dg) / 10^6),
              agg_arr_term = rnd3(sum(arr_term) / 10^6),
              agg_arr_add = rnd3(sum(arr_add) / 10^6),
              agg_arr_xpand = rnd3(sum(arr_xpand) / 10^6),
              agg_arr_land = rnd3(sum(arr_land) / 10^6),
              agg_arr_netren = rnd3(sum(arr_netren) / 10^6),
              churn_seats = rnd3(sum(seats_lost) / sum(seats.x)),
              p_seats_retain = rnd3(sum(seats_retain) / sum(seats.x)),
              p_seats_xpand = rnd3(sum(seats_xpand) / sum(seats.x)),
              churn_seats_term = rnd3(sum(seats_term) / sum(seats.x)),
              churn_seats_dg = rnd3(sum(seats_dg) / sum(seats.x)),
              churn_arr = rnd3 (sum(arr_lost) / sum(arr.x)),
              p_arr_retain = rnd3(sum(arr_retain) / sum(arr.x)),
              p_arr_xpand = rnd3(sum(arr_xpand) / sum(arr.x)),
              churn_arr_term = rnd3(sum(arr_term) / sum(arr.x)),
              churn_arr_dg = rnd3(sum(arr_dg) / sum(arr.x)),
              churn_cust_10seat = rnd3(sum(cust_lost10) / sum(seats.x >= 10)),
              net_renew_seats = rnd3(sum(seats_netren) / sum(seats.x)),
              net_renew_arr = rnd3(sum(arr_netren) / sum(arr.x))
    )
}

##custom function for doing calcs based on segmentation data - not customer ID data
##to get all YoY outputs to generate correctly using separate function to generate account_segment outputs
fx_calc_churn_SEG <- function(df, x, y, ...) {
  
  full_join(filter(df, fw_month == x), filter(df, fw_month == y), by = "odbid") %>% 
    mutate(fw_month = min(fw_month.x, fw_month.y, na.rm = TRUE),
           seats.x = znull(seats_odb.x),
           seats.y = znull(seats_odb.y),
           size_cat = size_seg(seats_odb.x),
           csm_size_cat = csm_size_seg(seats_odb.x), ##added CSM size bands
           account_segment = znull(account_segment.x), ## added account_segment
           arr.y = znull(arr_odb.y),
           arr.x = znull(arr_odb.x),
           seats_lost = lost(seats.x, seats.y),
           arr_lost = lost(arr.x, arr.y),
           seats_retain = retain(seats.x, seats.y),
           arr_retain = retain(arr.x, arr.y),
           seats_dg = dg(seats.x, seats.y),
           arr_dg = dg(arr.x, arr.y),
           seats_term = term(seats.x, seats.y),
           arr_term = term(arr.x, arr.y),
           seats_add = added(seats.x, seats.y),
           arr_add = added(arr.x, arr.y),
           seats_xpand = xpand(seats.x, seats.y),
           arr_xpand = xpand(arr.x, arr.y),
           seats_land = land(seats.x, seats.y),
           arr_land = land(arr.x, arr.y),
           seats_netren = netren(seats.x, seats.y),
           arr_netren = netren(arr.x, arr.y),
           cust_cohort = arr.x > 0,
           cust_land = arr.y > 0 & arr.x == 0,
           cust_xpand = arr.y > arr.x & arr.x > 0,
           cust_decl = arr.y < arr.x,
           cust_term = arr.x > 0 & arr.y == 0,
           cust_lost10 = seats.x > 10 & seats.y <= 10) %>%
    data.frame() %>%
    group_by_(...) %>%
    summarise(start_dt = min(fw_month.x, na.rm = TRUE),
              end_dt = max(fw_month.y, na.rm = TRUE),
              ##n = length(odbid),
              n = n_distinct(odbid),
              cohort_n = sum(cust_cohort),
              cust_land = sum(cust_land),
              cust_xpand = sum(cust_xpand),
              cust_decl = sum(cust_decl),
              cust_term = sum(cust_term),
              cust_lost10 = sum(cust_lost10),
              p_cust_xpand = rnd3(sum(cust_xpand) / sum(cust_cohort)),
              avg_arr_xpand = rnd1((sum(arr_xpand) / sum(cust_xpand)) / 10^3),
              p_cust_decl = rnd3(sum(cust_decl) / sum(cust_cohort)),
              avg_arr_decl = rnd1((sum(arr_lost) / sum(cust_decl)) / 10^3),
              agg_seats_begin = sum(seats.x),
              agg_seats_end = sum(seats.y),
              agg_seats_lost = sum(seats_lost),
              agg_seats_dg = sum(seats_dg),
              agg_seats_term = sum(seats_term),
              agg_seats_add = sum(seats_add),
              agg_seats_xpand = sum(seats_xpand),
              agg_seats_land = sum(seats_land),
              agg_seats_netren = sum(seats_netren),
              agg_arr_begin = rnd3(sum(arr.x) / 10^6),
              agg_arr_end = rnd3(sum(arr.y) / 10^6),
              agg_arr_lost = rnd3(sum(arr_lost) / 10^6),
              agg_arr_dg = rnd3(sum(arr_dg) / 10^6),
              agg_arr_term = rnd3(sum(arr_term) / 10^6),
              agg_arr_add = rnd3(sum(arr_add) / 10^6),
              agg_arr_xpand = rnd3(sum(arr_xpand) / 10^6),
              agg_arr_land = rnd3(sum(arr_land) / 10^6),
              agg_arr_netren = rnd3(sum(arr_netren) / 10^6),
              churn_seats = rnd3(sum(seats_lost) / sum(seats.x)),
              p_seats_retain = rnd3(sum(seats_retain) / sum(seats.x)),
              p_seats_xpand = rnd3(sum(seats_xpand) / sum(seats.x)),
              churn_seats_term = rnd3(sum(seats_term) / sum(seats.x)),
              churn_seats_dg = rnd3(sum(seats_dg) / sum(seats.x)),
              churn_arr = rnd3 (sum(arr_lost) / sum(arr.x)),
              p_arr_retain = rnd3(sum(arr_retain) / sum(arr.x)),
              p_arr_xpand = rnd3(sum(arr_xpand) / sum(arr.x)),
              churn_arr_term = rnd3(sum(arr_term) / sum(arr.x)),
              churn_arr_dg = rnd3(sum(arr_dg) / sum(arr.x)),
              churn_cust_10seat = rnd3(sum(cust_lost10) / sum(seats.x >= 10)),
              net_renew_seats = rnd3(sum(seats_netren) / sum(seats.x)),
              net_renew_arr = rnd3(sum(arr_netren) / sum(arr.x))
    )
}

##custom function for doing calcs based on GEO data - not customer ID data
##to get all YoY outputs to generate correctly using separate function to generate geo outputs
fx_calc_churn_GEO <- function(df, x, y, ...) {
  
  full_join(filter(df, fw_month == x), filter(df, fw_month == y), by = "odbid") %>% 
    mutate(fw_month = min(fw_month.x, fw_month.y, na.rm = TRUE),
           seats.x = znull(seats_odb.x),
           seats.y = znull(seats_odb.y),
           size_cat = size_seg(seats_odb.x),
           csm_size_cat = csm_size_seg(seats_odb.x), ##added CSM size bands
           geo = znull(geo.x), ## added geo
           arr.y = znull(arr_odb.y),
           arr.x = znull(arr_odb.x),
           seats_lost = lost(seats.x, seats.y),
           arr_lost = lost(arr.x, arr.y),
           seats_retain = retain(seats.x, seats.y),
           arr_retain = retain(arr.x, arr.y),
           seats_dg = dg(seats.x, seats.y),
           arr_dg = dg(arr.x, arr.y),
           seats_term = term(seats.x, seats.y),
           arr_term = term(arr.x, arr.y),
           seats_add = added(seats.x, seats.y),
           arr_add = added(arr.x, arr.y),
           seats_xpand = xpand(seats.x, seats.y),
           arr_xpand = xpand(arr.x, arr.y),
           seats_land = land(seats.x, seats.y),
           arr_land = land(arr.x, arr.y),
           seats_netren = netren(seats.x, seats.y),
           arr_netren = netren(arr.x, arr.y),
           cust_cohort = arr.x > 0,
           cust_land = arr.y > 0 & arr.x == 0,
           cust_xpand = arr.y > arr.x & arr.x > 0,
           cust_decl = arr.y < arr.x,
           cust_term = arr.x > 0 & arr.y == 0,
           cust_lost10 = seats.x > 10 & seats.y <= 10) %>%
    data.frame() %>%
    group_by_(...) %>%
    summarise(start_dt = min(fw_month.x, na.rm = TRUE),
              end_dt = max(fw_month.y, na.rm = TRUE),
              ##n = length(odbid),
              n = n_distinct(odbid),
              cohort_n = sum(cust_cohort),
              cust_land = sum(cust_land),
              cust_xpand = sum(cust_xpand),
              cust_decl = sum(cust_decl),
              cust_term = sum(cust_term),
              cust_lost10 = sum(cust_lost10),
              p_cust_xpand = rnd3(sum(cust_xpand) / sum(cust_cohort)),
              avg_arr_xpand = rnd1((sum(arr_xpand) / sum(cust_xpand)) / 10^3),
              p_cust_decl = rnd3(sum(cust_decl) / sum(cust_cohort)),
              avg_arr_decl = rnd1((sum(arr_lost) / sum(cust_decl)) / 10^3),
              agg_seats_begin = sum(seats.x),
              agg_seats_end = sum(seats.y),
              agg_seats_lost = sum(seats_lost),
              agg_seats_dg = sum(seats_dg),
              agg_seats_term = sum(seats_term),
              agg_seats_add = sum(seats_add),
              agg_seats_xpand = sum(seats_xpand),
              agg_seats_land = sum(seats_land),
              agg_seats_netren = sum(seats_netren),
              agg_arr_begin = rnd3(sum(arr.x) / 10^6),
              agg_arr_end = rnd3(sum(arr.y) / 10^6),
              agg_arr_lost = rnd3(sum(arr_lost) / 10^6),
              agg_arr_dg = rnd3(sum(arr_dg) / 10^6),
              agg_arr_term = rnd3(sum(arr_term) / 10^6),
              agg_arr_add = rnd3(sum(arr_add) / 10^6),
              agg_arr_xpand = rnd3(sum(arr_xpand) / 10^6),
              agg_arr_land = rnd3(sum(arr_land) / 10^6),
              agg_arr_netren = rnd3(sum(arr_netren) / 10^6),
              churn_seats = rnd3(sum(seats_lost) / sum(seats.x)),
              p_seats_retain = rnd3(sum(seats_retain) / sum(seats.x)),
              p_seats_xpand = rnd3(sum(seats_xpand) / sum(seats.x)),
              churn_seats_term = rnd3(sum(seats_term) / sum(seats.x)),
              churn_seats_dg = rnd3(sum(seats_dg) / sum(seats.x)),
              churn_arr = rnd3 (sum(arr_lost) / sum(arr.x)),
              p_arr_retain = rnd3(sum(arr_retain) / sum(arr.x)),
              p_arr_xpand = rnd3(sum(arr_xpand) / sum(arr.x)),
              churn_arr_term = rnd3(sum(arr_term) / sum(arr.x)),
              churn_arr_dg = rnd3(sum(arr_dg) / sum(arr.x)),
              churn_cust_10seat = rnd3(sum(cust_lost10) / sum(seats.x >= 10)),
              net_renew_seats = rnd3(sum(seats_netren) / sum(seats.x)),
              net_renew_arr = rnd3(sum(arr_netren) / sum(arr.x))
    )
}


fx_calc_churn_FOCUS <- function(df, x, y, ...) {
  
  full_join(filter(df, fw_month == x), filter(df, fw_month == y), by = "odbid") %>% 
    mutate(fw_month = min(fw_month.x, fw_month.y, na.rm = TRUE),
           seats.x = znull(seats_odb.x),
           seats.y = znull(seats_odb.y),
           size_cat = size_seg(seats_odb.x),
           csm_size_cat = csm_size_seg(seats_odb.x), ##added CSM size bands
           focus = znull(focus_list.x), ## added focus list
           arr.y = znull(arr_odb.y),
           arr.x = znull(arr_odb.x),
           seats_lost = lost(seats.x, seats.y),
           arr_lost = lost(arr.x, arr.y),
           seats_retain = retain(seats.x, seats.y),
           arr_retain = retain(arr.x, arr.y),
           seats_dg = dg(seats.x, seats.y),
           arr_dg = dg(arr.x, arr.y),
           seats_term = term(seats.x, seats.y),
           arr_term = term(arr.x, arr.y),
           seats_add = added(seats.x, seats.y),
           arr_add = added(arr.x, arr.y),
           seats_xpand = xpand(seats.x, seats.y),
           arr_xpand = xpand(arr.x, arr.y),
           seats_land = land(seats.x, seats.y),
           arr_land = land(arr.x, arr.y),
           seats_netren = netren(seats.x, seats.y),
           arr_netren = netren(arr.x, arr.y),
           cust_cohort = arr.x > 0,
           cust_land = arr.y > 0 & arr.x == 0,
           cust_xpand = arr.y > arr.x & arr.x > 0,
           cust_decl = arr.y < arr.x,
           cust_term = arr.x > 0 & arr.y == 0,
           cust_lost10 = seats.x > 10 & seats.y <= 10) %>%
    data.frame() %>%
    group_by_(...) %>%
    summarise(start_dt = min(fw_month.x, na.rm = TRUE),
              end_dt = max(fw_month.y, na.rm = TRUE),
              ##n = length(odbid),
              n = n_distinct(odbid),
              cohort_n = sum(cust_cohort),
              cust_land = sum(cust_land),
              cust_xpand = sum(cust_xpand),
              cust_decl = sum(cust_decl),
              cust_term = sum(cust_term),
              cust_lost10 = sum(cust_lost10),
              p_cust_xpand = rnd3(sum(cust_xpand) / sum(cust_cohort)),
              avg_arr_xpand = rnd1((sum(arr_xpand) / sum(cust_xpand)) / 10^3),
              p_cust_decl = rnd3(sum(cust_decl) / sum(cust_cohort)),
              avg_arr_decl = rnd1((sum(arr_lost) / sum(cust_decl)) / 10^3),
              agg_seats_begin = sum(seats.x),
              agg_seats_end = sum(seats.y),
              agg_seats_lost = sum(seats_lost),
              agg_seats_dg = sum(seats_dg),
              agg_seats_term = sum(seats_term),
              agg_seats_add = sum(seats_add),
              agg_seats_xpand = sum(seats_xpand),
              agg_seats_land = sum(seats_land),
              agg_seats_netren = sum(seats_netren),
              agg_arr_begin = rnd3(sum(arr.x) / 10^6),
              agg_arr_end = rnd3(sum(arr.y) / 10^6),
              agg_arr_lost = rnd3(sum(arr_lost) / 10^6),
              agg_arr_dg = rnd3(sum(arr_dg) / 10^6),
              agg_arr_term = rnd3(sum(arr_term) / 10^6),
              agg_arr_add = rnd3(sum(arr_add) / 10^6),
              agg_arr_xpand = rnd3(sum(arr_xpand) / 10^6),
              agg_arr_land = rnd3(sum(arr_land) / 10^6),
              agg_arr_netren = rnd3(sum(arr_netren) / 10^6),
              churn_seats = rnd3(sum(seats_lost) / sum(seats.x)),
              p_seats_retain = rnd3(sum(seats_retain) / sum(seats.x)),
              p_seats_xpand = rnd3(sum(seats_xpand) / sum(seats.x)),
              churn_seats_term = rnd3(sum(seats_term) / sum(seats.x)),
              churn_seats_dg = rnd3(sum(seats_dg) / sum(seats.x)),
              churn_arr = rnd3 (sum(arr_lost) / sum(arr.x)),
              p_arr_retain = rnd3(sum(arr_retain) / sum(arr.x)),
              p_arr_xpand = rnd3(sum(arr_xpand) / sum(arr.x)),
              churn_arr_term = rnd3(sum(arr_term) / sum(arr.x)),
              churn_arr_dg = rnd3(sum(arr_dg) / sum(arr.x)),
              churn_cust_10seat = rnd3(sum(cust_lost10) / sum(seats.x >= 10)),
              net_renew_seats = rnd3(sum(seats_netren) / sum(seats.x)),
              net_renew_arr = rnd3(sum(arr_netren) / sum(arr.x))
    )
}

##  cumulative fy charting function
fx_plot_fy <- function(df, y_val, y_lab, t_lab, label_format = "seats"){
  
  if (label_format == "percent") {
    dots <- list(label_point = interp(~paste0(rnd1(100 * y_val), "%"), y_val = as.name(y_val)))
  } else if (label_format == "dollar") {
    dots <- list(label_point = interp(~paste0("$", rnd2(y_val)), y_val = as.name(y_val)))
  } else if (label_format == "seats") {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  } else {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  }
  
  # dots <- list(label_point = interp(~paste0(y_val, "%"), y_val = as.name(y_val)))
  df <- mutate_(df, .dots = dots)
  
  y_val <- as.name(y_val)
  
  ggplot(df, aes_string(x = "factor(month)", 
                        y = y_val, 
                        ymax = paste0(y_val, " * 1.01"),
                        colour = "fy", 
                        shape = "fy",
                        linetype = "fy",
                        fill = "fy",
                        group = "fy",
                        label = "label_point")) +
    geom_line(size = I(2/3)) +
    geom_point(size = sz, 
               alpha = I(1/3)) + 
    scale_shape_manual(values = 21:27) +
    scale_colour_manual(values = my_steel) +
    scale_fill_manual(values = my_steel) +
    ##  this is how to filter to first and last label
    ## geom_text(size = 5, vjust = -1) + 
    ##  geom_text(data = df[c(1, nrow(df)), ], size = 7, vjust = -1) +
    geom_text(data = filter(group_by(df, fy), month == min(month) | month == max(month)), 
              size = 4, 
              vjust = -1) +
    labs(title = t_lab,
         y = y_lab, 
         x = "Month of CA Fiscal Year",
         shape = "CA Fiscal Year",
         colour = "CA Fiscal Year",
         fill = "CA Fiscal Year",
         linetype = "CA Fiscal Year")
}


##  charting function for where grouping variable:  size, strategic
fx_plot_x <- function(df, y_val, z_val, label_format = "seats"){
  
  if (label_format == "percent") {
    dots <- list(label_point = interp(~paste0(rnd1(100 * y_val), "%"), y_val = as.name(y_val)))
  } else if (label_format == "dollar") {
    dots <- list(label_point = interp(~paste0("$", rnd2(y_val)), y_val = as.name(y_val)))
  } else if (label_format == "seats") {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  } else {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  }
  
  df <- mutate_(df, .dots = dots)
  
  y_val <- as.name(y_val)
  z_val <- as.name(z_val)
  
  ggplot(df, 
         aes_string(x = "factor(month)", 
                    y = y_val, 
                    ymax = paste0(y_val, " * 1.01"),
                    colour = z_val, 
                    shape = z_val,
                    fill = z_val,
                    linetype = z_val,
                    group = z_val,
                    label = "label_point")) +
    geom_line(size = I(2/3)) +
    geom_point(size = sz,
               alpha = I(1/3)) + 
    ##  this is how to filter to first and last label
    ## geom_text(size = 5, vjust = -1) + 
    ##  geom_text(data = df[c(1, nrow(df)), ], size = 7, vjust = -1) +
    geom_text(data = filter(df, month == max(month)), 
              size = sz, 
              vjust = -1) +
    scale_color_manual(values = my_clrs) +
    scale_fill_manual(values = my_clrs) +
    scale_shape_manual(values = 21:27) 
}


##  test fx to plot monthly
fx_plot_monthly <- function(df, y_val, z_val, x_lab, y_lab, t_lab, label_format = "seats"){
  
  if (label_format == "percent") {
    dots <- list(label_point = interp(~paste0(rnd0(100 * y_val), "%"), y_val = as.name(y_val)))
  } else if (label_format == "dollar") {
    dots <- list(label_point = interp(~paste0("$", rnd2(y_val)), y_val = as.name(y_val)))
  } else if (label_format == "seats") {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  } else {
    dots <- list(label_point = interp(~y_val, y_val = as.name(y_val)))
  }
  
  df <- mutate_(df, .dots = dots)
  
  y_val <- as.name(y_val)
  z_val <- as.name(z_val)
  
  ggplot(df, 
         aes_string(x = "end_dt", 
                    y = y_val, 
                    ymax = paste0(y_val, " * 1.01"),
                    colour = z_val,
                    fill = z_val,
                    shape = z_val,
                    group = z_val,
                    linetype = z_val,
                    label =  "label_point")) +
    geom_line(size = I(2/3)) +
    geom_point(size = sz, alpha = I(1/3)) + 
    scale_shape_manual(values = 21:27) +
    scale_color_manual(values = my_clrs_2) +
    scale_fill_manual(values = my_clrs_2) +
    ##scale_color_manual(values = my_clrs) +
    ##scale_fill_manual(values = my_clrs) +
    ##  this is how to filter to first and last label
    ##geom_text(size = 5, vjust = -1) + 
    ##geom_text(data = df[c(1, nrow(df)), ], size = 7, vjust = -1) +
    ##  or, tricky group by underscore by z
    ##  geom_text(data = filter(group_by_(df, z_val), 
    ##                         y_val == min(y_val) | y_val == max(y_val) | end_dt == max(end_dt)), 
    geom_text(data = filter(df, end_dt == min(end_dt) | end_dt == max(end_dt)), 
              size = 3, 
              vjust = -1, check_overlap = TRUE)  +
    
    
    labs(title = t_lab,
         x = x_lab,
         y = y_lab, 
         shape = "Legend",
         fill = "Legend",
         colour = "Legend",
         linetype = "Legend")
}

##  clean-up the x_size or x_strat dataframes
fx_clean_df <- function(df, ...){
  data.frame(df) %>%
    group_by_(...) %>%
    arrange(start_dt, end_dt) %>%
    mutate(month = row_number() - 1) %>%
    filter(!is.na(end_dt))
}


## date origin when pulling straight from DB
fmt_dt <- function(x) as.Date(x, origin = "1970-01-01")

my_dt <- function(x) floor_date(as.Date(x, origin = "1970-01-01"), "month")

cur_mon <- function() floor_date(Sys.Date(), "month")

##  setting up some stuff like the default color, size for all
theme_set(theme_bw(base_size = 18))

mytheme <- theme(text = element_text(size = 18), 
                 title = element_text(size = 18), 
                 legend.position = "top")
#                 axis.title.x = element_blank())


h <- 160
w <- h * 2
sz <- 3
shp <- 21
my_clrs <- c("black",  "steelblue4", "darkcyan", "orangered")
my_clrs_2 <- c("black",  "steelblue4", "steelblue1","darkcyan", "orangered")
my_steel <- c("black", "steelblue4", "steelblue3", "steelblue2", "steelblue1")
clrs3 <- c("black", "steelblue4", "orangered")
col1 <- "steelblue4"

##update for change in default justification in ggplot 2.2.0
theme_update(plot.title = element_text(hjust = 0.5))


##  build raw flywheel data ----------------------------------------------------------------------
## a separate history file for Rally only data has been maintained
## this code is run to generate flywheel for Rally historic info
## if Rally data is not going to change, then the output of this code may be saved and loaded without running this code everytime
## WK ACTION : NEED TO SAVE VERSION OF THIS OUTPUT AND PROVIDE FILE NAME

##for existing flywheel process
d_oli0_ODB <- read.csv("rally_historical_transaction_data.csv", as.is = TRUE)

## MUST! - use these format functions to fix dates loaded from Excel/csv files - after this must use the 1970 origin
d_oli0_ODB$start_month <- as.Date(d_oli0_ODB$start_month, origin = "1899-12-30", format = "%Y-%m-%d")
d_oli0_ODB$end_month <- as.Date(d_oli0_ODB$end_month, origin = "1899-12-30", format = "%Y-%m-%d")

##only use next block of code if need ODB expand grid
d_oli_ODB <- d_oli0_ODB %>%
  filter(end_month > start_month & order_bridge == 0)

##generate list for expand.grid 
l_oli_ODB <- d_oli_ODB %>% 
  # filter(sku_family != "Flowdock") %>% 
  select(oli_seqid, start_month, end_month) %>% 
  as.list()

##  apply fw_expand via purrr::pmap to create mini flywheels for each line item
##WK edit - added account_segment to left_join
d_fw_oli_ODB <- l_oli_ODB %>% 
  pmap(fw_expand_ODB) %>% 
  bind_rows() %>% 
  left_join(select(d_oli_ODB, 
                   oli_seqid, odbid, subid, order_seqid, focus_list, sku_base, sku_perpetual, sku_family, sku_subscription, sku_ondemand, sku_onprem, start_month, end_month, arr_odb, seats_odb, ca_account_id, ca_account_name, geo, operating_area, sales_region, account_segment), 
            by = "oli_seqid") %>% 
    data.frame()

##to export expanded ODB
##write_csv(d_fw_oli_ODB, 
##          "~/flywheel/tables/final_ODB_expand_grid.csv")

##END

##load the final ODB expand grid data if it has been saved out as output from code above
##d_fw_oli_ODB <- read.csv("final_ODB_expand_grid.csv", as.is = TRUE)

##d_fw_oli_ODB$start_month <- as.Date(d_fw_oli_ODB$start_month, origin = "1899-12-30", format = "%Y-%m-%d")
##d_fw_oli_ODB$end_month <- as.Date(d_fw_oli_ODB$end_month, origin = "1899-12-30", format = "%Y-%m-%d")
##d_fw_oli_ODB$fw_month <- as.Date(d_fw_oli_ODB$fw_month, origin = "1899-12-30", format = "%Y-%m-%d")


##  SAP data flywheel
## separate process to generate flywheel data from SAP data
d_oli0_EIW <- read.csv("EIW_data_clean_18.10.01_v1.csv", as.is = TRUE)

## MUST! - use these format functions to fix dates loaded from Excel - after this must use the 1970 origin
d_oli0_EIW$start_month <- as.Date(d_oli0_EIW$start_month, origin = "1899-12-30", format = "%Y-%m-%d")
d_oli0_EIW$end_month <- as.Date(d_oli0_EIW$end_month, origin = "1899-12-30", format = "%Y-%m-%d")

## for running FW with CA Account IDs
##d_oli0_EIW$subid <- as.character(d_oli0_EIW$subid)

##filter out bridge orders and anything with an end month before or equal to the start month
d_oli_EIW <- d_oli0_EIW %>%
  filter(end_month > start_month & order_bridge == 0) %>%
  filter(start_month < cur_close_month)

##generate list for expand.grid 
l_oli_EIW <- d_oli_EIW %>% 
  # filter(sku_family != "Flowdock") %>% 
  select(oli_seqid, start_month, end_month) %>% 
  as.list()

##  apply fw_expand via purrr::pmap to create mini flywheels for each line item
##WK edit - added account_segment to left_join
d_fw_oli_EIW <- l_oli_EIW %>% 
  pmap(fw_expand_EIW) %>% 
  bind_rows() %>% 
  left_join(select(d_oli_EIW, 
                   oli_seqid, odbid, subid, order_seqid, focus_list, sku_base, sku_perpetual, sku_family, sku_subscription, sku_ondemand, sku_onprem, start_month, end_month, arr_odb, seats_odb, ca_account_id, ca_account_name, geo, operating_area, sales_region, account_segment), 
            by = "oli_seqid") %>% 
  data.frame()

##to export this
##write_csv(d_fw_oli_EIW, 
##          paste0("~/flywheel/d_fw_EIW_", cur_dt, ".csv"))

##bind ODB and EIW data together
d_fw_bind <- bind_rows(d_fw_oli_ODB,d_fw_oli_EIW)

#to export this
##write_csv(d_fw_bind, 
##          paste0("~/flywheel/d_fw_bind_all_", cur_dt, ".csv"))

##to filter out past dates - when we want to look into future FW behavior
##d_fw_bind <- d_fw_bind %>%
##  filter(fw_month > fy0)

##to filter out future dates
## to see all future flywheel dates (handy for looking at future renewals) do not run this filter snippet
d_fw_bind <- d_fw_bind %>%
              filter(fw_month < cur_close_month)

#to export this
##write_csv(d_fw_bind, 
##          paste0("~/flywheel/tables/d_fw_bind_", cur_dt, ".csv"))

##to filter out future dates + AT&T history - to normalize charts without AT&T
##d_fw_bind <- d_fw_bind %>%
##  filter(fw_month < cur_close_month, odbid != '7000152')


##CAREFUL - RUN NEXT SECTION OF CODE ONLY IF LOOKING AT SaaS-only --- !!!!!!!!!!
##AD HOC - to filter to On-Prem/Perpetual/etc
##d_fw_bind <- d_fw_bind %>%
##  filter(sku_subscription == 1, sku_onprem == 0)


## with raw flywheel data build various data frames for summary and charting (data wrangling code)--------------------------------------------

## primary summary of flywheel - each Account with ARR and Seats by Month, can be enriched with various segment data
## ORIGINAL VERSION - ONLY USED ODBID ORIGINALLY

##flywheel data frame for running all summaries and charting based on ODB ID (or CA Account ID)
d_fw <- d_fw_bind %>% 
  group_by(odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>%
  data.frame()

## flywheel data frame for exporting to file and sharing for analysis outside of flywheel code
d_fw_full <- d_fw_bind %>% 
  group_by(odbid, ca_account_id, ca_account_name, focus_list, account_segment, geo, operating_area, sales_region, sku_perpetual, sku_onprem, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>%
  ##filter(fw_month > '2015-07-31') %>%
  data.frame()


#to export this
write_csv(d_fw_full, 
          paste0("~/flywheel/tables/d_fw_full_", cur_dt, ".csv"))
##to export on-prem version
##write_csv(d_fw, 
##          paste0("~/flywheel/tables/d_fw_odbID_OP_", cur_dt, ".csv"))

## VARIOUS AD HOC SUMMARIES - NOT PART OF REGULAR FLYWHEEL PRODUCTION ------------------------------------------------

## AD HOC - top 20 customeranalysis
d_fw_top20 <- d_fw %>%
  filter(fw_month == cur_fw) %>%
  arrange(desc(arr_odb)) %>%
  top_n(20)

d_fw_top20 <- d_fw_top20  %>%
  select(-fw_month, -arr_odb, -seats_odb)

d_fw_top20 <- left_join(d_fw_top20, d_fw, by = 'odbid') %>%
  filter(fw_month < cur_close_month)
  data.frame()
  
## go to adhoc.R to run code and flywheel_plot_top20.R for charting 

##AD HOC - to get CA Account ID infomation
#d_fw_CAAC <- d_fw_bind %>% 
#  group_by(account_segment, ca_account_id, fw_month) %>% 
#  summarize(arr_odb = sum(arr_odb),
#            seats_odb = sum(seats_odb)) %>% 
#  data.frame()

#to export this
#write_csv(d_fw_CAAC, 
#          paste0("~/flywheel/tables/d_fw_CAAC_", cur_dt, ".csv"))

#to export this
##write_csv(d_fw_CAAC, 
##          paste0("~/flywheel/tables/d_fw_CAAC_OP_", cur_dt, ".csv"))


##AD HOC - to get flywheel by contract line item
d_fw_lines <- d_fw_bind %>% 
  group_by(ca_account_id, order_seqid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>%
  filter(fw_month >= "2016-10-01")
  data.frame()

#to export this
write_csv(d_fw_lines, 
          paste0("~/flywheel/tables/d_fw_lines_renewals_", cur_dt, ".csv"))


##AD HOC analysis on customer tenure - data frame wrangle - go to adhoc.R file for plotting
d_fw_subid <- d_fw_bind %>% 
  group_by(subid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()
#to export this
write_csv(d_fw_subid, 
          paste0("~/flywheel/tables/d_fw_CA_subid_", cur_dt, ".csv"))

d_fw_start <- d_fw_subid %>%
  group_by(ca_account_id) %>%
  summarize(initial_order_date = min(fw_month),
            final_date = max(fw_month),
            max_seats = max(seats_odb),
            max_arr = max(arr_odb)) %>%
  filter(final_date < as.Date("2018-03-01")) %>%
  mutate(years_as_customer = rnd3(as.numeric(interval(initial_order_date,final_date),"years")))

d_fw_final <- d_fw_subid %>%
  group_by(ca_account_id) %>%
  filter(fw_month == max(fw_month))
  
d_fw_join <- left_join(d_fw_start, d_fw_final, by = 'ca_account_id') %>%
    select(-fw_month) %>%
    data.frame()

d_fw_join <- rename(d_fw_join, arr_lost = arr_odb)
d_fw_join <- rename(d_fw_join, seats_lost = seats_odb)

#to export this
write_csv(d_fw_join, 
          "~/flywheel/tables/lost_subid_start_date.csv")

##AD HOC analysis on customer tenure - this is by odbid
d_fw_odbid <- d_fw_bind %>% 
  group_by(odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()


d_fw_start_parent <- d_fw_odbid %>%
  group_by(odbid) %>%
  summarize(start_date = min(fw_month),
            max_seats = max(seats_odb),
            max_arr = max(arr_odb))

#to export this
write_csv(d_fw_start_parent, 
          "~/flywheel/tables/odbid_start_date.csv")
##END AD HOC
## back to wrangle code -------------------------------------------

lg <- 12 ##to run 12 month lag for YoY comparisons

x_month <- d_fw %>%
  filter(fw_month >= fw_start_date) %>%
  group_by(fw_month) %>%
  summarize(arr = sum(arr_odb),
            paidseats = sum(seats_odb),
            customers = n_distinct(odbid),
            seats10 = sum(seats_odb >= 10),
            seats100 = sum(seats_odb >= 100),
            seats1000 = sum(seats_odb >= 1000)) %>%
  mutate(lagged_seats = lag(paidseats, lg),
         lagged_seats_delta = paidseats - lagged_seats,
         yoy_seats = rnd3((paidseats - lagged_seats) / lagged_seats),
         yoy_arr = rnd3((arr - lag(arr, lg)) / lag(arr, lg)),
         yoy_customers = rnd3((customers - lag(customers, lg)) / lag(customers, lg))) %>%
  data.frame()
##WK edit changed fwid to odbid, date to fw_month, and in summarize arr to arr_odb and seats to seats_odb

#to export this
##write_csv(x_month, 
##          paste0("~/flywheel/tables/x_month", cur_dt, ".csv"))


##  using tidyr to re-organize data for improved charting
x_gath <- x_month %>%
  mutate(arr_m = rnd2(arr / 10^6),
         seats_k = rnd2(paidseats / 10^3)) %>%
  filter(!is.na(yoy_seats)) %>%
  gather(key = measure, value = y_val, -fw_month)

#to export this
##write_csv(x_gath, 
##          paste0("~/flywheel/tables/x_gath", cur_dt, ".csv"))



##  things that recur to clean-up: 
fw_mx_dt <- max(d_fw$fw_month)
##WK edit - changed date to fw_month

year_seq <- seq(from = fw_start_date, ##need to manually update this each new FY
                to = fy0, 
                by = "year")

cum_fy <- year_seq %>%
  map_df(~ data_frame(x = .,
                      y = seq(from = .,
                              to = . + months(12),
                              by = "months")
  )
  )

cum_fy <- cum_fy[cum_fy$y <= fw_mx_dt,]

x_fy <- map2_df(cum_fy$x, cum_fy$y, ~ fx_calc_churn_NEW(d_fw, .x, .y, "fw_month"))


##  minor wrangling to get the fiscal year labels right and add month labels
##  note last year's last month is the first (month 0) of current
x_fy <- x_fy %>%
  filter(!is.na(end_dt)) %>%
  arrange(start_dt, end_dt) %>%
  mutate(fy = factor(year(start_dt + months(12)), 
                     levels = 2019:2014)) %>%
  group_by(fy) %>%
  mutate(month = row_number() - 1) %>%
  data.frame()

#to export this
write_csv(x_fy, 
          paste0("~/flywheel/tables/master_fy_summary_", cur_dt, ".csv"))

##now for fiscal year view by size -- to chart cumulative views by size
x_fy_size <- map2_df(cum_fy$x, cum_fy$y, ~ fx_calc_churn_NEW(d_fw, .x, .y, "fw_month", "size_cat"))

##write_csv(x_fy, 
##          paste0("~/flywheel/tables/master_fy_summary_size_", cur_dt, ".csv"))



x_fy_size <- x_fy_size %>%
  filter(!is.na(end_dt)) %>%
  arrange(start_dt, end_dt) %>%
  mutate(fy = factor(year(start_dt + months(12)), 
                     levels = 2019:2014)) %>%
  group_by(fy) %>%
  mutate(month = row_number() - 1) %>%
  data.frame()

##create frames to allow fiscal year views by size
x_fy_size_L <- x_fy_size %>% filter(size_cat == ">=1000")
x_fy_size_M <- x_fy_size %>% filter(size_cat == "<1000")
x_fy_size_S <- x_fy_size %>% filter(size_cat == "<100")


#to export this
##write_csv(x_fy_size, 
##          paste0("~/flywheel/tables/master_fy_summary_size_", cur_dt, ".csv"))


month_seq <- seq(from = fw_start_date, 
                 to = cur_close_month, 
                 by = "months")

##in case need to look at month over month version instead of YoY - currently not used in flywheel
##special monthly version by size
#x_1m_date <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw, .x, .x + months(1), "fw_month"))

##to export this
#write_csv(x_1m_date, 
#          paste0("~/flywheel/tables/master_monthly_summary_", cur_dt, ".csv"))

##special monthly version by size
#x_1m_date_size <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw, .x, .x + months(1), "fw_month", "size_cat"))

##to export this
#write_csv(x_1m_date_size, 
#          paste0("~/flywheel/tables/master_monthly_summary_size_", cur_dt, ".csv"))

##main production of YoY data
x_12m_date <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw, .x, .x + months(12), "fw_month"))

##to export this
write_csv(x_12m_date, 
          paste0("~/flywheel/tables/master_rolling_summary_", cur_dt, ".csv"))


x_12m_date_size <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw, .x, .x + months(12), "fw_month", "size_cat"))

x_12m_date_size <- x_12m_date_size %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date, is.na(size_cat) == FALSE)

##to export this
write_csv(x_12m_date_size, 
          paste0("~/flywheel/tables/master_rolling_summary_size_", cur_dt, ".csv"))

#use this filter to clean up export file for sharing
#x_12m_date_size <- x_12m_date_size %>%
#  filter(end_dt < cur_close_month, end_dt >= fw_start_date, is.na(size_cat) == FALSE)

##  net renewals
x_netrens <- x_12m_date %>%
  select(end_dt, net_renew_arr, net_renew_seats) %>%
  gather(key = measure, value = net_renew, -end_dt) %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date)

## rolling 12 month renewal rate -- NEW
x_renew <- x_12m_date %>%
  select(end_dt, p_arr_retain, p_seats_retain) %>%
  gather(key = measure, value = renewal, -end_dt) %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date)


##  single summary output of the three seat churn measures
x_seat_churns <- x_12m_date %>%
  select(end_dt, churn_seats, churn_seats_term, churn_seats_dg) %>%
  gather(key = measure, value = percent_churn, -end_dt) %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date)


##  single summary output of the seats and arr side x side
x_churns <- x_12m_date %>%
  select(end_dt, churn_arr, churn_seats, churn_cust_10seat) %>%
  gather(key = measure, value = percent_churn, -end_dt) %>%
  mutate(measure = my_lvls(measure, c("churn_arr", "churn_seats", "churn_cust_10seat"))) %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date)

## fy by size, thanks lazy eval ...
## these are used in average price section of flywheel code
x_size <- data.frame(date0 = as.Date(character()),
                     stringsAsFactors = FALSE)


x_size <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw, fy0, .x, "fw_month", "size_cat"))
##WK edit change date to fw_month and fx_calc_churn to fx_calc_churn_NEW

x_size <- fx_clean_df(x_size, "size_cat")


##CHARTING main metrics deck charts -----------------------------------------------------
##not all charts that may appear in a metric deck are in this section of code, others are produced when underlying data is generated

## simple aggregate trends
p_facet <- ggplot(x_gath %>% 
                    filter(measure %in% c("arr_m", "seats_k", "customers")) %>% 
                    mutate(measure = my_lvls(measure, c("customers", "arr_m", "seats_k"))), 
                  aes(x = fw_month, 
                      y = y_val,
                      ymax = y_val * 1.05,
                      label = rnd0(y_val))) + 
  geom_line(colour = col1) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  geom_point(size = sz,
             colour = col1,
             alpha = I(3/4)) + 
  geom_text(size = 3, vjust = -1, check_overlap = TRUE) +
  labs(title = "Customers, ARR, and Seats",
       x = "Month",
       y = "Aggregate",
       shape = "Measure",
       fill = "Measure",
       colour = "Measure",
       linetype = "Measure") +
  facet_grid(measure ~ ., scales = "free") +
  mytheme

p_facet

ggsave("~/flywheel/exhibits/001_p_facet.png", p_facet, width = w, height = h, units = "mm")



##  year over year percent changes in aggregate trends
d_yoy <- x_gath %>% 
  filter(str_detect(measure, fixed("yoy_"))) %>% 
  mutate(measure = my_lvls(measure, c("yoy_arr", "yoy_seats", "yoy_customers")))

p_yoy <- ggplot(d_yoy, 
                aes(x = fw_month, 
                    y = y_val, 
                    ymax = max(y_val) * 1.01,
                    group = measure,
                    colour = measure,
                    shape = measure,
                    linetype = measure,
                    label = paste0(rnd2(y_val) * 100, "%"))) + 
  geom_line(size = I(2/3)) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  geom_point(size = sz,
             alpha = I(1/3)) + 
  geom_text(data = filter(d_yoy, fw_month == min(fw_month) | fw_month == max(fw_month)),
            size = 4, 
            vjust = -1) +
  scale_colour_manual(values = my_clrs) +
  scale_y_continuous(label = percent) +
  ##  this is how to filter to first and last label
  ##  geom_text(size = 5, vjust = -1) + 
  ##  geom_text(data = df[c(1, nrow(df)), ], size = 7, vjust = -1) +
  
  labs(title = "Year-over-Year Change",
       x = "Month",
       y = "Percent",
       shape = "Measure",
       fill = "Measure",
       colour = "Measure",
       linetype = "Measure") +
  
  mytheme

p_yoy

ggsave("~/flywheel/exhibits/002_p_yoy.png", p_yoy, width = w, height = h, units = "mm")



##  landing new customers 
p_landing_customers_fym <- fx_plot_fy(df = x_fy, 
                                      y_val = "cust_land",
                                      y_lab = "Customers",
                                      t_lab = "Landing:  Cumulative New Customers",
                                      label_format = "seats") +
  mytheme

p_landing_customers_fym

ggsave("~/flywheel/exhibits/003_p_landing_customers_fym.png", p_landing_customers_fym, width = w, height = h, units = "mm")



##  landing arr from customers new to the cohort
p_landing_arr_fym <- fx_plot_fy(df = x_fy, 
                                y_val = "agg_arr_land",
                                y_lab = "ARR (millions)",
                                t_lab = "Landing:  Cumulative New Customer ARR",
                                label_format = "dollar") +
  scale_y_continuous(label = dollar) +
  mytheme

p_landing_arr_fym

ggsave("~/flywheel/exhibits/004_p_landing_arr_fym.png", p_landing_arr_fym, width = w, height = h, units = "mm")



##customers lost in aggregate
p_lost_cust_fym <- fx_plot_fy(df = x_fy, 
                              y_val = "cust_term", 
                              y_lab = "Cumulative Customers Lost",
                              t_lab = "Retaining:  Cumulative Customers Lost",
                              label_format = "seats") +
  ##scale_y_continuous(labels = cscale) +
  mytheme

p_lost_cust_fym

ggsave("~/flywheel/exhibits/005_p_lost_cust_fym.png", p_lost_cust_fym, width = w, height = h, units = "mm")



##  arr lost in aggregate
p_lost_arr_fym <- fx_plot_fy(df = x_fy, 
                             y_val = "agg_arr_lost", 
                             y_lab = "Cumulative ARR Lost (millions)",
                             t_lab = "Retaining:  Cumulative ARR Lost",
                             label_format = "dollar") +
  scale_y_continuous(labels = dollar) +
  mytheme

p_lost_arr_fym

ggsave("~/flywheel/exhibits/006_p_lost_arr_fym.png", p_lost_arr_fym, width = w, height = h, units = "mm")



## expanding customer count
p_gross_add_cust_fym <- fx_plot_fy(df = x_fy, 
                                   y_val = "cust_xpand", 
                                   y_lab = "Cumulative Number of Customers Expanding",
                                   t_lab = "Expanding:  Cumulative Number of Customers Expanding",
                                   label_format = "seats") +
  ##scale_y_continuous(labels = cscale) +
  mytheme

p_gross_add_cust_fym

ggsave("~/flywheel/exhibits/007_p_gross_add_cust_fym.png", p_gross_add_cust_fym, width = w, height = h, units = "mm")



## expanding customer count - percentage - cumulative
p_gross_add_cust_perc_fym <- fx_plot_fy(df = x_fy, 
                                        y_val = "p_cust_xpand", 
                                        y_lab = "Cumulative Percentage of Customers Expanding",
                                        t_lab = "Expanding:  Cumulative Percentage of Customers Expanding",
                                        label_format = "percent") +
  scale_y_continuous(labels = percent) +
  mytheme

p_gross_add_cust_perc_fym

ggsave("~/flywheel/exhibits/008_p_gross_add_cust_perc_fym.png", p_gross_add_cust_perc_fym, width = w, height = h, units = "mm")



## expanding customer count
p_gross_add_arr_amt_fym <- fx_plot_fy(df = x_fy, 
                                      y_val = "agg_arr_xpand", 
                                      y_lab = "Cumulative ARR Amount - Expanding",
                                      t_lab = "Expanding:  Cumulative ARR",
                                      label_format = "dollar") +
  scale_y_continuous(labels = dollar) +
  mytheme

p_gross_add_arr_amt_fym

ggsave("~/flywheel/exhibits/009_p_gross_add_arr_amt_fym.png", p_gross_add_arr_amt_fym, width = w, height = h, units = "mm")



## expanding customer count - percentage - rolling 12 months
p_cust_expand_sizesegment <- fx_plot_monthly(df = x_12m_date_size, 
                                             y_val = "p_cust_xpand", 
                                             z_val = "size_cat",
                                             x_lab = "fw_month",
                                             y_lab = "expanding (%)",
                                             t_lab = "Expanding: % of customers expanding by size",
                                             label_format = "percent") +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent)
mytheme

p_cust_expand_sizesegment

ggsave("~/flywheel/exhibits/010_p_cust_expand_sizesegment.png", p_cust_expand_sizesegment, width = w, height = h, units = "mm")



##  three big churns trended over time
p_churns <- fx_plot_monthly(df = x_churns,
                            y_val = "percent_churn", 
                            z_val = "measure",
                            x_lab = "Date",
                            y_lab = "Churn (%)",
                            t_lab = "Retaining:  12 Month Churn - Customers, ARR, and Seats",
                            label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churns

ggsave("~/flywheel/exhibits/011_p_churns.png", p_churns, width = w, height = h, units = "mm")



##  retaining arr churn
p_churn_arr_fym <- fx_plot_fy(df = x_fy, 
                              y_val = "churn_arr", 
                              y_lab = "Cumulative ARR Churn (%)",
                              t_lab = "Retaining:  Cumulative CA Fiscal Year ARR Churn",
                              label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_fym

ggsave("~/flywheel/exhibits/012_p_churn_arr_fyms.png", p_churn_arr_fym, width = w, height = h, units = "mm")



##  arr churn by size
p_churn_arr_size <- fx_plot_monthly(df = x_12m_date_size, 
                                    y_val = "churn_arr", 
                                    z_val = "size_cat",
                                    x_lab = "Date",
                                    y_lab = "Churn (%)",
                                    t_lab = "Retaining:  12 Month ARR Churn by Size Segment",
                                    label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_size

ggsave("~/flywheel/exhibits/013_p_churn_arr_size.png", p_churn_arr_size, width = w, height = h, units = "mm")



##  aggregate net renewal trend
p_netrens <- fx_plot_monthly(df = x_netrens, 
                             y_val = "net_renew", 
                             z_val = "measure",
                             y_lab = "Change (%)",
                             x_lab = "Date",
                             t_lab = "Expanding:  12 month Net Renewal",
                             label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_netrens

ggsave("~/flywheel/exhibits/014_p_netrens.png", p_netrens, width = w, height = h, units = "mm")

## aggregate renewal rate trend -- this is NEW as of October 2018, has not been included in metrics decks
p_renewal <- fx_plot_monthly(df = x_renew, 
                             y_val = "renewal", 
                             z_val = "measure",
                             y_lab = "Change (%)",
                             x_lab = "Date",
                             t_lab = "Retaining:  12 month Renewal Rate",
                             label_format = "percent") +
  geom_hline(yintercept = 0.9, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_renewal

ggsave("~/flywheel/exhibits/014b_p_renewal.png", p_renewal, width = w, height = h, units = "mm")


##  expanding arr, net
p_net_arr_fym <- fx_plot_fy(df = x_fy, 
                            y_val = "net_renew_arr", 
                            y_lab = "Cumulative ARR New Renew Rate (%)",
                            t_lab = "Expanding:  Cumulative ARR Net Renew Rate",
                            label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  mytheme

p_net_arr_fym

ggsave("~/flywheel/exhibits/015_p_net_arr_fym.png", p_net_arr_fym, width = w, height = h, units = "mm")



##  arr renew by size
p_net_arr_size <- fx_plot_monthly(df = x_12m_date_size, 
                                  y_val = "net_renew_arr", 
                                  z_val = "size_cat",
                                  x_lab = "Date",
                                  y_lab = "Net Renew (%)",
                                  t_lab = "Expanding: 12 month ARR Net Renewal by Size",
                                  label_format = "percent") +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  mytheme

p_net_arr_size

ggsave("~/flywheel/exhibits/016_p_net_arr_size.png", p_net_arr_size, width = w, height = h, units = "mm")



##  expanding arr, gross
p_gross_add_arr_fym <- fx_plot_fy(df = x_fy, 
                                  y_val = "p_arr_xpand", 
                                  y_lab = "Cumulative ARR Expansion Rate (%)",
                                  t_lab = "Expanding:  Cumulative ARR Gross Expansion Rate",
                                  label_format = "percent") +
  scale_y_continuous(labels = percent) +
  mytheme

p_gross_add_arr_fym

ggsave("~/flywheel/exhibits/017_p_gross_add_arr_fym.png", p_gross_add_arr_fym, width = w, height = h, units = "mm")



## retaining arr churn by size
p_churn_arr_fym_large <- fx_plot_fy(df = x_fy_size_L, 
                                    y_val = "churn_arr", 
                                    y_lab = "Cumulative ARR Churn (%)",
                                    t_lab = "Retaining:  Cumulative Fiscal Year ARR Churn -- Above 1000 Seats",
                                    label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,36), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_churn_arr_fym_large

ggsave("~/flywheel/exhibits/018_p_churn_arr_fyms_large.png", p_churn_arr_fym_large, width = w, height = h, units = "mm")



p_churn_arr_fym_medium <- fx_plot_fy(df = x_fy_size_M, 
                                     y_val = "churn_arr", 
                                     y_lab = "Cumulative ARR Churn (%)",
                                     t_lab = "Retaining:  Cumulative Fiscal Year ARR Churn - 100 to 1000 Seats",
                                     label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(1,4,7,10,13,16,19,22,25,28,31,34,37), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_churn_arr_fym_medium

ggsave("~/flywheel/exhibits/019_p_churn_arr_fyms_medium.png", p_churn_arr_fym_medium, width = w, height = h, units = "mm")



p_churn_arr_fym_small <- fx_plot_fy(df = x_fy_size_S, 
                                    y_val = "churn_arr", 
                                    y_lab = "Cumulative ARR Churn (%)",
                                    t_lab = "Retaining:  Cumulative Fiscal Year ARR Churn - Under 100 Seats",
                                    label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(2,5,8,11,14,17,20,23,26,29,32,35,38), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_churn_arr_fym_small

ggsave("~/flywheel/exhibits/020_p_churn_arr_fyms_small.png", p_churn_arr_fym_small, width = w, height = h, units = "mm")



## expanding net renewal by size
p_netren_fym_large <- fx_plot_fy(df = x_fy_size_L, 
                                 y_val = "net_renew_arr", 
                                 y_lab = "Net Renew Rate (%)",
                                 t_lab = "Expanding:  Cumulmative Fiscal Year Net Renew -- Above 1000 Seats",
                                 label_format = "percent") +
  geom_hline(yintercept = 1.0, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,36), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_netren_fym_large

ggsave("~/flywheel/exhibits/021_p_netren_fyms_large.png", p_netren_fym_large, width = w, height = h, units = "mm")



p_netren_fym_medium <- fx_plot_fy(df = x_fy_size_M, 
                                  y_val = "net_renew_arr", 
                                  y_lab = "Net Renew Rate (%)",
                                  t_lab = "Expanding:  Cumulative Fiscal Year Net Renew - 100 to 1000 Seats",
                                  label_format = "percent") +
  geom_hline(yintercept = 1.0, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(1,4,7,10,13,16,19,22,25,28,31,34,37), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_netren_fym_medium

ggsave("~/flywheel/exhibits/022_p_netren_fyms_medium.png", p_netren_fym_medium, width = w, height = h, units = "mm")



p_netren_fym_small <- fx_plot_fy(df = x_fy_size_S, 
                                 y_val = "net_renew_arr", 
                                 y_lab = "Net Renew Rate (%)",
                                 t_lab = "Expanding:  Cumulative Fiscal Year Net Renew - Under 100 Seats",
                                 label_format = "percent") +
  geom_hline(yintercept = 1.0, linetype = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(2,5,8,11,14,17,20,23,26,29,32,35,38), label = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  mytheme

p_netren_fym_small

ggsave("~/flywheel/exhibits/023_p_netren_fyms_small.png", p_netren_fym_small, width = w, height = h, units = "mm")



## SEGMENTATION ANALYSIS -- create 5 seat size categories rather than standard 3  --------------------------------------------

d_csm <- d_fw %>% 
  mutate(csm_size = factor(ifelse(seats_odb < 50, "<50",
                                  ifelse(seats_odb < 100, "50-99",
                                         ifelse(seats_odb < 500, "100-499",
                                                ifelse(seats_odb < 1000, "500-999", ">=1000")))),
                           levels = c("<50", "50-99", "100-499", "500-999", ">=1000")))

x_csm <- d_csm %>% 
  group_by(csm_size, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()

#to export this
##write_csv(x_csm, 
##          paste0("~/flywheel/csm_summary_", cur_dt, ".csv"))


p_csm <- ggplot(x_csm, 
                aes(x = fw_month,
                    y = n,
                    ymax = 0,
                    shape = csm_size,
                    linetype = csm_size,
                    colour = csm_size)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_continuous(label = comma, limits = c(0, 800)) +
  geom_text(data = filter(x_csm, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  labs(title = "Number of Customers, Grouped by Total Customer Size",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_csm

ggsave("~/flywheel/exhibits/p_csm.png", p_csm, width = w, height = h, units = "mm")

p_csm_size <- ggplot(x_csm, 
                aes(x = fw_month,
                    y = arr,
                    ymax = 0,
                    shape = csm_size,
                    linetype = csm_size,
                    colour = csm_size)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_log10(labels = NULL) +
  geom_text(data = filter(x_csm, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd2(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  labs(title = "Total ARR Under Contract by Customer Size",
       x = "Date", 
       y = "ARR (M)") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")
p_csm_size

ggsave("~/flywheel/exhibits/p_csm_size.png", p_csm_size, width = w, height = h, units = "mm")

##next four charts are typically not shown in the metrics meetings as they are "dense" but interesting to see how different size segments perform
csm_12m_date_size <- map_df(month_seq, ~ fx_calc_churn_NEW(d_csm, .x, .x + months(12), "fw_month", "csm_size_cat"))
##WK edit to change to CSM size category

csm_12m_date_size <- csm_12m_date_size %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date, is.na(csm_size_cat) == FALSE)


p_csm_netrens <- fx_plot_monthly(df = csm_12m_date_size, 
                             y_val = "net_renew_arr", 
                             z_val = "csm_size_cat",
                             y_lab = "Change (%)",
                             x_lab = "Date",
                             t_lab = "Expanding:  12 month Net Renewal - ARR",
                             label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_csm_netrens

ggsave("~/flywheel/exhibits/ARR_NR_CSM_size.png", p_csm_netrens, width = w, height = h, units = "mm")

p_csm_netrens_seats <- fx_plot_monthly(df = csm_12m_date_size, 
                                 y_val = "net_renew_seats", 
                                 z_val = "csm_size_cat",
                                 y_lab = "Change (%)",
                                 x_lab = "Date",
                                 t_lab = "Expanding:  12 month Net Renewal - Seats",
                                 label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_csm_netrens_seats

ggsave("~/flywheel/exhibits/Seats_NR_CSM_size.png", p_csm_netrens_seats, width = w, height = h, units = "mm")

p_churn_csm_arr_size <- fx_plot_monthly(df = csm_12m_date_size, 
                                    y_val = "churn_arr", 
                                    z_val = "csm_size_cat",
                                    x_lab = "Date",
                                    y_lab = "Churn (%)",
                                    t_lab = "Retaining:  12 Month ARR Churn by Size Segment",
                                    label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_csm_arr_size

ggsave("~/flywheel/exhibits/ARR_churn_CSM_size.png", p_churn_csm_arr_size, width = w, height = h, units = "mm")

p_churn_csm_seats_size <- fx_plot_monthly(df = csm_12m_date_size, 
                                        y_val = "churn_seats", 
                                        z_val = "csm_size_cat",
                                        x_lab = "Date",
                                        y_lab = "Churn (%)",
                                        t_lab = "Retaining:  12 Month Seats Churn by Size Segment",
                                        label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_csm_seats_size

ggsave("~/flywheel/exhibits/Seats_churn_CSM_size.png", p_churn_csm_seats_size, width = w, height = h, units = "mm")

## SEGMENTATION ANALYSIS -- by GEO -----------------------------------------------------------------------------

##geo splits not normally discussed in metrics meeting - created in respond to ad hoc request
d_geo <- d_fw_full %>% 
  group_by(geo, odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()

x_geo <- d_geo %>% 
  group_by(geo, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()



#to export this
write_csv(x_geo, 
          paste0("~/flywheel/tables/geo_summary_", cur_dt, ".csv"))

p_geo_cust <- ggplot(filter(x_geo), 
                aes(x = fw_month,
                    y = n,
                    ymax = 0,
                    shape = geo,
                    linetype = geo,
                    colour = geo)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_continuous(label = comma, limits = c(0, 1000)) +
  geom_text(data = filter(x_geo, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  labs(title = "Number of Customers, Grouped by GEO",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_geo_cust

ggsave("~/flywheel/exhibits/p_geo_cust.png", p_geo_cust, width = w, height = h, units = "mm")

p_geo_arr <- ggplot(x_geo, 
                     aes(x = fw_month,
                         y = arr,
                         ymax = 0,
                         shape = geo,
                         linetype = geo,
                         colour = geo)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_log10(labels = NULL) +
  geom_text(data = filter(x_geo, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd2(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  labs(title = "Total ARR Under Contract by GEO",
       x = "Date", 
       y = "ARR (M)") +
  mytheme
  ##theme_bw(base_size = 20) +
  ##theme(legend.position = "top")

p_geo_arr

ggsave("~/flywheel/exhibits/p_geo_arr.png", p_geo_arr, width = w, height = h, units = "mm")


csm_12m_date_geo <- map_df(month_seq, ~ fx_calc_churn_GEO(d_geo, .x, .x + months(12), "fw_month", "geo"))
##WK edit to change to GEO category

csm_12m_date_geo <- csm_12m_date_geo %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date, geo != 0)

##only built ARR net renewal chart for GEO - not seats
p_netrens_geo <- fx_plot_monthly(df = csm_12m_date_geo, 
                                 y_val = "net_renew_arr", 
                                 z_val = "geo",
                                 y_lab = "Change (%)",
                                 x_lab = "Date",
                                 t_lab = "Expanding:  12 month Net Renewal - ARR - by GEO",
                                 label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_netrens_geo

ggsave("~/flywheel/exhibits/ARR_NR_geo.png", p_netrens_geo, width = w, height = h, units = "mm")

##only built ARR churn chart for GEO - not seats
p_churn_arr_geo <- fx_plot_monthly(df = csm_12m_date_geo, 
                                        y_val = "churn_arr", 
                                        z_val = "geo",
                                        x_lab = "Date",
                                        y_lab = "Churn (%)",
                                        t_lab = "Retaining:  12 Month ARR Churn by GEO",
                                        label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_geo

ggsave("~/flywheel/exhibits/ARR_churn_geo.png", p_churn_arr_geo, width = w, height = h, units = "mm")


## SEGMENTATION ANALYSIS -- by CA Account segmentation scheme (in FY19 Platinum, Gold, Silver, Commercial)  --------------------------------------------

d_segment <- d_fw_bind %>% 
  group_by(account_segment, odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()

x_segment <- d_segment %>% 
  group_by(account_segment, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()

#to export this
##write_csv(x_segment, 
##          paste0("~/flywheel/tables/segment_summary_", cur_dt, ".csv"))


p_segment_cust <- ggplot(x_segment, 
                          aes(x = fw_month,
                              y = n,
                              ymax = 0,
                              shape = account_segment,
                              linetype = account_segment,
                              colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = my_clrs) +
  scale_y_continuous(label = comma, limits = c(0, 1100)) +
  geom_text(data = filter(x_segment, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Number of Customers, Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_cust

ggsave("~/flywheel/exhibits/segment_customers.png", p_segment_cust, width = w, height = h, units = "mm")


p_segment_seats <- ggplot(x_segment, 
                aes(x = fw_month,
                    y = seats/1000,
                    ymax = 0,
                    shape = account_segment,
                    linetype = account_segment,
                    colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = my_clrs) +
  scale_y_continuous(label = comma, limits = c(0, 300)) +
  geom_text(data = filter(x_segment, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd0(seats/1000)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Number of Seats ('000), Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_seats

ggsave("~/flywheel/exhibits/segment_seats.png", p_segment_seats, width = w, height = h, units = "mm")

p_segment_arr <- ggplot(x_segment, 
                          aes(x = fw_month,
                              y = arr,
                              ymax = 0,
                              shape = account_segment,
                              linetype = account_segment,
                              colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = my_clrs) +
  scale_y_continuous(label = comma, limits = c(0, 70)) +
  geom_text(data = filter(x_segment, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd1(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Total ARR (millions), Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_arr

ggsave("~/flywheel/exhibits/segment_arr.png", p_segment_arr, width = w, height = h, units = "mm")

segment_12m_date <- map_df(month_seq, ~ fx_calc_churn_SEG(d_segment, .x, .x + months(12), "fw_month", "account_segment"))
##WK edit to change to CSM size category

segment_12m_date <- segment_12m_date %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date, is.na(account_segment) == FALSE, account_segment != 0)

#to export this
write_csv(segment_12m_date, 
          paste0("~/flywheel/tables/segment_summary_", cur_dt, ".csv"))


p_segment_netrens <- fx_plot_monthly(df = segment_12m_date, 
                                 y_val = "net_renew_arr", 
                                 z_val = "account_segment",
                                 y_lab = "Change (%)",
                                 x_lab = "Date",
                                 t_lab = "Expanding: 12 month ARR Net Renewal by Account Segment",
                                 label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  mytheme

p_segment_netrens

ggsave("~/flywheel/exhibits/ARR_NR_Segment.png", p_segment_netrens, width = w, height = h, units = "mm")

p_segment_netrens_seats <- fx_plot_monthly(df = segment_12m_date, 
                                       y_val = "net_renew_seats", 
                                       z_val = "account_segment",
                                       y_lab = "Change (%)",
                                       x_lab = "Date",
                                       t_lab = "Expanding: 12 month Seats Net Renewal by Account Segment",
                                       label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  mytheme

p_segment_netrens_seats

ggsave("~/flywheel/exhibits/Seats_NR_Segment.png", p_segment_netrens_seats, width = w, height = h, units = "mm")

p_churn_segment_arr <- fx_plot_monthly(df = segment_12m_date, 
                                        y_val = "churn_arr", 
                                        z_val = "account_segment",
                                        x_lab = "Date",
                                        y_lab = "Churn (%)",
                                        t_lab = "Retaining:  12 Month ARR Churn by Account Segment",
                                        label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  mytheme

p_churn_segment_arr

ggsave("~/flywheel/exhibits/ARR_churn_Segment.png", p_churn_segment_arr, width = w, height = h, units = "mm")

p_churn_segment_seats <- fx_plot_monthly(df = segment_12m_date, 
                                          y_val = "churn_seats", 
                                          z_val = "account_segment",
                                          x_lab = "Date",
                                          y_lab = "Churn (%)",
                                          t_lab = "Retaining:  12 Month Seats Churn by Account Segment",
                                          label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  mytheme

p_churn_segment_seats

ggsave("~/flywheel/exhibits/Seats_churn_Segment.png", p_churn_segment_seats, width = w, height = h, units = "mm")


## SEGMENTATION ANALYSIS -- by Broadcom Focus List -----------------------------------------------------------------------------
##NEW - Broadcom focus list splits - created to show how flywheel looks under that segmentation scheme 
d_focus <- d_fw_full %>% 
  group_by(focus_list, odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()

x_focus <- d_focus %>% 
  group_by(focus_list, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()

x_focus$focus_list <- as.factor(x_focus$focus_list)

#to export this
write_csv(x_focus, 
          paste0("~/flywheel/tables/focus_summary_", cur_dt, ".csv"))

p_focus_cust <- ggplot(filter(x_focus), 
                     aes(x = fw_month,
                         y = n,
                         ymax = 0,
                         shape = focus_list,
                         linetype = focus_list,
                         colour = focus_list)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "black")) +
  scale_y_continuous(label = comma, limits = c(0, 1200)) +
  geom_text(data = filter(x_focus, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  labs(title = "Number of Customers, Grouped by Focus List",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_focus_cust

ggsave("~/flywheel/exhibits/p_focus_cust.png", p_focus_cust, width = w, height = h, units = "mm")

p_focus_arr <- ggplot(x_focus, 
                    aes(x = fw_month,
                        y = arr,
                        ymax = 0,
                        shape = focus_list,
                        linetype = focus_list,
                        colour = focus_list)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "black")) +
  scale_y_log10(labels = NULL) +
  geom_text(data = filter(x_focus, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd2(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  labs(title = "Total ARR Under Contract by Focus List",
       x = "Date", 
       y = "ARR (M)") +
  mytheme
##theme_bw(base_size = 20) +
##theme(legend.position = "top")

p_focus_arr

ggsave("~/flywheel/exhibits/p_focus_arr.png", p_focus_arr, width = w, height = h, units = "mm")


csm_12m_date_focus <- map_df(month_seq, ~ fx_calc_churn_FOCUS(d_focus, .x, .x + months(12), "fw_month", "focus"))
##WK edit to change to focus category

csm_12m_date_focus <- csm_12m_date_focus %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date)

csm_12m_date_focus$focus <- as.factor(csm_12m_date_focus$focus)

#to export this
write_csv(csm_12m_date_focus, 
          paste0("~/flywheel/tables/focus_12m_detail_", cur_dt, ".csv"))


##only built ARR net renewal chart for focus - not seats
p_netrens_focus <- fx_plot_monthly(df = csm_12m_date_focus, 
                                 y_val = "net_renew_arr", 
                                 z_val = "focus",
                                 y_lab = "Change (%)",
                                 x_lab = "Date",
                                 t_lab = "Expanding:  12 month Net Renewal - ARR - by focus",
                                 label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_netrens_focus

ggsave("~/flywheel/exhibits/ARR_NR_focus.png", p_netrens_focus, width = w, height = h, units = "mm")

##only built ARR net renewal chart for focus - not seats
p_renewal_focus <- fx_plot_monthly(df = csm_12m_date_focus, 
                                   y_val = "p_arr_retain", 
                                   z_val = "focus",
                                   y_lab = "Change (%)",
                                   x_lab = "Date",
                                   t_lab = "Expanding:  12 month Renewal Rate - ARR - by focus",
                                   label_format = "percent") +
  geom_hline(yintercept = 0.9, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_renewal_focus

ggsave("~/flywheel/exhibits/ARR_RR_focus.png", p_renewal_focus, width = w, height = h, units = "mm")


##only built ARR churn chart for focus - not seats
p_churn_arr_focus <- fx_plot_monthly(df = csm_12m_date_focus, 
                                   y_val = "churn_arr", 
                                   z_val = "focus",
                                   x_lab = "Date",
                                   y_lab = "Churn (%)",
                                   t_lab = "Retaining:  12 Month ARR Churn by Focus List",
                                   label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_focus

ggsave("~/flywheel/exhibits/ARR_churn_focus.png", p_churn_arr_focus, width = w, height = h, units = "mm")



## Average Seat Price --------------------------------------------

d_asp0 <- d_fw_bind %>%
  filter(sku_family == "UE") %>%
  filter(sku_perpetual == 0) %>%
  filter(sku_base == 1) %>%
  filter(arr_odb != 0)

d_asp <- d_asp0 %>% 
  group_by(account_segment, odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()

d_asp <- d_asp %>% 
  mutate(asp_x_id = round((arr_odb / seats_odb) / 12, 2),
         size_test = factor(ifelse(seats_odb < 25, "<25",
                                   ifelse(seats_odb < 50, "25-49",
                                          ifelse(seats_odb < 200, "50-199",
                                                 ifelse(seats_odb < 500, "200-499",
                                                        ifelse(seats_odb < 2000, "500-1999", ">=2000"))))),
                            levels = c("<25", "25-49", "50-199", "200-499", "500-1999", ">=2000")),
         size_ue = ifelse(seats_odb < 10^2, "<100", 
                          ifelse(seats_odb < 10^3, "<1000", ">=1000")),
         size_jeff = ifelse(seats_odb < 10^2, "<100", 
                            ifelse(seats_odb < 10^3, "<1000",
                                   ifelse(seats_odb < 5 * 10^3, "<5000", ">=5000"))))

##write_csv(d_asp, 
##          paste0("~/flywheel/AC_asp_detail", cur_dt, ".csv"))


x_month <- d_asp %>% 
  group_by(fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb), 
            size_jeff = "aggregate") %>% 
  ungroup() %>% 
  mutate(asp = round((arr / 12) / seats, 1)) %>%
  # filter(date > "2014-01-01" & (month(date) %in% c(3, 6, 9, 12) | date == max(date))) %>% 
  filter(fw_month > "2012-03-01", fw_month < cur_close_month) %>% 
  select(size_jeff, fw_month, n, seats, arr, asp) %>% 
  data.frame()

x_month_segment <- d_asp %>% 
  group_by(fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb), 
            account_segment = "aggregate") %>% 
  ungroup() %>% 
  mutate(asp = round((arr / 12) / seats, 1)) %>%
  # filter(date > "2014-01-01" & (month(date) %in% c(3, 6, 9, 12) | date == max(date))) %>% 
  filter(fw_month > "2012-03-01", fw_month < cur_close_month) %>% 
  select(account_segment, fw_month, n, seats, arr, asp) %>% 
  data.frame()

x_size_odb <- d_asp %>% 
  # filter(date > "2014-01-01" & (month(date) %in% c(1, 4, 7, 10) | date == max(date))) %>% 
  filter(fw_month > "2012-03-01", fw_month < cur_close_month) %>% 
  group_by(size_test, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)) %>% 
  ungroup() %>% 
  mutate(asp = round((arr / 12) / seats, 1)) %>% 
  data.frame()
##x_size_odb

x_asp_jeff <- d_asp %>% 
  group_by(size_jeff, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)) %>% 
  ungroup() %>% 
  mutate(asp = round((arr / 12) / seats, 1)) %>% 
  bind_rows(x_month) %>% 
  mutate(sized = size_jeff != "aggregate") %>% 
  # filter(date > "2014-01-01" & (month(date) %in% c(3, 6, 9, 12) | date == max(date))) %>% 
  filter(fw_month > "2012-03-01", fw_month < cur_close_month) %>% 
  data.frame()
##x_asp_jeff

##write_csv(x_asp_jeff, 
##          paste0("~/flywheel/AC_asp_", cur_dt, ".csv"))

x_asp_segment <- d_asp %>% 
  group_by(account_segment, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)) %>% 
  ungroup() %>% 
  mutate(asp = round((arr / 12) / seats, 1)) %>% 
  bind_rows(x_month) %>% 
  mutate(sized = is.na(account_segment) != TRUE) %>% 
  # filter(date > "2014-01-01" & (month(date) %in% c(3, 6, 9, 12) | date == max(date))) %>% 
  filter(fw_month > "2012-03-01", fw_month < cur_close_month) %>% 
  data.frame()
##x_asp_segment

##write_csv(x_asp_segment, 
##          paste0("~/flywheel/AC_asp_segment", cur_dt, ".csv"))



##d_curr_scatt <- d_asp %>% filter(fw_month == max(fw_month) & size_ue == "<1000")
##have to change to hard-coded date - max(fw_month) no longer gives current close month

d_curr_scatt <- d_asp %>% filter(fw_month == "2018-08-01" & size_ue == "<1000")

##v_curr_mean <- d_curr_scatt %>% 
##summarize(asp_avg = round(sum(arr_odb / 12) / sum(seats_odb), 0))

v_curr_mean <- d_curr_scatt %>% 
  summarize(asp_avg = round(sum(arr_odb / 12) / sum(seats_odb), 0))

## average seat price -- plotting --------------------------------------------
# ugly
#p_month <- ggplot(x_month, 
#                  aes(x = fw_month,
#                      y = asp,
#                      ymax = asp * 1.02)) +
#  geom_point(size = 5, alpha = I(3/4)) +
#  geom_line() +
#  scale_colour_manual(values = c("black", "steelblue4", "steelblue3", "steelblue2", "orangered")) +
#  scale_y_continuous(label = dollar, limits = c(10, 50)) +
#  geom_text(aes(label = paste0("$", asp)), size = 4, vjust = -3, check_overlap = TRUE) +
#  geom_text(aes(label = paste0("N=", n)), size = 4, vjust = -1.5) +
#  labs(title = "UE SaaS Average Seat Price (ASP)",
#       x = "Date", 
#       y = "UE ASP") +
#  theme_bw(base_size = 24) +
#  theme(legend.position = "top") 
#p_month

# also ugly
#p_size_odb <- ggplot(filter(x_size_odb, 
#                            fw_month >= "2014-08-01"), 
#                     aes(x = fw_month,
#                         y = asp,
#                         shape = size_test,
#                         colour = size_test,
#                         label = paste0("$", asp))) +
#  geom_point(size = 5, alpha = I(3/4)) +
#  geom_line() +
#  scale_colour_manual(values = rev(c("black", "steelblue4", "steelblue3", "steelblue2", "steelblue1", "orangered"))) +
#  scale_y_continuous(label = dollar, limits = c(10,50)) +
#  geom_text(size = 4, vjust = -1, check_overlap = TRUE) +
#  labs(title = "UE SaaS Average Seat Price (ASP), Grouped by Total Customer Size",
#       x = "Date", 
#       y = "UE ASP",
#       shape = "ODB Sizes",
#       colour = "ODB Sizes") +
#  theme_bw(base_size = 20) +
#  theme(legend.position = "top")
#p_size_odb

#ggsave("~/flywheel/exhibits/ASP2.png", p_size_odb, width = w, height = h, units = "mm")


p_asp_jeff <- ggplot(x_asp_jeff, 
                     aes(x = fw_month,
                         y = asp,
                         ymax = asp * 1.04,
                         shape = size_jeff,
                         linetype = size_jeff,
                         colour = size_jeff)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_continuous(label = dollar, limits = c(10, 50)) +
  geom_text(data = filter(x_asp_jeff, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = paste0("$", asp)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  # geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  labs(title = "UE SaaS Average Seat Price (ASP), Grouped by Total Customer Size",
       x = "Date", 
       y = "UE ASP",
       shape = "Seat Size Category",
       linetype = "Seat Size Category",
       colour = "Seat Size Category") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top") +
  facet_grid(sized ~ ., labeller = label_both) 
p_asp_jeff

ggsave("~/flywheel/exhibits/ASP.png", p_asp_jeff, width = w, height = h, units = "mm")


##not discussed in metrics meeting
p_curr_scatt <- ggplot(d_curr_scatt,
                       aes(x = seats_odb,
                           y = asp_x_id)) +
  geom_jitter(position = position_jitter(width = .5),
              size = 7,
              alpha = I(1/3)) +
  geom_hline(yintercept = v_curr_mean[1,1],
             linetype = 2) +
  geom_smooth(method = "lm") +
  scale_y_continuous(label = dollar) +
  annotate("text", 
           x = 800, 
           y = v_curr_mean[1,1] + 1, 
           label = paste0("Segment Mean ASP: $", v_curr_mean[1,1])) +
  labs(x = "UE Seats",
       y = "UE ASP",
       title = "UE ASP in the 100-1000 Seat Segment") +
  theme_bw(base_size = 24) 
p_curr_scatt


############################################################################################################
## SEGMENTATION ANALYSIS charting for on-prem reporting -- must rerun d_fw with filter on for on_prem only ---------------------
## be careful -- am changing charting parameters for different data set
## must rerun all of flywheel code but with only on-prem activity passing through filter


##CAREFUL - RUN NEXT SECTION OF CODE ONLY IF LOOKING AT ON-PREM --- !!!!!!!!!!
##AD HOC - to filter to On-Prem/Perpetual/etc
d_fw_bind_op <- d_fw_bind %>%  
  filter(sku_onprem == 1)

d_fw_op <- d_fw_bind_op %>% 
  group_by(odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>%
  data.frame()

#to export this
write_csv(d_fw_op, 
          paste0("~/flywheel/tables/d_fw_odbID_op", cur_dt, ".csv"))


d_csm_op <- d_fw_op %>% 
  mutate(csm_size = factor(ifelse(seats_odb < 50, "<50",
                                  ifelse(seats_odb < 100, "50-99",
                                         ifelse(seats_odb < 500, "100-499",
                                                ifelse(seats_odb < 1000, "500-999", ">=1000")))),
                           levels = c("<50", "50-99", "100-499", "500-999", ">=1000")))


x_op <- d_csm_op %>% 
  group_by(csm_size, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()


p_csm_op <- ggplot(x_op, 
                   aes(x = fw_month,
                       y = n,
                       ymax = 0,
                       shape = csm_size,
                       linetype = csm_size,
                       colour = csm_size)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_continuous(label = comma, limits = c(0, 50)) +
  geom_text(data = filter(x_op, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  labs(title = "Number of On-Prem Customers, Grouped by Total Customer Size",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_csm_op

ggsave("~/flywheel/exhibits/p_csm_op.png", p_csm_op, width = w, height = h, units = "mm")

p_csm_size_op <- ggplot(x_op, 
                        aes(x = fw_month,
                            y = arr,
                            ymax = 0,
                            shape = csm_size,
                            linetype = csm_size,
                            colour = csm_size)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4", "black")) +
  scale_y_log10(labels = NULL) +
  geom_text(data = filter(x_op, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd2(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = paste0("N=", n)), size = 3, vjust = -1.5) +
  labs(title = "On-Prem Total ARR Under Contract by Customer Size",
       x = "Date", 
       y = "ARR (M)") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_csm_size_op

ggsave("~/flywheel/exhibits/p_csm_size_op.png", p_csm_size_op, width = w, height = h, units = "mm")

d_segment_op <- d_fw_bind_op %>% 
  group_by(account_segment, odbid, fw_month) %>% 
  summarize(arr_odb = sum(arr_odb),
            seats_odb = sum(seats_odb)) %>% 
  data.frame()

x_segment_op <- d_segment_op %>% 
  group_by(account_segment, fw_month) %>% 
  summarize(n = n_distinct(odbid),
            seats = sum(seats_odb),
            arr = sum(arr_odb)/1000000) %>%
  filter(fw_month > fw_start_date)
data.frame()

p_segment_cust_op <- ggplot(x_segment_op, 
                            aes(x = fw_month,
                                y = n,
                                ymax = 0,
                                shape = account_segment,
                                linetype = account_segment,
                                colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  scale_y_continuous(label = comma, limits = c(0, 50)) +
  geom_text(data = filter(x_segment_op, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = n), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Number of On-Prem Customers, Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_cust_op

ggsave("~/flywheel/exhibits/segment_customers_op.png", p_segment_cust_op, width = w, height = h, units = "mm")


p_segment_seats_op <- ggplot(x_segment_op, 
                             aes(x = fw_month,
                                 y = seats/1000,
                                 ymax = 0,
                                 shape = account_segment,
                                 linetype = account_segment,
                                 colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) + 
  scale_y_continuous(label = comma, limits = c(0, 30)) +
  geom_text(data = filter(x_segment_op, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd0(seats/1000)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Number of Seats ('000) On-Prem Customers, Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_seats_op

ggsave("~/flywheel/exhibits/segment_seats_op.png", p_segment_seats_op, width = w, height = h, units = "mm")


p_segment_arr_op <- ggplot(x_segment_op, 
                           aes(x = fw_month,
                               y = arr,
                               ymax = 0,
                               shape = account_segment,
                               linetype = account_segment,
                               colour = account_segment)) +
  # label = paste0("$", asp, ", N=", n))) +
  geom_point(size = 3, alpha = I(2/3)) +
  geom_line(alpha = I(2/3)) +
  scale_shape_manual(values = 7:10) +
  scale_color_manual(values = c("darkgreen", "blue", "black", "red")) +
  scale_y_continuous(label = comma, limits = c(0, 5)) +
  geom_text(data = filter(x_segment_op, fw_month == min(fw_month) | fw_month == max(fw_month)),
            aes(label = rnd1(arr)), 
            size = 3, 
            vjust = -1, 
            check_overlap = TRUE) +
  ##geom_text(aes(label = n), size = 3, vjust = -1.5) +
  labs(title = "Total ARR (millions) On-Prem Customers, Grouped by Account Segment",
       x = "Date", 
       y = "Count") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top")

p_segment_arr_op

ggsave("~/flywheel/exhibits/segment_arr_op.png", p_segment_arr_op, width = w, height = h, units = "mm")


x_month_op <- d_fw_op %>%
  filter(fw_month >= fw_start_date) %>%
  group_by(fw_month) %>%
  summarize(arr = sum(arr_odb),
            paidseats = sum(seats_odb),
            customers = n_distinct(odbid),
            seats10 = sum(seats_odb >= 10),
            seats100 = sum(seats_odb >= 100),
            seats1000 = sum(seats_odb >= 1000)) %>%
  mutate(lagged_seats = lag(paidseats, lg),
         lagged_seats_delta = paidseats - lagged_seats,
         yoy_seats = rnd3((paidseats - lagged_seats) / lagged_seats),
         yoy_arr = rnd3((arr - lag(arr, lg)) / lag(arr, lg)),
         yoy_customers = rnd3((customers - lag(customers, lg)) / lag(customers, lg))) %>%
  data.frame()
##WK edit changed fwid to odbid, date to fw_month, and in summarize arr to arr_odb and seats to seats_odb

#to export this
##write_csv(x_month_op, 
##          paste0("~/flywheel/x_month_op", cur_dt, ".csv"))


##  using tidyr to re-organize data for improved charting
x_gath_op <- x_month_op %>%
  mutate(arr_m = rnd2(arr / 10^6),
         seats_k = rnd2(paidseats / 10^3)) %>%
  filter(!is.na(yoy_seats)) %>%
  gather(key = measure, value = y_val, -fw_month)

#to export this
##write_csv(x_gath_op, 
##          paste0("~/flywheel/x_gath_op", cur_dt, ".csv"))



##  things that recur to clean-up: 
fw_mx_dt <- max(d_fw$fw_month)
##WK edit - changed date to fw_month

year_seq <- seq(from = fw_start_date, ##need to manually update this each new FY
                to = fy0, 
                by = "year")

cum_fy <- year_seq %>%
  map_df(~ data_frame(x = .,
                      y = seq(from = .,
                              to = . + months(12),
                              by = "months")
  )
  )

cum_fy <- cum_fy[cum_fy$y <= fw_mx_dt,]

x_fy_op <- map2_df(cum_fy$x, cum_fy$y, ~ fx_calc_churn_NEW(d_fw_op, .x, .y, "fw_month"))


##  minor wrangling to get the fiscal year labels right and add month labels
##  note last year's last month is the first (month 0) of current
x_fy_op <- x_fy_op %>%
  filter(!is.na(end_dt)) %>%
  arrange(start_dt, end_dt) %>%
  mutate(fy = factor(year(start_dt + months(12)), 
                     levels = 2019:2014)) %>%
  group_by(fy) %>%
  mutate(month = row_number() - 1) %>%
  data.frame()


p_facet_op <- ggplot(x_gath_op %>% 
                       filter(measure %in% c("arr_m", "seats_k", "customers")) %>% 
                       mutate(measure = my_lvls(measure, c("customers", "arr_m", "seats_k"))), 
                     aes(x = fw_month, 
                         y = y_val,
                         ymax = y_val * 1.05,
                         label = rnd0(y_val))) + 
  geom_line(colour = col1) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  geom_point(size = sz,
             colour = col1,
             alpha = I(3/4)) + 
  geom_text(size = 3, vjust = -1, check_overlap = TRUE) +
  labs(title = "On-Prem (Perpetual & Subscription) Customers, ARR, and Seats",
       x = "Month",
       y = "Aggregate",
       shape = "Measure",
       fill = "Measure",
       colour = "Measure",
       linetype = "Measure") +
  facet_grid(measure ~ ., scales = "free") +
  mytheme

p_facet_op

ggsave("~/flywheel/exhibits/p_facet_op.png", p_facet_op, width = w, height = h, units = "mm")


p_churn_arr_fym_op <- fx_plot_fy(df = x_fy_op, 
                                 y_val = "churn_arr", 
                                 y_lab = "Cumulative ARR Churn (%)",
                                 t_lab = "Retaining:  On-Prem Cumulative CA Fiscal Year ARR Churn",
                                 label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_fym_op

ggsave("~/flywheel/exhibits/p_churn_arr_fyms_op.png", p_churn_arr_fym_op, width = w, height = h, units = "mm")


x_12m_date_size_op <- map_df(month_seq, ~ fx_calc_churn_NEW(d_fw_op, .x, .x + months(12), "fw_month", "size_cat"))

x_12m_date_size_op <- x_12m_date_size_op %>%
  filter(end_dt < cur_close_month, end_dt >= fw_start_date, is.na(size_cat) == FALSE)


p_churn_arr_size_op <- fx_plot_monthly(df = x_12m_date_size_op, 
                                       y_val = "churn_arr", 
                                       z_val = "size_cat",
                                       x_lab = "Date",
                                       y_lab = "Churn (%)",
                                       t_lab = "Retaining:  On-Prem 12 Month ARR Churn by Size Segment",
                                       label_format = "percent") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-01")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2016-10-01")), linetype = 4, colour = "blue") +
  scale_y_continuous(labels = percent) +
  mytheme

p_churn_arr_size_op

ggsave("~/flywheel/exhibits/p_churn_arr_size_op.png", p_churn_arr_size_op, width = w, height = h, units = "mm")


p_net_arr_fym_op <- fx_plot_fy(df = x_fy_op, 
                               y_val = "net_renew_arr", 
                               y_lab = "Cumulative ARR New Renew Rate (%)",
                               t_lab = "Expanding:  On-Prem Cumulative ARR Net Renew Rate",
                               label_format = "percent") +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(labels = percent) +
  mytheme

p_net_arr_fym_op

ggsave("~/flywheel/exhibits/p_net_arr_fym_op.png", p_net_arr_fym_op, width = w, height = h, units = "mm")


p_lost_arr_fym_op <- fx_plot_fy(df = x_fy_op, 
                                y_val = "agg_arr_lost", 
                                y_lab = "Cumulative ARR Lost (millions)",
                                t_lab = "Retaining:  Cumulative ARR Lost",
                                label_format = "dollar") +
  scale_y_continuous(labels = dollar) +
  mytheme

p_lost_arr_fym_op

ggsave("~/flywheel/exhibits/p_lost_arr_fym_op.png", p_lost_arr_fym_op, width = w, height = h, units = "mm")


##customers lost in aggregate
p_lost_cust_fym_op <- fx_plot_fy(df = x_fy_op, 
                                 y_val = "cust_term", 
                                 y_lab = "Cumulative Customers Lost",
                                 t_lab = "Retaining:  Cumulative Customers Lost",
                                 label_format = "seats") +
  ##scale_y_continuous(labels = cscale) +
  mytheme

p_lost_cust_fym_op

ggsave("~/flywheel/exhibits/p_lost_cust_fym_op.png", p_lost_cust_fym_op, width = w, height = h, units = "mm")

p_gross_add_arr_fym_op <- fx_plot_fy(df = x_fy_op, 
                                     y_val = "p_arr_xpand", 
                                     y_lab = "Cumulative ARR Expansion Rate (%)",
                                     t_lab = "Expanding:  Cumulative ARR Gross Expansion Rate",
                                     label_format = "percent") +
  scale_y_continuous(labels = percent) +
  mytheme

p_gross_add_arr_fym_op

ggsave("~/flywheel/exhibits/p_gross_add_arr_fym_op.png", p_gross_add_arr_fym_op, width = w, height = h, units = "mm")

## expanding customer count
p_gross_add_cust_fym_op <- fx_plot_fy(df = x_fy_op, 
                                      y_val = "cust_xpand", 
                                      y_lab = "Cumulative Number of Customers Expanding",
                                      t_lab = "Expanding:  Cumulative Number of Customers Expanding",
                                      label_format = "seats") +
  ##scale_y_continuous(labels = cscale) +
  mytheme

p_gross_add_cust_fym_op

ggsave("~/flywheel/exhibits/p_gross_add_cust_fym_op.png", p_gross_add_cust_fym_op, width = w, height = h, units = "mm")


## expanding customer count - percentage - cumulative
p_gross_add_cust_perc_fym_op <- fx_plot_fy(df = x_fy_op, 
                                           y_val = "p_cust_xpand", 
                                           y_lab = "Cumulative Percentage of Customers Expanding",
                                           t_lab = "Expanding:  Cumulative Percentage of Customers Expanding",
                                           label_format = "percent") +
  scale_y_continuous(labels = percent) +
  mytheme

p_gross_add_cust_perc_fym_op

ggsave("~/flywheel/exhibits/p_gross_add_cust_perc_fym_op.png", p_gross_add_cust_perc_fym_op, width = w, height = h, units = "mm")


p_landing_customers_fym_op <- fx_plot_fy(df = x_fy_op, 
                                         y_val = "cust_land",
                                         y_lab = "Customers",
                                         t_lab = "Landing:  Cumulative New Customers",
                                         label_format = "seats") +
  mytheme

p_landing_customers_fym_op

ggsave("~/flywheel/exhibits/p_landing_customers_fym_op.png", p_landing_customers_fym_op, width = w, height = h, units = "mm")


##  landing arr from customers new to the cohort
p_landing_arr_fym_op <- fx_plot_fy(df = x_fy_op, 
                                   y_val = "agg_arr_land",
                                   y_lab = "ARR (millions)",
                                   t_lab = "Landing:  Cumulative New Customer ARR",
                                   label_format = "dollar") +
  scale_y_continuous(label = dollar) +
  mytheme

p_landing_arr_fym_op

ggsave("~/flywheel/exhibits/p_landing_arr_fym_op.png", p_landing_arr_fym_op, width = w, height = h, units = "mm")
