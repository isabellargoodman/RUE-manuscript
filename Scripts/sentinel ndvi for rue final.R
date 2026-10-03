library(tidyverse)
#load sentinel2 data for all sites 
library(signal)

setwd("~/Desktop/Thesis stuff/RUE")
sentinel<-read.csv("Sentinel2_NDVI_Allsites.csv")

#clean data 
sentinelNDVI<-sentinel%>%
  dplyr::select(DOY,NDVI,date,site)%>%
  mutate(year=year(date))%>%
  dplyr::filter(year>2024)%>%
  group_by(site,date)%>%
  mutate(NDVI= mean(NDVI))%>%
  unique()%>%
  drop_na()

sentinelNDVI <- sentinelNDVI %>%
  mutate(date = as.Date(date))

#------determine dates of greenup to determine start of season ------------
#interpolate NDVI
sentinelNDVI <- sentinelNDVI %>%
  group_by(year,site) %>%
  arrange(DOY) %>%
  group_modify(~ {
    day8_date <- seq(min(.x$date), max(.x$date), by = "day")
    day8_DOY <- yday(day8_date)  # Day of year
    data.frame(
      date = day8_date,
      week = week(day8_date),
      DOY = day8_DOY,
      NDVI = approx(.x$DOY, .x$NDVI, xout = day8_DOY)$y)
  }) %>%
  ungroup()

smooth_ndvi_site <- function(df,
                             ndvi_col = "NDVI",
                             doy_col  = "DOY",
                             year_col = "year",
                             site_col = "site",
                             p = 2,
                             window_size = 33) {
  
  if (window_size %% 2 == 0) {
    window_size <- window_size + 1
  }
  
  df %>%
    arrange(!!sym(site_col), !!sym(year_col), !!sym(doy_col)) %>%
    group_by(!!sym(site_col), !!sym(year_col)) %>%
    mutate(
      NDVI_smooth = if (n() >= window_size) {
        sgolayfilt(!!sym(ndvi_col), p = p, n = window_size)
      } else {
        NA_real_
      }
    ) %>%
    ungroup()
}

#smooth data
sentinelNDVI<- smooth_ndvi_site(sentinelNDVI, window_size=33)%>%
  drop_na()

sentinelNDVI <- sentinelNDVI%>%
  group_by(year,site) %>%
  mutate(
    NDVI_scaled = (NDVI - min(NDVI, na.rm = TRUE)) / 
      (max(NDVI, na.rm = TRUE) - min(NDVI, na.rm = TRUE)),
    NDVI_scaled_smooth= (NDVI_smooth - min(NDVI_smooth, na.rm = TRUE)) / 
      (max(NDVI_smooth, na.rm = TRUE) - min(NDVI_smooth, na.rm = TRUE))
  ) %>%
  ungroup()

#---------function for determing dates of phenology--------- 
ratio_pheno_function <- function(df) {
  df <- df %>% arrange(DOY)
  NDVI_max <- max(df$NDVI_scaled_smooth, na.rm = TRUE)
  NDVI_max_val <- max(df$NDVI_smooth, na.rm = TRUE)
  # Green-up (first >= 0.5)
  green_up <- df %>%
    dplyr::filter(NDVI_scaled_smooth >= 0.5) %>%
    slice(1) %>%
    pull(DOY)
  green_up <- ifelse(length(green_up) == 0, NA_real_, green_up)
  # Peak NDVI
  peak_green <- df %>%
    dplyr::filter(NDVI_scaled_smooth == NDVI_max) %>%
    slice(1) %>%
    pull(DOY)
  peak_green <- ifelse(length(peak_green) == 0, NA_real_, peak_green)
  # Brown-down (first < 0.5 after peak, else last DOY)
  brown_down <- {
    after_peak <- df$DOY > peak_green
    below_half <- df$NDVI_scaled_smooth[after_peak] < 0.5
    
    if (any(below_half, na.rm = TRUE)) {
      # first day after peak where it drops below 0.5
      df$DOY[after_peak][which(below_half)[1]]
    } else {
      # if it never drops below 0.5, take the day after peak with minimum NDVI_scaled_smooth
      df$DOY[after_peak][which.min(df$NDVI_scaled_smooth[after_peak])]
    }
  }
  
  # Seaon length
  length <- ifelse(!is.na(green_up) & !is.na(brown_down),
                   brown_down - green_up,
                   NA_real_)
  tibble(
    year = unique(df$year),
    green_up = green_up,
    brown_down = brown_down,
    peak_green = peak_green,
    length = length,
    peak_value = NDVI_max_val
  )
}

#calculate Start of season for each site (green-up)
sos_RUE <-sentinelNDVI%>%
  group_by(site)%>%
  group_modify(~ratio_pheno_function(.x))

#------calculate fpar from sentinel 
sentinelNDVI<-sentinelNDVI%>%
  mutate(fPAR=(0.89*NDVI)-0.05)


#rename site to match others 
sentinelNDVI<-sentinelNDVI%>%
  rename(Site="site")

#join sentinelNDVI data with field totals to create first rue datafram 
RUEdata1<-left_join(sentinelNDVI,field_totals, by=c("Site"))%>%
  rename(Biomass="total")%>%
  mutate(date<-as.Date(date))

#quadrat level data 
sentinelNDVI_quad<-sentinelNDVI%>%
  select(year,Site,date,week,DOY,fPAR)

#merge quadrat level
RUEdata1_quad<-left_join(sentinelNDVI_quad,field_quad_totals, by=c("Site"), relationship = "many-to-many")

  
  




  
  