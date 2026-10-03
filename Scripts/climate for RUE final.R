library(tidyverse)
setwd("~/Desktop/Thesis stuff/Thesis related CSVs")
prismclimate<-read.csv("prismclimate.csv")%>%
  dplyr::select(Name,Date,ppt..mm.,tmin..degrees.C.,tmax..degrees.C.)%>%
  rename(Site="Name",
         PPT_cm="ppt..mm.",
         Tmin_C="tmin..degrees.C.",
         Tmax_C="tmax..degrees.C."
  )%>%
  mutate(PPT_cm = PPT_cm*0.1) 

canadaclimate<-read.csv("canadaclimate.csv")%>%
  dplyr::select(date,ppt_mm,tmax_c,tmin_c)%>%
  mutate(ppt_mm = ppt_mm*0.1)%>%
  rename(PPT_cm ="ppt_mm",
         Tmin_C="tmin_c",
         Tmax_C ="tmax_c",
         Date="date")%>%
  mutate(Site='SASK')

RUEclimate<-rbind(prismclimate,canadaclimate)%>%
  mutate(Date = as.Date(Date))%>%
  mutate(DOY = yday(Date))%>%
  mutate(Year=year(Date))%>%
  dplyr::select(-Date)

#Dont run if making xdf 
RUEclimate<-RUEclimate%>%
 mutate(Tmean = ((Tmax_C+Tmin_C)/2))%>%
 dplyr::select(-Tmin_C, -Tmax_C)


TXclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "TX")
NMclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "NM")
SCOLOclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "SCOLO")
NCOLOclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "NCOLO")
WYclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "WY")
MTclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "MT")
SASKclimateRUE<-RUEclimate%>%
  dplyr::filter(Site == "SASK")

#create xdf dataframe only used in SOILWAT2 
# xdf<-RUEclimate

# RUEclimate<-RUEclimate[,c("Year","DOY","Tmax_C","Tmin_C","PPT_cm","Site")]
# 
# xdf_tx<-RUEclimate%>%
#   dplyr::filter(Site=="TX")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_nm<-RUEclimate%>%
#   dplyr::filter(Site=="NM")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_scolo<-RUEclimate%>%
#   dplyr::filter(Site=="SCOLO")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_ncolo<-RUEclimate%>%
#   dplyr::filter(Site=="NCOLO")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_wy<-RUEclimate%>%
#   dplyr::filter(Site=="WY")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_mt<-RUEclimate%>%
#   dplyr::filter(Site=="MT")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_nm<-RUEclimate%>%
#   dplyr::filter(Site=="NM")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# xdf_sask<-RUEclimate%>%
#   dplyr::filter(Site=="SASK")%>%
#   dplyr::select(-Site)%>%
#   mutate(DOY = as.integer(DOY))
# 
# #add last day to saskatchewan
# Year<-c(2025)
# DOY<-c(365)
# Tmax_C<-(-1.38652649)
# Tmin_C<-(-5.2737335)
# PPT_cm<-(1.136065e-03)
# 
# df<-data.frame(Year,DOY,Tmax_C,Tmin_C,PPT_cm)
# xdf_sask<-xdf_sask<-rbind(xdf_sask,df)

#longterm climate 
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
setwd("~/Desktop/Thesis stuff/phenology/phenology climate data")
LongClimate<-read.csv("climatePheno.csv")%>%
  dplyr::select(Date,DAYMET_004_prcp,DAYMET_004_tmax,DAYMET_004_tmin,ID)%>%
  rename(PPT ="DAYMET_004_prcp",
         TMIN="DAYMET_004_tmin",
         TMAX="DAYMET_004_tmax")

LongClimate<- LongClimate%>%
  mutate(
    week=week(Date),
    DOY=yday(Date),
    year=year(Date),
    tmean=((TMIN+TMAX)/2)
  )%>%
  dplyr::select(-TMIN,-TMAX)%>%
  rename(Site="ID")

mean_climate<-LongClimate%>%
  group_by(Site,year)%>%
  mutate(MAP=sum(PPT))%>%
  mutate(MAT = mean(tmean))

mean_climate<-mean_climate%>%
  group_by(Site)%>%
  summarise_at(vars(MAP,MAT),
               list(mean))



  
