library(tidyverse)

#read in previously downloaded AET data for each site. 
aet_TX_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_TX_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="TX")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_TX_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_TX_RUE.csv")%>%
  rbind(aet_TX_RUE2)%>%
  dplyr::filter(DOY <= 197)

aet_NM_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_NM_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="NM")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_NM_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_NM_RUE.csv")%>%
  rbind(aet_NM_RUE2)%>%
  dplyr::filter(DOY <= 192)

aet_SCOLO_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_SCOLO_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="SCOLO")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_SCOLO_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_SCOLO_RUE.csv")%>%
  rbind(aet_SCOLO_RUE2)%>%
  dplyr::filter(DOY <= 189)

aet_NCOLO_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_NCOLO_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="NCOLO")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_NCOLO_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_NCOLO_RUE.csv")%>%
  rbind(aet_NCOLO_RUE2)%>%
  dplyr::filter(DOY <= 184)

aet_WY_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_WY_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="WY")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_WY_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_WY_RUE.csv")%>%
  rbind(aet_WY_RUE2)%>%
  dplyr::filter(DOY <= 181)

aet_MT_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_MT_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="MT")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
aet_MT_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_MT_RUE.csv")%>%
  rbind(aet_MT_RUE2)%>%
  dplyr::filter(DOY <= 177)

aet_SASK_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_SASK_RUE2.csv")%>%
  dplyr::select(-X)%>%
  mutate(site="SASK")%>%
  rename(Site="site")
aet_SASK_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/aet/aet_SASK_RUE.csv")%>%
  rbind(aet_SASK_RUE2)%>%
  dplyr::filter(DOY <= 170)

aet_allsite<-rbind(aet_TX_RUE,aet_NM_RUE,aet_SCOLO_RUE,aet_NCOLO_RUE,aet_WY_RUE,aet_MT_RUE,aet_SASK_RUE)

#remove necessary variables
rm(aet_TX_RUE,aet_NM_RUE,aet_SCOLO_RUE,aet_NCOLO_RUE,aet_WY_RUE,aet_MT_RUE,aet_SASK_RUE)
rm(aet_TX_RUE2,aet_NM_RUE2,aet_SCOLO_RUE2,aet_NCOLO_RUE2,aet_WY_RUE2,aet_MT_RUE2,aet_SASK_RUE2)

#creat full AET variables 
aet_allsite_sum<-aet_allsite%>%
  group_by(Site,year)%>%
  summarise_at(vars(evapotr_cm),
               list(sum))

aet_allsite_sum_2<-aet_allsite_sum%>%
  group_by(Site)%>%
  mutate(avg_aet=mean(evapotr_cm))%>%
  dplyr::filter(year>=2025)%>%
  mutate(relative_aet=evapotr_cm/avg_aet)


#----Transpration-----
#load in previously downloaded (modedl from soilway) data for each site. 
transp_TX_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_TX_RUE2.csv")%>%
  mutate(site="TX")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_TX_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/tranp_TX_RUE.csv")%>%
  mutate(Site="TX")%>%
  rbind(transp_TX_RUE2)%>%
  dplyr::filter(DOY <= 197)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_TX_RUE2)

transp_NM_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_NM_RUE2.csv")%>%
  mutate(site="NM")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_NM_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_NM_RUE.csv")%>%
  mutate(Site="NM")%>%
  rbind(transp_NM_RUE2)%>%
  dplyr::filter(DOY <= 192)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_NM_RUE2)

transp_SCOLO_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_SCOLO_RUE2.csv")%>%
  mutate(site="SCOLO")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_SCOLO_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_SCOLO_RUE.csv")%>%
  mutate(Site="SCOLO")%>%
  rbind(transp_SCOLO_RUE2)%>%
  dplyr::filter(DOY <= 189)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_SCOLO_RUE2)

transp_NCOLO_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_NCOLO_RUE2.csv")%>%
  mutate(site="NCOLO")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_NCOLO_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_NCOLO_RUE.csv")%>%
  mutate(Site="NCOLO")%>%
  rbind(transp_NCOLO_RUE2)%>%
  dplyr::filter(DOY <= 184)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_NCOLO_RUE2)

transp_WY_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_WY_RUE2.csv")%>%
  mutate(site="WY")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_WY_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_WY_RUE.csv")%>%
  mutate(Site="WY")%>%
  rbind(transp_WY_RUE2)%>%
  dplyr::filter(DOY <= 181)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_WY_RUE2)

transp_MT_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_MT_RUE2.csv")%>%
  mutate(site="MT")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_MT_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_MT_RUE.csv")%>%
  mutate(Site="MT")%>%
  rbind(transp_MT_RUE2)%>%
  dplyr::filter(DOY <= 177)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_MT_RUE2)

transp_SASK_RUE2<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/TRANSP_SASK_RUE2.csv")%>%
  mutate(site="SASK")%>%
  dplyr::filter(year>=1995)%>%
  rename(Site="site")
transp_SASK_RUE<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/transp/transp_SASK_RUE.csv")%>%
  mutate(Site="SASK")%>%
  rbind(transp_SASK_RUE2)%>%
  dplyr::filter(DOY <= 170)%>%
  dplyr::select(-X)%>%
  group_by(year,DOY)%>%
  mutate(transp = rowSums(across(3:10), na.rm = TRUE))
rm(transp_SASK_RUE2)

#create transpiration variables
transp_allsite<-rbind(transp_TX_RUE,transp_NM_RUE,transp_SCOLO_RUE,transp_NCOLO_RUE,transp_WY_RUE,transp_MT_RUE,transp_SASK_RUE)%>%
  dplyr::select(year,DOY,Site,transp)

rm(transp_TX_RUE,transp_NM_RUE,transp_SCOLO_RUE,transp_NCOLO_RUE,transp_WY_RUE,transp_MT_RUE,transp_SASK_RUE)

transp_allsite_sum<-transp_allsite%>%
  group_by(Site,year)%>%
  summarise_at(vars(transp),
               list(sum))

transp_allsite_sum_2<-transp_allsite_sum%>%
  group_by(Site)%>%
  mutate(avg_transp=mean(transp))%>%
  dplyr::filter(year>=2025)%>%
  mutate(relative_transp=transp/avg_transp)







