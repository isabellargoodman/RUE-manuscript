#Script for calculating GDD 
#calculate from januaray 1 to date of sample and using a temperature base of 5 degree c.
Tbase<-5
TXsample<-197
NMsample<-191
SCOLOsample<-188
NCOLOsample<-183
WYsample<-181
MTsample<-177
SASKsample<-169

#Growing degree days for each site 
GDD_tx_rue <- TXclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0), 
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= TXsample) %>%                     
  group_by (Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_nm_rue <- NMclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0), 
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= NMsample) %>%                     
  group_by (Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_scolo_rue <- SCOLOclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0), 
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= SCOLOsample) %>%                    
  group_by(Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_ncolo_rue <- NCOLOclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0), 
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= NCOLOsample) %>%                    
  group_by (Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_wy_rue <- WYclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0), 
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= WYsample) %>%                   
  group_by (Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_mt_rue <- MTclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0),
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= MTsample) %>%                  
  group_by (Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>% 
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()
    

GDD_sask_rue <- SASKclimateRUE %>%
  mutate(
    GDD = pmax(Tmean - Tbase, 0),
    CDD = pmax(0, Tbase - Tmean) 
  ) %>%
  dplyr::filter(DOY <= SASKsample) %>%                    
  group_by(Year,Site) %>%
  mutate(GDD= sum(GDD, na.rm = TRUE),
         CDD= sum(CDD, na.rm = TRUE))%>%  
  ungroup()%>%
  dplyr::select(Site,GDD,CDD)%>%
  unique()

GDD_RUE<-rbind(GDD_mt_rue,GDD_ncolo_rue,GDD_nm_rue,GDD_sask_rue,GDD_scolo_rue,GDD_tx_rue,GDD_wy_rue)


#----WDD days----
#all soil water potential data (SWP) is from soilwat and available in downloads folder. 
SWP_TX<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_TX_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(TXclimateRUE, by=c("DOY","Site","Year"))

WDD_TX_rue <- SWP_TX %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= TXsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE))

SWP_NM<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_NM_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(NMclimateRUE, by=c("DOY","Site","Year"))

WDD_NM_rue <- SWP_NM %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= NMsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE)) 

SWP_SCOLO<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_SCOLO_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(SCOLOclimateRUE, by=c("DOY","Site","Year"))

WDD_SCOLO_rue <- SWP_SCOLO %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= SCOLOsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE)) 

SWP_NCOLO<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_NCOLO_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(NCOLOclimateRUE, by=c("DOY","Site","Year"))

WDD_NCOLO_rue <- SWP_NCOLO %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= NCOLOsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE)) 
  mutate(site="NCOLO")

SWP_WY<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_WY_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(WYclimateRUE, by=c("DOY","Site","Year"))

WDD_WY_rue <- SWP_WY %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= WYsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE))


SWP_MT<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_MT_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(MTclimateRUE, by=c("DOY","Site","Year"))

WDD_MT_rue <- SWP_MT %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= MTsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE)) 

SWP_SASK<-read.csv("~/Desktop/Thesis stuff/RUE/soilwatdata/swp/swp_SASK_RUE.csv")%>%
  mutate(across(3:10, ~ - .x*.1))%>%
  rename(DOY="Day",
         Site="site")%>%
  drop_na()%>%
  left_join(SASKclimateRUE, by=c("DOY","Site","Year"))

WDD_SASK_rue <- SWP_SASK %>%
  mutate(
    soil_wet = if_any(3:10, ~ .x > -1.5),       # TRUE if any layer is wet
    WDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY <= SASKsample) %>%
  group_by(Year,Site) %>%
  summarise(WDDsum = sum(WDD, na.rm = TRUE))

#merge all
WDD_data_rue<-rbind(WDD_MT_rue,WDD_NCOLO_rue,WDD_NM_rue,WDD_SASK_rue,WDD_SCOLO_rue,WDD_WY_rue,WDD_TX_rue)


#----dry degree days for each site
DDD_TX_rue <- SWP_TX %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= TXsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_NM_rue <- SWP_NM %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= NMsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_SCOLO_rue <- SWP_SCOLO %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= SCOLOsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_NCOLO_rue <- SWP_NCOLO %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= NCOLOsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_WY_rue <- SWP_WY %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= WYsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_MT_rue <- SWP_MT %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= MTsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_SASK_rue <- SWP_SASK %>%
  mutate(
    soil_wet = if_all(3:10, ~ .x < -1.5),       # TRUE if any layer is wet
    DDD = ifelse(Tmean > Tbase & soil_wet,      # only count if temp > Tbase AND soil wet
                 Tmean - Tbase,
                 0)
  ) %>%
  dplyr::filter(DOY >= SASKsample) %>%
  group_by(Year,Site) %>%
  summarise(DDDsum = sum(DDD, na.rm = TRUE)) 

DDD_rue<-rbind(DDD_MT_rue,DDD_NCOLO_rue,DDD_NM_rue,DDD_SASK_rue,DDD_SCOLO_rue,DDD_TX_rue,DDD_WY_rue)

#----climate-----
RUEpptmeans<-RUEclimate%>%
  dplyr::filter(Year>=2025)%>%
  mutate(PPT_mm = PPT_cm*10)%>%
  group_by(Site)%>%
  summarise_at(vars(PPT_mm),
               list(sum))

RUEtempmeans<-RUEclimate%>%
  dplyr::filter(Year>=2025)%>%
  group_by(Site)%>%
  summarise_at(vars(Tmean),
               list(mean))



#----aridity index -----
#calculated using pet from soilwat, in downloads folder
sites <- c("SASK", "MT", "WY", "NCOLO", "SCOLO", "NM", "TX")
PET_2025 <- bind_rows(lapply(sites, function(s) {
  read.csv(paste0("~/Desktop/Thesis stuff/RUE/soilwatdata/pet/pet_", s, "_RUE.csv")) %>%
    mutate(Site = s)
}))

PET_2025<-PET_2025%>%
  dplyr::select(Year, Day, pet_cm,Site)%>%
  mutate(pet_mm=pet_cm*10)%>%
  group_by(Site)%>%
  summarise_at(vars(pet_mm),
               list(sum))

PET_2025<-PET_2025%>%
  left_join(RUEpptmeans,by="Site")

Aridity_index2025<-PET_2025%>%
  mutate(AI_2025 = PPT_mm/pet_mm)%>%
  dplyr::select(AI_2025,Site)

#calcualte longterm AI
sites <- c("SASK", "MT", "WY", "NCOLO", "SCOLO", "NM", "TX")
PET_all <- bind_rows(lapply(sites, function(s) {
  read.csv(paste0("~/Desktop/Thesis stuff/RUE/soilwatdata/pet/pet_", s, "_RUE2.csv")) %>%
    mutate(Site = s)
}))

PET_all<-PET_all%>%
  dplyr::select(year, DOY, pet_cm,Site)%>%
  mutate(pet_mm=pet_cm*10)%>%
  group_by(Site,year)%>%
  summarise_at(vars(pet_mm),
               list(sum))
PET_all<-PET_all%>%
  group_by(Site)%>%
  summarise_at(vars(pet_mm),
               list(mean))

PET_all<-PET_all%>%
  left_join(mean_climate,by="Site")

Aridity_indexAll<-PET_all%>%
  mutate(AI_all = MAP/pet_mm)%>%
  dplyr::select(AI_all,Site)

Aridity_index<-Aridity_index2025%>%
  left_join(Aridity_indexAll,by="Site")

#----explantories----
#merge all explantory variables together
explantories_rue<-left_join(GDD_RUE, RUE_means, by=c("Site"))%>%
  left_join(aet_allsite_sum_2)%>%
  left_join(transp_allsite_sum_2)%>%
  left_join(WDD_data_rue)%>%
  left_join(DDD_rue)%>%
  left_join(RUEtempmeans)%>%
  left_join(RUEpptmeans)%>%
  left_join(field_phot)%>%
  left_join(Aridity_index)%>%
  left_join(mean_climate)


#----figures----- 
#vizualize figures, use y=rue when looking at RUE relationships, and y=total when looking at ANPP relationships 

# c3figure<-ggplot(data=explantories_rue, mapping=aes(x=C3percent,y=rue,color=Site))+
#   geom_point(size=3)+
#   geom_smooth(explantories_rue,mapping = aes(x=C3percent,y=rue), method="lm", se=F,linewidth=1, color="black")+
#   stat_cor(aes(x = C3percent, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)",x="C3 Percent")+
#   theme_classic()

Gddfigure<-ggplot(data=explantories_rue, mapping=aes(x=GDD,y=total))+
  geom_point(size=5)+
  scale_y_continuous(limits=c(50,140))+
  geom_smooth(explantories_rue,mapping = aes(x=GDD,y=total), method="lm", se=F,linewidth=1.5, color="darkred")+ 
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,  
    point.padding = 0.5,   
    force = 2, max.overlaps = Inf)+
  stat_cor(aes(label = ..rr.label..),size=5)+
 # stat_cor()+
  labs(y=expression("Aboveground Net Primary Production (g/m"^"2"*")"), x="Growing Degree Days")+
  theme_classic(base_size = 20)

# CDDfigure<-ggplot(data=explantories_rue, mapping=aes(x=CDD,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=CDD,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = CDD, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   stat_cor()+
#   labs(y="Radiation Use Efficiency (g/Mj)",x="Chilling Degree Days")+
#   theme_classic()

# WDDfigure<-ggplot(data=explantories_rue, mapping=aes(x=WDDsum,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=WDDsum,y=rue), method='lm', se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = WDDsum, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   stat_cor()+
#   labs(y="Radiation Use Efficiency (g/Mj)",x="Wet Degree Days")+
#   theme_classic()

# DDDfigure<-ggplot(data=explantories_rue, mapping=aes(x=DDDsum,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=DDDsum,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = DDDsum, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   stat_cor()+
#   labs(y="Radiation Use Efficiency (g/Mj)",x="Dry Degree Days")+
#   theme_classic()

tfigure<-ggplot(data=explantories_rue, mapping=aes(x=Tmean,y=total))+
  geom_point(size=5)+
  scale_y_continuous(limits=c(50,140))+
  geom_smooth(explantories_rue,mapping = aes(x=Tmean,y=total), method="lm", se=F,linewidth=1.5, color="darkred")+ 
  # stat_cor(aes(x = Tmean, y = total, group = 1),
  #          method = "pearson")+
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,  
    point.padding = 0.5,   
    force = 2, max.overlaps = Inf)+
  stat_cor(aes(label = ..rr.label..),size=5)+
  #scale_color_manual(values=site_colors_grassland_hc)+
  labs(y=expression("Aboveground Net Primary Production (g/m"^"2"*")"), x="2025 Mean Temperature (°C)")+
  theme_classic(base_size = 20)

MATfigure<-ggplot(data=explantories_rue, mapping=aes(x=MAT,y=total))+
  geom_point(size=5)+
  #scale_y_continuous(limits=c(50,140))+
  geom_smooth(explantories_rue,mapping = aes(x=MAT,y=total), method="lm", se=F,linewidth=1, color="darkred")+
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,
    point.padding = 0.5,
    force = 2)+
  stat_cor(aes(label = ..rr.label..),size=5)+
  labs(y=expression("Aboveground Net Primary Production (g/m"^"2"*")"), x="Mean Annual Temperature(°C)")+
  theme_classic(base_size = 20)

library(ggrepel)  # optional but highly recommended

Pptfigure <- ggplot(data = explantories_rue,mapping = aes(x = PPT_mm, y = rue)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "darkred") +
 #stat_cor(aes(x = PPT_mm, y = rue, group = 1), method = "pearson") +
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,
    point.padding = 0.5,
    force = 2)+
  
  stat_cor(aes(label = ..rr.label..),size=5)+
  labs(y=expression("Radiation Use Efficiency (g/Mj)"), x="2025 Precipitation(mm)")+
  theme_classic(base_size = 18)

MAPfigure<-ggplot(data=explantories_rue, mapping=aes(x=MAP,y=total))+
  geom_point(size=5)+
  scale_y_continuous(limits=c(50,140))+
  geom_smooth(explantories_rue,mapping = aes(x=MAP,y=total), method="lm", se=F,linewidth=1.5, color="darkred")+ 
  # stat_cor()
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,  
    point.padding = 0.5,   
    force = 2)+
  #scale_color_manual(values=site_colors_grassland_hc)+
  stat_cor(aes(label = ..rr.label..),size=5)+
  labs(y=expression("Aboveground Net Primary Production (g/m"^"2"*")"), x="Mean Annual Precipitation(mm)")+
  theme_classic(base_size = 20)

# evapfigure<-ggplot(data=explantories_rue, mapping=aes(x=evapotr_cm,y=rue,color=Site))+
#   geom_point(size=3)+
#  # scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=evapotr_cm,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = evapotr_cm, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   #stat_cor()+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="2025 Evapotranspiration (cm)")+
#   theme_classic()
# 
# relativeaetFigure<-ggplot(data=explantories_rue, mapping=aes(x=relative_aet,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   stat_cor(aes(x = relative_aet, y = rue, group = 1),  
#            method = "pearson")+
#   geom_smooth(explantories_rue,mapping = aes(x=relative_aet,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="Relative AET")+
#   theme_classic()
# 
# transpirationfigure<-ggplot(data=explantories_rue, mapping=aes(x=transp,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=transp,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = transp, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="2025 Transpiration")+
# theme_classic()

# transpirationRElfigure<-ggplot(data=explantories_rue, mapping=aes(x=relative_transp,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=relative_transp,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = relative_transp, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="Relative Transpiration")+
#   theme_classic()
# 
# longtermTransfigure<-ggplot(data=explantories_rue, mapping=aes(x=avg_transp,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=avg_transp,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = avg_transp, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="Mean Annual Transpiration")+
#   theme_classic()
# 

AIfigure_2025<-ggplot(data=explantories_rue, mapping=aes(x=AI_2025,y=rue))+
  geom_point(size=5)+
  #scale_y_continuous(limits=c(50,140))+
  geom_smooth(explantories_rue,mapping = aes(x=AI_2025,y=rue), method="lm", se=F,linewidth=1.5, color="darkred")+
  #stat_cor(aes(x = AI_2025, y = rue, group = 1),
     #      method = "pearson")+
  geom_label_repel(
    aes(label = Site),
    size = 5,
    box.padding = 0.6,
    point.padding = 0.5,
    force = 2)+
  stat_cor(aes(label = ..rr.label..),size=5)+
  scale_color_manual(values=site_colors_grassland_hc)+
  labs(y="Radiation Use Efficiency (g/Mj)", x="2025 Aridity Index")+
  theme_classic(base_size =18)

#Create figure 5
jpeg("two RUE explantories.jpeg",
     width = 10,
     height =5,
     units = "in",
     res = 300)
explanatory_figures<-ggarrange(Pptfigure, AIfigure_2025+rremove("ylab"),  common.legend = T, 
                               hjust= -.9,font.label = list(size = 18), labels=c("a)","b)"))

explanatory_figures
dev.off() 

# AIfigure<-ggplot(data=explantories_rue, mapping=aes(x=AI_all,y=rue,color=Site))+
#   geom_point(size=3)+
#   #scale_y_continuous(limits=c(50,140))+
#   geom_smooth(explantories_rue,mapping = aes(x=AI_all,y=rue), method="lm", se=F,linewidth=1, color="black")+ 
#   stat_cor(aes(x = AI_all, y = rue, group = 1),  
#            method = "pearson")+
#   scale_color_manual(values=site_colors_grassland_hc)+
#   labs(y="Radiation Use Efficiency (g/Mj)", x="Mean Annual Aridity Index")+
#   theme_classic()

library(lme4)
model <- lmer(rue ~ AI + (1 | Site), data = explantories_rue)

#create figure 6 
jpeg("four ANPP explantories.jpeg",
     width = 12,
     height =14,
     units = "in",
     res = 300)
explanatory_figures<-ggarrange(MAPfigure+rremove("ylab"), MATfigure+rremove("ylab"), tfigure+rremove("ylab"),Gddfigure+rremove("ylab"),  common.legend = T,ncol=2, nrow=2, 
                               hjust= -.9,font.label = list(size = 18), labels=c("a)","b)","c)","d)"))


explanatory_figures <- annotate_figure(
  explanatory_figures,
  left = text_grob(
    "Aboveground Net Primary Production (g/m²)",
    size=20,
    rot=90
  )
)
explanatory_figures
dev.off() 

cor(explantories_rue$total,explantories_rue$Tmean)

 

#all models and statistics 
#----models-----

GDDmodel<-lm(rue~GDD, data=explantories_rue)
CDDmodel<-lm(rue~CDD, data=explantories_rue)
WDDmmodel<-lm(rue~WDDsum, data=explantories_rue)
DDDmodel<-lm(rue~DDDsum, data=explantories_rue)
Tmodel<-lm(rue~Tmean, data=explantories_rue)
PPTmodel<-lm(rue~PPT_mm, data=explantories_rue)
evapmodel<-lm(rue~evapotr_cm, data=explantories_rue)
rel_evapmodel<-lm(rue~relative_transp, data=explantories_rue)
rel_transpmodel<-lm(rue~relative_aet, data=explantories_rue)
transpmodel<-lm(rue~avg_transp, data=explantories_rue)
ai2025model<-lm(rue~AI_2025, data=explantories_rue)
C3model<-lm(rue~C3percent,data=explantories_rue)
MAPmodel<-lm(rue~MAP,data=explantories_rue)
MATmodel<-lm(rue~MAT,data=explantories_rue)
mean_transpmodel<-lm(rue~transp,data=explantories_rue)
mean_AIpmodel<-lm(rue~AI_all,data=explantories_rue)


summary(GDDmodel) # r2 = 0.006  #pval = .86 tbase 7.5 pval =  .90  
summary(CDDmodel) # r2 = 0.12 #pval = .44 tbase 7.5 pval =  .49
summary(WDDmmodel) # r2 = <0.001  #pval = .98 tbase 7.5 pval =  .98 
summary(DDDmodel) ## r2 = .17 #pval = 0.34 tbase 7.5 pval =  .34
summary(Tmodel) # r2 = 0.039 #pval = .67
summary(PPTmodel) # r2 = 0.37  #pval = .14### 
summary(evapmodel)# r2 = 0.15 #pval = .39 
summary(rel_evapmodel) # r2 = 0.10 #pval= .47
summary(rel_transpmodel) # r2 = 0.00282 #val = .9
summary(transpmodel)# r2 = 0.070  # .56 
summary(ai2025model)# r2 = .31 #.19 *** 
summary(C3model) # r2 = .11 #
summary(MAPmodel)# r2 = 0.03
summary(MATmodel) #r2 = 0.075 
summary(mean_transpmodel) # r2 =0.41  #
summary(mean_AIpmodel)  #

mlr<-lm(rue~AI+PPT_mm, data=explantories_rue)
summary(mlr)
explantories_rue_anpp<-explantories_rue%>%
  left_join(field_totals,by="Site")

GDDmodel_anpp<-lm(total~GDD, data=explantories_rue)
summary(GDDmodel_anpp) # pval - 0.03

CDDmodel_anpp<-lm(total~CDD, data=explantories_rue)
summary(CDDmodel_anpp) # pval - 0.07

WDDmmodel_anpp<-lm(total~WDDsum, data=explantories_rue)
summary(WDDmmodel_anpp) # pval - 0.09

DDDmodel_anpp<-lm(total~DDDsum, data=explantories_rue)
summary(DDDmodel_anpp) # pval - 0.83 

Tmodel_anpp<-lm(total~Tmean, data=explantories_rue)
summary(Tmodel_anpp) # pval - 0.03

MATmodel_anpp<-lm(total~MAT, data=explantories_rue)
summary(MATmodel_anpp) 

PPTmodel_anpp<-lm(total~PPT_mm, data=explantories_rue)
summary(PPTmodel_anpp) # pval - 0.97 

evapmodel_anpp<-lm(total~evapotr_cm, data=explantories_rue)
summary(evapmodel_anpp) # pval - 0.80 

rel_evapmodel_anpp<-lm(total~relative_transp, data=explantories_rue)
summary(rel_evapmodel_anpp) # pval - 0.42 

rel_transpmodel_anpp<-lm(total~relative_aet, data=explantories_rue)
summary(rel_transpmodel_anpp) # pval - 0.90

transpmodel_anpp<-lm(total~avg_transp, data=explantories_rue)
summary(transpmodel_anpp) # pval - 0.06 

aimodel_anpp<-lm(total~AI, data=explantories_rue)
summary(aimodel_anpp) # pval - 0.50

matmodel<-lm(total~MAT, data=explantories_rue)
summary(matmodel) # pval - 0.50


mlr2<-lm(total~Tmean+GDD, data=explantories_rue)
summary(mlr2)

explantory_sub <- explantories_rue %>%
  dplyr::select(transp, avg_transp, Tmean, PPT_mm, MAT, MAP, AI_2025, AI_all,total,Site) %>%
  dplyr::rename(
    TRANSP_2025 = transp,
    TRANSP_long = avg_transp,
    TEMP_2025   = Tmean,
    TEMP_long   = MAT,
    PPT_2025    = PPT_mm,
    PPT_long    = MAP,
    AI_2025     = AI_2025,
    AI_long     = AI_all
  )
library(tidyr)

explantory_long <- explantory_sub %>%
  pivot_longer(
    cols = -c(total,Site),
    names_to = c("variable", "timescale"),
    names_sep = "_",
    values_to = "value"
  )      


lm<-lm(total~PPT_mm+MAP, data=explantories_rue)
summary(lm)
