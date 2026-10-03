library(tidyverse)
library(ggpubr)
library(ggrepel)

#load SSRD data from ERA5 daily 
setwd("~/Desktop/Thesis stuff/RUE")
SSRD<-read.csv("SSRD_all_sites.csv")

#correct units
SSRD<-SSRD%>%
  mutate(ssrd=mean*(10^-6))

#Multiple by 0.48 based on Tsubo and Walker
SSRD <- SSRD %>%
  rename(Site="site")%>%
  mutate(PARi = ssrd * 0.48)%>%
  dplyr::select(-ssrd)%>% 
  mutate(date = as.Date(date))

#merge with ruedata1 so biomass, PARi and fAPAR are all in one dataset for both total and quad level 
rueDATA <- left_join(SSRD,RUEdata1,by=c("date","Site"))%>%
  drop_na()

rueDATA_quad <- left_join(SSRD,RUEdata1_quad,by=c("date","Site"))%>%
  drop_na()

#Reorder sites
site_order <- c("SASK", "MT", "WY", "NCOLO","SCOLO","NM","TX")

#Calculate denominator of RUE
rueDATA_quad<-rueDATA_quad%>%
  mutate(apar=PARi*fPAR)

#Sum denominator of montieth model along growing season for all sites - growing season determined in "sentinel NDVI for rue - final.r"
rue_GS_TX<-rueDATA_quad%>%
  dplyr::filter(Site=="TX")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-06-03" & date <= "2025-7-16")%>%
  mutate(aparGrow = sum(apar))
rue_GS_NM<-rueDATA_quad%>%
  dplyr::filter(Site=="NM")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-04-27" & date <= "2025-7-11")%>%
  mutate(aparGrow = sum(apar))
rue_GS_SCOLO<-rueDATA_quad%>%
  dplyr::filter(Site=="SCOLO")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-04-22" & date <= "2025-7-8")%>%
  mutate(aparGrow = sum(apar))
rue_GS_NCOLO<-rueDATA_quad%>%
  dplyr::filter(Site=="NCOLO")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-04-23" & date <= "2025-7-3")%>%
  mutate(aparGrow = sum(apar))
rue_GS_WY<-rueDATA_quad%>%
  dplyr::filter(Site=="WY")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-04-10" & date <= "2025-6-30")%>%
  mutate(aparGrow = sum(apar))
rue_GS_MT<-rueDATA_quad%>%
  dplyr::filter(Site=="MT")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-03-24" & date <= "2025-6-26")%>%
  mutate(aparGrow = sum(apar))
rue_GS_SASK<-rueDATA_quad%>%
  dplyr::filter(Site=="SASK")%>%
  group_by(Quadrat)%>%
  dplyr::filter(date >="2025-05-4" & date <= "2025-6-18")%>%
  mutate(aparGrow = sum(apar))

#merge all together
rueDATA_growingseason_quad<-rbind(rue_GS_MT,rue_GS_NCOLO,rue_GS_NM,rue_GS_SASK,rue_GS_SCOLO,rue_GS_TX,rue_GS_WY)
  
site_order <- c("SASK", "MT", "WY", "NCOLO","SCOLO","NM","TX")
#calculate rue by dividing biomass by growing season apar 
rueDATA_growingseason_quad<-rueDATA_growingseason_quad%>%
  mutate(rue=Weight/aparGrow)%>%
  mutate(Site = factor(Site, levels = site_order))%>%
  select(Site,year,Quadrat,Weight,aparGrow,rue)%>%
  unique()

#calculate quad level means
rue_quad_means<-rueDATA_growingseason_quad%>%
  group_by(Site,Quadrat)%>%
  summarise_at(vars(rue),
               list(mean))

#calculate site means 
RUE_means<-rue_quad_means%>%
  group_by(Site)%>%
  summarise_at(vars(rue),
               list(mean))


#assign colors to sites 
site_colors_grassland_hc <- 
  c( "TX" = "#2E7D32", # deep prairie green 
     "NM" = "#E6A160", # warm desert grass / ochre
     "SCOLO" = "#3A6EA5", # strong sky blue 
     "NCOLO" = "indianred", # muted purple (flowers/forbs) 
     "WY" = "#8C5A2B", # rich soil brown 
     "MT" = "plum4", # olive green (different value than TX) 
     "SASK" = "#5C6F7B" )# cool slate / northern grassland )
site_labels <- c(
  "TX"    = "Texas",
  "NM"    = "New Mexico",
  "SCOLO" = "South Colorado",
  "NCOLO" = "North Colorado",
  "WY"    = "Wyoming",
  "MT"    = "Montana",
  "SASK"  = "Saskatchewan"
)


rue_quad_means<-rue_quad_means%>%
  left_join(site_lat, by="Site")
rue_quad_means<-rue_quad_means%>%
  mutate(Site = factor(Site, levels=site_order))
# #plot showing quadrat and boxplot of ANPP means

rue_quad_means$Site<-as.factor(rue_quad_means$Site)
rue_quad_means$Site <- fct_relevel(rue_quad_means$Site, "TX", "NM", "SCOLO","NCOLO","WY","MT","SASK")

#create figure 2b
rue_boxplot<-ggplot()+
  geom_boxplot(data=rue_quad_means,mapping = aes(x=Latitude,y=rue, fill=Site),alpha=.5,color="black",size=1.2,key_glyph = "rect")+
  geom_jitter(data=rue_quad_means,mapping = aes(x=Latitude,y=rue,color=Site),size=4,alpha=.7,width=.2)+
  scale_y_continuous(limits=c(0,1.5),breaks=seq(0,1.5,by=.25),expand = c(0,0))+
  scale_color_manual(values = site_colors_grassland_hc) +
  scale_fill_manual(values = site_colors_grassland_hc) +
  labs(x="Latitude",y="Radiation Use Efficiency (g/Mj)")+
  theme_classic(base_size=25)+
  theme(legend.position = "top")

#arrange with anpp figure
jpeg("anpp rue figure (latitude).jpeg",
     width = 18,
     height =8,
     units = "in",
     res = 300)
ggarrange(anpp_boxplot,rue_boxplot, labels = c("a)", "b)"),common.legend=T,hjust=-4.8,vjust=-.001,font.label = list(size = 18))
dev.off()



ruemeans<-left_join(RUE_means,field_phot, by="Site")%>%
  dplyr::select(C3percent,C4percent,Site,rue)

rue_quad_means

#anova test on difference between rue for sites 
anova_quad_rue <- aov(rue ~ Site, data = rue_quad_means)
summary(anova_quad_rue)
TukeyHSD(anova_quad_rue)

allrueQuad<-left_join(rue_quad_means, field_phot_quad, by=c("Site", "Quadrat"))%>%
  unique()

allrueQuad<-allrueQuad%>%
  pivot_wider(names_from = "Group",
              values_from = "Weight")

library(ggrepel)

 jpeg("rue relationship.jpeg",
      width = 10,
      height =7,
      units = "in",
      res = 300)
ggplot()+
  geom_point(allrueQuad,mapping = aes(x=C3percent,y=rue, color=Site,group=Site), alpha = .6, size=5)+
  geom_point(ruemeans,mapping = aes(x=C3percent,y=rue),color="Black", size=5)+
  labs(
    x = expression("Proportion of C"["3"]*" plants"),
    y = "Radiation Use Efficiency (g/MJ)"
  )+
  theme_classic(base_size = 25)+
  scale_color_manual(values=site_colors_grassland_hc)+
  geom_smooth(allrueQuad,mapping = aes(x=C3percent,y=rue,color=Site), method="lm", se=T,linewidth=2, color="darkblue")+

   geom_label_repel(ruemeans,mapping=aes(x=C3percent,y=rue,label = Site),
                     box.padding   = 0.95,
                     point.padding = 0.95,
                     segment.color = 'grey50',
                     size=6)+
  theme(legend.position = "top")
dev.off()

allrueQuad<-allrueQuad%>%
  mutate(logC3 = log(C3percent+1),
         sqrtC3 = sqrt(C3percent))


model<-lm(rue~C3percent, data=allrueQuad)
summary(model)



lmermodel<-lmer(rue~C3percent + (1|Site),data=allrueQuad)
 summary(lmermodel)
 Anova(lmermodel, type = "II")
 

