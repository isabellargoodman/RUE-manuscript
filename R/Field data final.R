#load libraries
library(tidyverse)
library(dplyr)
library(forcats)
library(ggpubr)

#create site order variables
site_order <- c("SASK", "MT", "WY", "NCOLO","SCOLO","NM","TX")

#read in cleaned field data
setwd("~/Desktop/Thesis stuff/RUE")
field<-read.csv("clean2.csv")%>%
  mutate_at("Quadrat",str_replace, "Q","")%>%
  mutate(Weight=Weight*4)

#remove spaces and reorder sites
field <- field %>%
  mutate(across(c(Group, Site), ~ gsub(" ", "", .))) %>%
  mutate(Site = factor(
    Site,
    levels = c("TX", "NM", "SCOLO", "NCOLO", "WY", "MT", "SASK")
  ))

#remove unnecessary columns and find the sum for each site,quad, group 
field<-field%>%
  group_by(Site, Quadrat, Group,Photosynthetic_pathway,Life_history,G_F)%>%
  summarise_at(vars(Weight),
               list(sum))%>%
  ungroup()

#calculate quadrat level total 
field_quad_totals<-field%>%
  group_by(Site,Quadrat )%>%
  summarise_at(vars(Weight),
               list(sum))%>%
  ungroup()

#pivot full wider and replace na with zeros. 
field_wide <- field %>%
  pivot_wider(
    names_from  = Group,
    values_from = Weight
  )

#calculate means with all quadrats 
field_wide_means<-field_wide%>%
  group_by(Site)%>%
  summarise_at(vars(`AF-C4`, `PF-C3`, `PG-C4`, `AF-C3`, `PG-C3`, `AG-C3`),
                    list(mean))
#pivot longer 
field_long_means<-field_wide_means%>%
  pivot_longer(cols = c("AF-C4","PF-C3", "PG-C4", "AF-C3", "PG-C3", "AG-C3"),
               names_to = "Group",
               values_to = "Weight")


#reorder sites and groups
field_long_means$Site <- factor(
  field_long_means$Site,
  levels = c("SASK", "MT", "WY", "NCOLO", "SCOLO","NM","TX"))

group_order <- c(
  "AF-C4","AF-C3","PF-C3","AG-C3","PG-C3","PG-C4")

field_long_means <- field_long_means %>%
  mutate(
    Group = factor(Group, levels = group_order))



#create another wide dataset for analysis 
field_wide_2<-field_wide%>%
  ungroup()%>%
  dplyr::select(-Photosynthetic_pathway,-Life_history,-G_F)

#Calculate pfg specific totals
field_wide_2<-field_wide_2%>%
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  group_by(Site,Quadrat)%>%
  summarise_at(vars(`AF-C4`,`PF-C3`,`PG-C4`,`AF-C3`,`PG-C3`,`AG-C3`),
               list(sum))%>%
  mutate(total=`AF-C4`+`PF-C3` +`PG-C4`+ `AF-C3` + `PG-C3` + `AG-C3`)%>%
  mutate(PGall=(`PG-C3`+`PG-C4`))%>%
  mutate(Grass_all=(`PG-C3`+`PG-C4`+`AG-C3`))%>%
  mutate(C3percent=`PG-C3`/PGall,
         C4percent=`PG-C4`/PGall)%>%
  drop_na()

#calculate site level totals
field_totals<-field_wide_2%>%
  group_by(Site)%>%
  summarise(across(c(total),mean))%>%
  drop_na()

#calculate c3 percent and c4 perecnet contributions
percent_average<-field_wide_2%>%
  group_by(Site)%>%
  summarise(across(c(C3percent, C4percent),mean))%>%
  pivot_longer(cols=c("C3percent","C4percent"),names_to = "Group",values_to = "Weight")%>%
  drop_na()

# total c3/c4 photosynthetic averages
field_phot<-field%>%  
  pivot_wider(
  names_from  = Photosynthetic_pathway,
  values_from = Weight)%>%
  mutate(across(where(is.numeric),~replace_na(.x,0)))

field_phot<-field_phot%>%
  group_by(Site,Quadrat)%>%
  summarise_at(vars(C3,C4),
               list(sum))%>%
  mutate(total=C3+C4)

field_phot<-field_phot%>%
  group_by(Site)%>%
  summarise_at(vars(C4,C3,total),
               list(mean))

field_phot<-field_phot%>%
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  mutate(C3percent=C3/total,
         C4percent=C4/total)
field_phot_long<-field_phot%>%
  pivot_longer(cols=c("C3percent","C4percent"),names_to = "Group",values_to = "Weight")%>%
  drop_na()

#c3/c4 quadrat level photosynthetic averages 
field_phot_quad<-field%>%
  group_by(Site, Quadrat,Photosynthetic_pathway)%>%
  summarise_at(vars(Weight),
               list(sum))

field_phot_quad<-field_phot_quad%>%
  pivot_wider(
  names_from  = Photosynthetic_pathway,
  values_from = Weight
)%>%
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  mutate(total=C3+C4)

field_phot_quad<-field_phot_quad%>%
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  mutate(C3percent=C3/total,
         C4percent=C4/total)%>%
  mutate(Site = factor(Site, levels = site_order))

field_phot_quad_long<-field_phot_quad%>%
  pivot_longer(cols=c("C3percent","C4percent"),names_to = "Group",values_to = "Weight")%>%
  drop_na()%>%
  mutate(Site = factor(Site, levels = site_order))
# calculate grass percentage of photosynthetic pathway


percent_average<-percent_average%>%
  mutate(Site = factor(Site, levels = site_order))

field_phot_long<-field_phot_long%>%
  mutate(Site = factor(Site, levels = site_order))

percent_average<-percent_average%>%
  left_join(mean_climate, by="Site")

jpeg("grass prop.jpeg",
     width = 10,
     height = 10,
     units = "in",
     res = 300)
ggplot()+
  geom_col(data=percent_average, mapping=aes(x=Site,y=Weight, fill=Group),width=.8,position="stack")+
  labs(x="Site/Mean Annual Temperature",y="Proportion of total Biomass")+
  scale_fill_manual(values=c( "#BA8A36","#DAB981"),
                    labels = c("C3 Perennial Grasses",
                               "C4 Perennial Grasses"))+
  scale_x_discrete(labels=c("SASK\n4°C","MT\n8°C","WY\n8°C","NCOLO\n9°C","SCOLO\n12°C","NM\n14°C","TX\n17°C"))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic(26)+
  theme(legend.position = "bottom")
dev.off()


site_lat<-data.frame(
  Site=c("TX","NM","SCOLO","NCOLO","WY","MT","SASK"),
  Latitude=c(30.325422,
               33.617455,
               38.212620,
               40.843213,
               43.319558,
               47.744912,
               52.180)) 

# 
#colors for sites
site_colors_grassland_hc <-
  c( "TX" = "#2E7D32", # deep prairie green
     "NM" = "#E6A160", # warm desert grass / ochre
     "SCOLO" = "#3A6EA5", # strong sky blue
     "NCOLO" = "indianred", # muted purple (flowers/forbs)
     "WY" = "#8C5A2B", # rich soil brown
     "MT" = "plum4", # olive green (different value than TX)
     "SASK" = "#5C6F7B" )# cool slate / northern grassland )
# 

field_quad_totals<-field_quad_totals%>%
  left_join(site_lat, by="Site")
field_quad_totals<-field_quad_totals%>%
  mutate(Site = factor(Site, levels=site_order))
# #plot showing quadrat and boxplot of ANPP means

field_quad_totals$Site<-as.factor(field_quad_totals$Site)
field_quad_totals$Site <- fct_relevel(field_quad_totals$Site, "TX", "NM", "SCOLO","NCOLO","WY","MT","SASK")
  
anpp_boxplot <- ggplot()+
  geom_boxplot(data=field_quad_totals,mapping = aes(x=Latitude,y=Weight, fill=Site),alpha=.5,color="black",size=1.2,key_glyph = "rect")+
  geom_jitter(data=field_quad_totals,mapping = aes(x=Latitude,y=Weight,color=Site),size=4,alpha=.7,width=.2)+
 # scale_y_continuous(limits=c(0,3),breaks=seq(0,3,by=.5),expand = c(0,0))+
  scale_color_manual(values = site_colors_grassland_hc ) +
  scale_fill_manual(values = site_colors_grassland_hc) +
  labs(y = expression("Aboveground Net Primary Production (g/m"^"2"*")"), x= "Latitude") +
  theme_classic(base_size=25)+
  theme(legend.position = "top")
# 

# #anova for site level differences *SIGNIFANT 
anova_quad_ANPP <- aov(Weight ~ Site, data = RUE_means)
blah<-lm(rue~lat,data=RUE_means)
summary(blah)

TukeyHSD(anova_quad_ANPP)
#  
#  

