#2024-06-04
#code for figures for CommonGround experimental plots 2023 data

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette

#2023 data for experimental plots-----------------------------------------------
#bring in and clean up data-----------------------------------------------------

#set working directory
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/
      Conservation - Documents/Restoration/CommonGround Golf Course/Data_rick")

#species richeness data
data2023 <- read.csv("2023_plantsurveys_data.csv") #read in csv

data2023_nounknowns <- data2023[-c(580:583,565:579,562:564,561,
                                   555:557,545:548,538,284),] #remove unknown species

#summaries of species by plot
tx_species_counts2023 <- data2023_nounknowns %>% group_by(Plot_number, Treatment) %>% 
  summarise_all(n_distinct)

#combine plot number and treatment columns
tx_species_counts2023$Plot = paste(tx_species_counts2023$Plot_number,
                                   tx_species_counts2023$Treatment, sep=" ")

#ground cover and abundance data 
GCA_df_2023 <- read.csv("2023_PlantSurveys_GroundCoverData.csv") #read csv

GCA_df_2023_clean <- na.omit(GCA_df_2023)

#figures------------------------------------------------------------------------

##species richness data#######
##boxplot for species richness by treatment############
ggplot(tx_species_counts2023, aes(x=Treatment, y=Species))+
  geom_boxplot(fill="steelblue")+
  theme_bw()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of species")

##bar plot for species richness by treatment###########
ggplot(data2023_nounknowns, aes(x=Treatment))+
  geom_bar(fill="steelblue")+
  theme_bw()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")

#bar plot for c.values by tx
ggplot(data2023_nounknowns, aes(x=Treatment, fill = C.value))+
  geom_bar()+
  scale_fill_viridis_d()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

#bar plot for origin by tx
ggplot(data2023_nounknowns, aes(x=Treatment, fill = Origin))+
  geom_bar()+
  scale_fill_viridis_d(end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

#bar plot for wetland status by tx
ggplot(data2023_nounknowns, aes(x=Treatment, fill = Wetland_indicator_status))+
  geom_bar()+
  scale_fill_viridis_d(end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

#bar plot for seeded species success by tx
ggplot(data2023_nounknowns, aes(x=Treatment, fill = Seeded.))+
  geom_bar()+
  scale_fill_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

##ground cover and abundance data#######

#boxplot for number of seeded species in each tx
ggplot(GCA_df_2023_clean, aes(x=Treatment, y=Number_focal_species))+
  geom_boxplot(fill="steelblue")+
  theme_bw()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of individuals of seeded species")
