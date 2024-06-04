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

data2023 <- read.csv("2023_plantsurveys_data.csv") #read in csv

data2023_nounknowns <- data2023[-c(1:28, 576:579),] #remove unknown species

#summaries of species by plot
tx_species_counts2023 <- data2023_nounknowns %>% group_by(Plot_number, Treatment) %>% 
  summarise_all(n_distinct)

#combine plot number and treatment columns
tx_species_counts2023$Plot = paste(tx_species_counts2023$Plot_number,
                                   tx_species_counts2023$Treatment, sep=" ")

#figures------------------------------------------------------------------------

##boxplot for species richness by treatment############
ggplot(tx_species_counts2023, aes(x=Treatment, y=Species))+
  geom_boxplot(fill="steelblue")+
  theme_bw()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of species")

##bar plot for species richness by treatment###########
ggplot(data2023_nounknowns, aes(x=Treatment, y=Species))+
  geom_bar(stat = "identity")+
  theme_bw()
