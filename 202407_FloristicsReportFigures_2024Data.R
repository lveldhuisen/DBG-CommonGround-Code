#2024-07-05
#code for figures with 2024 veg survey data

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(ggthemes) #to make ggplots pretty
library(hrbrthemes) #to make ggplots pretty
library(waffle) # for waffle plots

#bring in data 
#ground cover and seeded species abundance 
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/Conservation - 
      Documents/Restoration/CommonGround Golf Course/Data_rick")

df_2024_GCA <- read.csv("2024_PlantSurveys_GroundCoverAbundance.csv")

#species lists
df_2024_species <- read.csv("2024_PlantSurveys_SpeciesRichness.csv")

data2024_nounknowns <- df_2024_species[-c(498:509,477:481),] #remove unknown species


###bar plot for seeded species success by tx####
ggplot(data2024_nounknowns, aes(x=Treatment, fill = Seeded.))+
  geom_bar()+
  scale_fill_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw(base_size = 15)+
  facet_wrap(.~Plot_number)

###waffle plot to show # of seeded individuals by treatment######
waffle_df_seeded24 <- unique(subset(df_2024_GCA, select = -c(Plot_number,
                                                             Random_quadrat,Seeded.,
                                                             Percent_GC_plants,
  Percent_BG, Percent_GC_thatch)))

waffle_seed24 <- waffle_df_seeded24 %>%
  group_by(Treatment) %>%
  summarise(
    Size_Sum = sum(Number_focal_species)
  )

df_waffle_2024_seeded <- c('C' = 0, 'S' = 64, 'A/S' = 84,'A/S/H'= 116)


#plot
waffle(df_waffle_2024_seeded, row = 15, size = 1)+
  labs(title = "Number of seeded species seedlings by treatment") +
  theme_minimal(base_family = "Roboto Condensed")
