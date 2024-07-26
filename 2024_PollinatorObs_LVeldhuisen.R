#2024-07-26
#code for pollinator observation data 

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(ggthemes) #to make ggplots pretty
library(FSA) #for Dunn post hoc test
library(rstatix)#stats tests


#set working directory
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/
      Conservation - Documents/Restoration/CommonGround Golf Course/Data_rick")

#bring in data

pollinator_df <- read.csv("2024_PollinatorObs_Data.csv")

#clean up data
pollinator_df <- pollinator_df[-c(160:209),] #remove blank rows
names(pollinator_df)[names(pollinator_df) == 'ï..Date'] <- 'Date' #fix column name

pollinator_df <- pollinator_df %>%
  mutate(Date = as.Date(Date, format= "%m/%d/%y"))


#compare treatments
wilcox.test(Total ~ Treatment, 
             data = pollinator_df)

#boxplot comparing total pollinators between tx
ggplot(pollinator_df, aes(x=Treatment,y=Total))+
  geom_violin()+
  theme_bw()+
  ylab("Total number of pollinators")+
  geom_jitter(shape=16, position=position_jitter(0.05))
  stat_summary(fun=mean, geom="point", size=4, color = "forestgreen")

#line graph pollinators by date 
ggplot(pollinator_df, aes(x=Date, y=Total)) +
  geom_smooth()+
  scale_x_date()+
  theme_bw()+
  ylab("Total number of pollinators")
