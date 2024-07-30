#2024-07-26
#code for pollinator observation data 

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(ggthemes) #to make ggplots pretty
library(FSA) #for Dunn post hoc test
library(rstatix)#stats tests
library(ggpubr) # to add pvalues to plots


#set working directory
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/
      Conservation - Documents/Restoration/CommonGround Golf Course/Data_rick")

#bring in data

pollinator_df <- read.csv("2024_PollinatorObs_Data.csv")

#clean up data
pollinator_df <- pollinator_df[-c(185:209),] #remove blank rows
names(pollinator_df)[names(pollinator_df) == 'ï..Date'] <- 'Date' #fix column name

pollinator_df <- pollinator_df %>%
  mutate(Date = as.Date(Date, format= "%m/%d/%y"))


#compare treatments
wilcox.test(Total ~ Treatment, 
             data = pollinator_df)

#boxplot comparing total pollinators between tx
ggplot(pollinator_df, aes(x=Treatment,y=Total, fill = Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Total number of pollinators/plot (sum of the four corners)")+
  stat_compare_means(method = "wilcox")+
  ggtitle("2024 Pollinators")
  
#violin plot for differences between treatments
ggplot(pollinator_df, aes(x=Treatment,y=Total, fill = Treatment))+
    geom_violin()+
    theme_bw()+
    ylab("Total number of pollinators")+
    geom_jitter(shape=16, position=position_jitter(0.05))
  stat_summary(fun=mean, geom="point", size=4, color = "forestgreen")

#line graph pollinators by date 
ggplot(pollinator_df, aes(x=Date, y=Total)) +
  geom_point()+
  geom_smooth()+
  scale_x_date(date_minor_breaks = "1 day", 
               date_labels = "%m/%d",
               date_breaks = "1 week")+
  theme_bw()+
  ylab("Total number of pollinators per plot corner")+
  facet_wrap(.~Treatment)+
  ggtitle("2024 Pollinator abundance over summer")

#boxplot for total pollinators grouped by date
ggplot(pollinator_df, aes(x=Date, y=Total, group = Date))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(.~Treatment)+
  ylab("Total number of pollinators per plot corner")

#bar plot for total pollinators grouped by date
ggplot(pollinator_df, aes(y=Total, x = Date))+
  geom_col()+
  theme_bw()+
  facet_wrap(.~Treatment)+
  ylab("Total number of pollinators/plot (sum of the four corners)")


#difference in honey bees by treatment
ggplot(pollinator_df, aes(x=Treatment, y=Honey.bees))+
  geom_boxplot()+
  theme_bw()+
  ylab("Honey bees per plot corner")+
  stat_compare_means(method = "wilcox")

#difference in bumble bees by treatment
ggplot(pollinator_df, aes(x=Treatment, y=Bumble.bees))+
  geom_boxplot()+
  theme_bw()+
  ylab("Bumble bees per plot corner")+
  stat_compare_means(method = "wilcox")

#boxplot proportion native bees by treatment 
ggplot(pollinator_df, aes(x=Treatment, y=Proportion_native_bees))+
  geom_boxplot()+
  theme_bw()+
  ylab("Proportion of native bees per plot corner")+
  stat_compare_means(method = "wilcox", label.x = 1.3)

#boxplot all bees by treatment 
ggplot(pollinator_df, aes(x=Treatment, y=Total_bees))+
  geom_boxplot()+
  theme_bw()+
  ylab("total bees per plot corner")+
  stat_compare_means(method = "wilcox", label.x = 1.3)
