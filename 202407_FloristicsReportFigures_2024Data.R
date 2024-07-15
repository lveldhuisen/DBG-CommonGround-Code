#2024-07-05
#code for figures with 2024 veg survey data

install.packages("patchwork")
install.packages("ggpubr")
install.packages("FSA")
install.packages("rstatix")

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(ggthemes) #to make ggplots pretty
library(hrbrthemes) #to make ggplots pretty
library(waffle) # for waffle plots
library(patchwork) #to combine plots
library(ggpubr) #to add p values to plots
library(FSA) #for Dunn post hoc test
library(rstatix)#stats tests

#bring in data 
#ground cover and seeded species abundance 
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/Conservation - 
      Documents/Restoration/CommonGround Golf Course/Data_rick")

df_2024_GCA <- read.csv("2024_PlantSurveys_GroundCoverAbundance.csv")

#species lists
df_2024_species <- read.csv("2024_PlantSurveys_SpeciesRichness.csv")

data2024_nounknowns <- df_2024_species[-c(498:509,477:481,510,38),] #remove unknown species

###stats to compare seeded species by tx######
kruskal.test(Number_focal_species ~ Treatment, 
             data = df_2024_GCA)

dunnTest(Number_focal_species ~ Treatment, data = df_2024_GCA)
pvalues_df <- compare_means(Number_focal_species ~ Treatment, df_2024_GCA)

###bar plot for seeded species success by tx####
seedlings_2024 <- ggplot(data2024_nounknowns, aes(x=Treatment, fill = Seeded.))+
  geom_bar()+
  scale_fill_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of species observations")+
  ylim(0, 60)+
  theme_bw(base_size = 15)+
  facet_wrap(.~Plot_number)+
  ggtitle("2024")

plot(seedlings_2024)

###combine with 2023 data####
seedlings_2023 / seedlings_2024 + 
  plot_layout(guides = "collect") +
  plot_layout(axes = "collect")

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

df_waffle_2024_seeded <- c('C' = 0, 'S' = 64, 'A/S' = 145,'A/S/H'= 116)


#plot
waffle(df_waffle_2024_seeded, row = 15, size = 1)+
  labs(title = "Number of seeded species seedlings by treatment") +
  theme_minimal(base_family = "Roboto Condensed")

###boxplot for number of seeded species in each tx######
ggplot(df_2024_GCA, aes(x=Treatment,y=Number_focal_species))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()+
  ylab("Number of individuals of seeded species")+
  ggtitle("2024")+
  ylim(0,35)+
  geom_bracket(xmin = c("C","C","C"), xmax = c("S","A/S","A/S/H"),
               y.position = c(15, 25, 34), label = c("p<0.01","p<0.01","p<0.01"), 
               tip.length = 0.01)
  

#Combined 2023 and 2024 plot data-----------------------------------------------

##add columns for year###
GCA_df_2023_clean$Year = c(2023) 
df_2024_GCA$Year = c(2024)

#remove unecessary columns
GCA_df_2023_clean = subset(GCA_df_2023_clean, select = 
                                 -c(Date, Percent_BG,Percent_GC_overall,
                                    Percent_GC_thatch))
df_2023_tocombine <- GCA_df_2023_clean

#rename column to match 2024###
df_2023_tocombine = rename(df_2023_tocombine, Seeded. = Focal_species.)

#remove columns from 2024
df_2024_tocombine = subset(df_2024_GCA, select = 
                             -c(X, ï..Date,Percent_GC_plants, Percent_BG,
                                Percent_GC_thatch))

#combine dataframes 
total_GCA <- rbind(df_2023_tocombine, df_2024_tocombine)

##boxplot for both years number of seeded species#####

total_GCA %>%
  group_by(Treatment) %>%
  dunn_test(Number_focal_species ~ Year) %>%
  adjust_pvalue(method = "bonferroni")

ggplot(total_GCA, aes(x=Treatment,y=Number_focal_species, fill = factor(Year)))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw(base_size = 14)+
  ylab("Number of seeded individuals")+
  scale_fill_manual(values=c("#2A788EFF", "#FDE725FF"),name = "Year")

##boxplot showing the number of seeded individuals across all tx in each year####
ggplot(total_GCA, aes(x=factor(Year),y=Number_focal_species))+
  geom_boxplot()+
  theme_bw(base_size = 14)+
  ylab("Number of seeded individuals")+
  xlab("Year")+
  stat_compare_means(method = "wilcox.test", label.x = "2024")

wilcox.test(x=total_GCA$Year, y=total_GCA$Number_focal_species,
            data=total_GCA, p.adjust.method = "bonferroni")
