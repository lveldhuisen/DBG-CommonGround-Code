#2024-05-31
#code for 2022 CommonGround species surveys figures to go in report

install.packages("devtools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("viridis")
install.packages("janitor")
install.packages("treemapify")

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(devtools) #to download waffle plot package
library(janitor) #calculate proportions of dataset
library(treemapify) #make tree maps to show community comp


#2022 data for the entire golf course-------------------------------------------
#Bring in and clean data--------------------------------------------------------
#bring in data
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/
      Conservation - Documents/Restoration/CommonGround Golf Course/
      Data_rick") #set working directory 

allplots2022_byplot <- read.csv("2022_CGGC_TaxaByPlot_combined.csv") #read in csv


#remove entries without plot numbers
allplots2022_byplot_clean <- allplots2022_byplot[!allplots2022_byplot$plot %in% 
                                                   c("16/1?", "Unknown",
                                                     "GC eastern access road",
                                                     "parking lot ",
                                                     "7?"), ]

#remove plot info and combine all species together 
all2022_noplots <- allplots2022_byplot #copy dataframe
all2022_noplots = subset(allplots2022_byplot, 
                         select = -c(plot) ) #remove plot column

all2022_noplots <- distinct(all2022_noplots) #combine entries of the same species

#summaries of species by plot
plot_species_counts_df <- allplots2022_byplot %>% group_by(plot) %>% 
  summarise_all(n_distinct)

plot_species_counts_df <- plot_species_counts_df[-c(9,17,20,21,22),] #remove species not in plots

#Figures------------------------------------------------------------------------

##whole site together without plot level data#####

###number of taxa by status for whole site######
ggplot(all2022_noplots, aes(x=factor(Status)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  xlab("Native vs Introduced")+
  ylab("Number of species")+
  scale_x_discrete(limits=c("Native","Introduced","Noxious","Unknown"))+
  theme_bw()+
  ggtitle("Distribution of native, introduced and noxious species for whole site")

#treemap for species origin
ggplot(testwaffle_df, aes(area = n, fill = Status)) +
  geom_treemap()

###conservation values for whole site##########
ggplot(all2022_noplots, aes(x=C.value))+
  geom_bar(stat = "count", width = 0.7, fill="steelblue")+
  xlab("Conservation value")+
  ylab("Number of species")+
  xlim("0","1","2","3","4","5","6","7","Unknown")+
  theme_bw()+
  ggtitle("Distribution of conservation values")

###wetland indicator status for whole site##########
ggplot(allplots2022_byplot, aes(x=Wetland_indicator_status))+
  geom_bar(stat = "count", width = 0.7, fill="steelblue")+
  xlab("Wetland Indicator Status")+
  ylab("Number of species")+
  scale_x_discrete(limits = c("OBL","FACW","FAC","FACU","UPL","Unknown"))+
  theme_bw()+
  ggtitle("Dsitribution of wetland indicator statuses")

##plot level figures##########

###wetland indicator status breakdown by plot#####
ggplot(allplots2022_byplot_clean, aes(x=plot, fill = Wetland_indicator_status))+
  geom_bar()+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16","18"))+
  ylab("Number of species")+
  theme_bw()+
  labs(fill = "Wetland indicator status")+
  scale_fill_viridis_d(breaks=c("OBL","FACW","FAC","FACU","UPL","Unknown"))+
  ggtitle("Wetland indicator statuses by plot")

###conservation status############
ggplot(allplots2022_byplot_clean, aes(x=plot, fill = C.value))+
  geom_bar() +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16","18"))+
  scale_fill_viridis_d()+
  ylab("Number of species")+
  theme_bw()+
  ggtitle("Distribution of conservation values by plot")

###native/invasive by plot#######
ggplot(allplots2022_byplot_clean, aes(x=plot, fill = Status))+
  geom_bar()+
  scale_fill_viridis_d()+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16","18"))+
  ylab("Number of species")+
  theme_bw()+
  ggtitle("Native vs introduced by plot")

###waffle plot for species origin by plot#####
#make data set for waffle plot
waffle_df <- unique(subset(allplots2022_byplot_clean, select = -c(plot)))
tabyl(waffle_df, Status)
df_waffle_2022 <- c('Native' = 54, 'Introduced' = 75, 'Noxious' = 15, 'Unknown' = 29)

#try different way to make data set for waffle
allplots2022_byplot_clean %>%
  count(Status, taxon) -> testwaffle_df

#plot

waffle(df_waffle_2022, row = 4, size = 1.5)+
  labs(
    title = "Proportion of native and non-native species at CommonGround"
  ) +
  theme_minimal(
    base_family = "Roboto Condensed"
  )


###number of species by plot#########
ggplot(plot_species_counts_df, aes(x=plot, y=taxon))+
  geom_bar(stat = "identity", fill="steelblue")+
  ylab("Number of species")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16","18"))+
  theme_bw()+
  ggtitle("Number of species per plot")
