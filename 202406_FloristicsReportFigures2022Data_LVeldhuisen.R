#2024-05-31
#code for 2022 CommonGround species surveys figures to go in report

install.packages("devtools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("viridis")
install.packages("janitor")
install.packages("treemapify")
install.packages("flextable")
install.packages("camtrapR")
install.packages("taxize")

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(devtools) #to download waffle plot package
library(janitor) #calculate proportions of dataset
library(treemapify) #make tree maps to show community comp
library(flextable) #to make tables exportable to Word
library(camtrapR)#check species name spelling
library(taxize) #species names
library(waffle) #to make waffle plots


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
                                                     "7?","18"), ]


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
  xlab("Species origin")+
  ylab("Number of species")+
  scale_x_discrete(limits=c("Native","Introduced","Noxious","Unknown"))+
  theme_bw(base_size = 15)+
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

###wetland indicator status waffle plot######
#make data set for waffle plot
waffle_df_WIS <- unique(subset(allplots2022_byplot_clean, select = -c(plot)))
tabyl(waffle_df_WIS, Wetland_indicator_status)
df_waffle_2022_WIS <- c('OBL'=5,'FACW' = 11,'FAC' = 12, 'FACU' = 44, 
                        'UPL'= 7, 'Unknown' = 94)


#plot
waffle(df_waffle_2022_WIS, row = 4, size = 1, colors = 
         c("deepskyblue4","deepskyblue2","palegreen2",
           "palegreen3","palegreen4","cornsilk2"))+
  labs(title = "Distribution of species' wetland indicator statuses at CommonGround") +
  theme_minimal(base_family = "Roboto Condensed")

##plot level figures##########

###wetland indicator status breakdown by plot#####


ggplot(allplots2022_byplot_clean, 
       aes(x=plot, fill = Wetland_indicator_status))+
  geom_bar()+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16"))+
  ylab("Number of species")+
  theme_bw()+
  labs(fill = "Wetland indicator status")+
  scale_fill_manual(breaks = c("OBL","FACW","FAC","FACU","UPL","Unknown"),
                    values = c("deepskyblue4","deepskyblue2",
                               "palegreen2","palegreen3","palegreen4","cornsilk2"))+
  ggtitle("Wetland indicator statuses by plot")


###conservation status############
ggplot(allplots2022_byplot_clean, aes(x=plot, fill = C.value))+
  geom_bar() +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16"))+
  scale_fill_manual(breaks = c("0","1","2","3","4","5","6","7","Unknown"),
                    values = c("black","grey50","grey70",
                               "grey89","palegreen1","palegreen2",
                               "palegreen3","palegreen4","cornsilk2"))+
  ylab("Number of species")+
  theme_bw()+
  ggtitle("Distribution of conservation values by plot")

###conservation values waffle plot#######
#make data set for waffle plot
waffle_df_C <- unique(subset(allplots2022_byplot_clean, select = -c(plot)))
tabyl(waffle_df_C, C.value)
df_waffle_2022_C <- c('0' = 91, '1' = 3, '2' = 7,'3'=5,
                      '4'= 14,'5'=2,'6'=2, '7'=3, 'Unknown' = 46)


#plot
waffle(df_waffle_2022_C, row = 4, size = 1, colors = c("black","grey50","grey70",
                                                       "grey89","palegreen1","palegreen2",
                                                       "palegreen3","palegreen4","cornsilk2"))+
  labs(title = "Distribution of species' conservation values at CommonGround") +
  theme_minimal(base_family = "Roboto Condensed")

###origin by plot#######
ggplot(allplots2022_byplot_clean, aes(x=plot, fill = Status))+
  geom_bar()+
  scale_fill_manual(breaks = c("Native","Introduced","Noxious","Unknown"),
                    values = c("palegreen4","gray70","black","cornsilk2"))+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16"))+
  ylab("Number of species")+
  theme_bw()+
  ggtitle("Species origin by plot")

###waffle plot for species origin by plot#####
#make data set for waffle plot
waffle_df <- unique(subset(allplots2022_byplot_clean, select = -c(plot)))
tabyl(waffle_df, Status)
df_waffle_2022 <- c('Native' = 54, 'Introduced' = 75, 'Noxious' = 15, 'Unknown' = 29)

#try different way to make data set for waffle
allplots2022_byplot_clean %>%
  count(Status, taxon) -> testwaffle_df

#plot

waffle(df_waffle_2022, row = 4, size = 1.5, colors = c("palegreen4","gray70",
                                                       "black","cornsilk2"))+
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
                              "13","14","15","16"))+
  theme_bw()+
  ggtitle("Number of species per plot")

#test making nicely formatted table---------------------------------------------
#remove extra columns
speciestable2022 <- read.csv("2022_RawSpeciesList.csv")

#make table
set_flextable_defaults(
  font.family = "Arial", 
  font.size = 10,
  padding = 6)
                     
table <- flextable(speciestable2022)
table <- set_header_labels(table, ï..2022.Entire.site.species = "2022 Species List")
table <- width(table, width = 2.5)
table <- bold(table, bold = TRUE, part = "header")
table
