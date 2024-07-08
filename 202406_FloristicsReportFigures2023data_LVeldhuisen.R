#2024-06-04
#code for figures for CommonGround experimental plots 2023 data
install.packages("ggthemes")
install.packages("hrbrthemes")

library(tidyverse) #for making figures
library(dplyr) #data manipulation
library(tidyr) #more data manipulation
library(viridis) #colorblind friendly color palette
library(ggthemes) #to make ggplots pretty
library(hrbrthemes) #to make ggplots pretty
library(waffle)

#waffle plot example code
https://github.com/hrbrmstr/waffle?tab=readme-ov-file

#2023 data for experimental plots-----------------------------------------------

#Bring in and clean up data-----------------------------------------------------

#set working directory
setwd("C:/Users/leah.veldhuisen/Denver Botanic Gardens/
      Conservation - Documents/Restoration/CommonGround Golf Course/Data_rick")

#species richness data
data2023 <- read.csv("2023_PlantSurveys_SpeciesData.csv") #read in csv

data2023_nounknowns <- data2023[-c(580:583,565:579,562:564,561,
                                   555:557,545:548,538,284),] #remove unknown species


#make focal species count column continuous and numeric
data2023_nounknowns$C.value <- as.numeric(as.character(
  data2023_nounknowns$C.value)) 

##reformat to show proportion of native/seeded/C value/wetland species by tx###
#remove unnecessary columns 
df_treatments_summary = subset(data2023_nounknowns, select = 
                                 -c(Date, Random_quadrat, Plot_number))
df_treatments_summary <- unique(df_treatments_summary)
df_treatments_summary %>% count(Treatment, Seeded., Origin, 
                                C.value, Wetland_indicator_status, Species) -> test

clean <- df_treatments_summary %>% group_by(Seeded., Origin, C.value, 
                                            Wetland_indicator_status) %>%
  summarise(n=sum())


#summaries of species by plot
tx_species_counts2023 <- data2023_nounknowns %>% group_by(Plot_number, Treatment) %>% 
  summarise_all(n_distinct)

#combine plot number and treatment columns
tx_species_counts2023$Plot = paste(tx_species_counts2023$Plot_number,
                                   tx_species_counts2023$Treatment, sep=" ")

#remove columns that don't make sense to sum
tx_species_counts2023 = subset(tx_species_counts2023, select = -c(Date, Random_quadrat,
                                            Seeded.,Origin, Wetland_indicator_status,
                                            Plot))

#ground cover and abundance data 
GCA_df_2023 <- read.csv("2023_PlantSurveys_GroundCoverData.csv") #read csv

GCA_df_2023_clean <- na.omit(GCA_df_2023) #get rid of NAs

#make focal species count column continuous and numeric
GCA_df_2023_clean$Number_focal_species <- as.numeric(as.character(
  GCA_df_2023_clean$Number_focal_species)) 

#Figures------------------------------------------------------------------------

##Species richness data#######
###boxplot for species richness by treatment############
ggplot(tx_species_counts2023, aes(x=Treatment, y=Species))+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of species")+
  ggtitle("Species richness by treatment")

###waffle plot for richness by treatment##########

waffle_richness_tx <- tx_species_counts2023 %>%
  group_by(Treatment) %>%
  summarise(
    Size_Sum = sum(Species)
  )

df_richness_2023waffle <- c('C' = 58, 'S' = 63, 'A/S' = 70,'A/S/H'= 66)


#plot
waffle(df_richness_2023waffle, row = 5, size = 1)+
  labs(title = "Species richness by treatment") +
  theme_minimal(base_family = "Roboto Condensed")

###bar plot for c.values by tx####
ggplot(data2023_nounknowns, aes(x=Treatment, fill = C.value))+
  geom_bar()+
  scale_fill_viridis_d()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

###bar plot for origin by tx#####
ggplot(data2023_nounknowns, aes(x=Treatment, fill = Origin))+
  geom_bar()+
  scale_fill_viridis_d(end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

###bar plot for wetland status by tx#########
ggplot(data2023_nounknowns, aes(x=Treatment, fill = Wetland_indicator_status))+
  geom_bar()+
  scale_fill_viridis_d(end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of observations")+
  theme_bw()

###bar plot for seeded species success by tx####
seedlings_2023 <- ggplot(data2023_nounknowns, aes(x=Treatment, fill = Seeded.))+
  geom_bar()+
  scale_fill_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  ylab("Number of quadrats with seeded species")+
  theme_bw(base_size = 15)+
  facet_wrap(.~Plot_number)+
  ggtitle("2023")

plot(seedlings_2023)

###boxplot for C values by treatment#######
ggplot(data2023_nounknowns, aes(x=Treatment, y=C.value))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()

###waffle plot for variables by treatment##########
test$Origin <- fct_relevel(test$Origin, "Native","Introduced","Noxious","Unknown")
test <- na.omit(test)

ggplot(data = test, aes(fill = Seeded., values = n, color = Origin))+
  geom_waffle(n_rows = 4, flip = TRUE, color = "white") +
  facet_wrap(~Treatment, nrow = 1, strip.position = "bottom")+
  scale_x_discrete()+
  scale_fill_viridis_d(begin = 0.1, end = 0.9) +
  coord_equal() +
  labs(x = "Treatment", y = "Species count",
       title = "Number of seeded species by treatment") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), legend.title = element_text()) +
  guides(fill = guide_legend(reverse = T))

##Ground cover and abundance data#######

###boxplot for number of seeded species in each tx######
ggplot(GCA_df_2023_clean, aes(x=Treatment,y=Number_focal_species))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()+
  ylab("Number of individuals of seeded species")+
  ggtitle("Number of seeded individuals by treatment")

###bar plot trying to color code species obs####
ggplot(data2023_nounknowns, aes(x=Treatment, y=Species, fill = C.value))+
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()
  
###boxplot for overall ground cover by treatment#####
ggplot(GCA_df_2023_clean, aes(x=Treatment,y=Percent_GC_overall))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()+
  ylab("% groundcover")+
  ggtitle("Percent ground cover by treatment")

###boxplot for % bare ground by treatment####
ggplot(GCA_df_2023_clean, aes(x=Treatment,y=Percent_BG))+
  geom_boxplot()+
  scale_x_discrete(limits = c("C","S","A/S","A/S/H"))+
  theme_bw()+
  ylab("% bare ground")+
  ggtitle("Percent bare ground by treatment")

ggplot(GCA_df_2023_clean, aes(x=Number_focal_species))+
  geom_histogram()+
  facet_grid(.~Treatment)+
  theme_bw()

###waffle plot to show # of seeded individuals by treatment######
waffle_df_seeded <- unique(subset(GCA_df_2023_clean, select = -c(
  Date, Plot_number,Random_quadrat,Focal_species.,Percent_GC_overall,
  Percent_BG, Percent_GC_thatch)))

waffle_seed <- waffle_df_seeded %>%
  group_by(Treatment) %>%
  summarise(
    Size_Sum = sum(Number_focal_species)
  )

df_waffle_2023_seeded <- c('C' = 0, 'S' = 444, 'A/S' = 590,'A/S/H'= 538)


#plot
waffle(df_waffle_2023_seeded, row = 15, size = 1)+
  labs(title = "Number of seeded species seedlings by treatment") +
  theme_minimal(base_family = "Roboto Condensed")

###bar plot for number of seeded species#######
ggplot(GCA_df_2023_clean, aes())+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  xlab("Teatment")+
  ylab("Number of observations")+
  scale_x_discrete(limits=c("C","S","A/S","A/S/H"))+
  theme_bw(base_size = 15)

  