#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Add Physiography to WBD HUC12 dataset
# Date: 7/31/2024
# Name: Nate Jones (natejones@ua.edu)
# Purpose: Add physiography to PC_Headwaters_P2_exploratory.csv dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Before this step, I completed a spatial join in ArcGIS Pro of WBD and physiographic
# datasets. Data sources below: 
# WBD: https://www.usgs.gov/national-hydrography/watershed-boundary-dataset
# Physio: https://www.sciencebase.gov/catalog/item/631405bbd34e36012efa304e

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Setup worksapce ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list = ls())

#load packages of interst
library(tidyverse)
library(patchwork)

#load data
df <- read_csv('data//HAND_class_headwaters_and_downstream_extracts_072924.csv')
HAND_area <- read_csv('data//SO12_SO3plus_by_HAND_class.csv')
HUC12_physio <- read_csv('data//HUC12_Physio.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tidy data ---------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#reduce HUC12_physio table to collumns of interest
HUC12_physio <- HUC12_physio %>% 
  select(HUC_12, DIVISION, PROVINCE, SECTION)  

#join HAND_area to master df
HAND_area <- HAND_area %>% 
  rename(handclass=name) %>% 
  rename(frac = value) %>% 
  mutate(handclass = if_else(handclass=='surface_frac', 'surface', handclass)) %>% 
  mutate(handclass = if_else(handclass=='shallow_frac', 'shallow', handclass)) %>% 
  mutate(handclass = if_else(handclass=='deep_frac', 'deep', handclass))
df <- left_join(df, HAND_area)

#left join to create master dataframe
df <- df %>% 
  rename(HUC_12 = huc12) %>% 
  left_join(., HUC12_physio)

#Export HUC12_phsio cross walk
write_csv(HUC12_physio, "output/HUC12_physio.csv")
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plots plots plots -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Watershed slope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slope <- df %>% 
  filter(headwaters=='SO12') %>% 
  select(HUC_12, slope_mean, DIVISION, frac) %>% 
  mutate(slope_mean_frac = slope_mean*frac) %>% 
  group_by(HUC_12) %>% 
  summarise(
    slope = sum(slope_mean_frac, na.rm=T),
    DIVISION = DIVISION) %>% 
  filter(slope<50) %>% 
  drop_na() %>% 
  ggplot(aes(DIVISION, slope)) + 
    geom_boxplot(outlier.shape = NA) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
            
#depth-to-bedrock ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bedrock <- df %>% 
  filter(headwaters=='SO12') %>% 
  select(HUC_12, bedrock_mean, DIVISION, frac) %>% 
  mutate(bedrock_mean_frac = bedrock_mean*frac) %>% 
  group_by(HUC_12) %>% 
  summarise(
    bedrock_depth = sum(bedrock_mean_frac, na.rm=T),
    DIVISION = DIVISION) %>% 
  filter(bedrock_depth<10000) %>% 
  mutate(bedrock_depth_m = bedrock_depth/100) %>% 
  drop_na() %>% 
  ggplot(aes(DIVISION, bedrock_depth_m)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#clay ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clay <- df %>% 
  filter(headwaters=='SO12') %>% 
  select(HUC_12, CLY_mean, DIVISION, frac) %>% 
  mutate(CLY_mean_frac = CLY_mean*frac) %>% 
  group_by(HUC_12) %>% 
  summarise(
    clay_prc = sum(CLY_mean_frac, na.rm=T),
    DIVISION = DIVISION) %>% 
  filter(clay_prc<50) %>% 
  drop_na() %>% 
  ggplot(aes(DIVISION, clay_prc)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Combine plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slope + bedrock + clay + patchwork::plot_layout(ncol=1, axes='collect')
