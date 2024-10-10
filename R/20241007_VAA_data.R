#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Explore NHD VAA 
# Coders: Nate Jones (natejones@ua.edu)
# Date: 10/10/2024
# Purpose: Extract stream slope and stream density from VAA tables for headwaters 
#          and downstream areas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data source: https://www.usgs.gov/national-hydrography/access-national-hydrography-products
#VAA table definitions: https://www.usgs.gov/national-hydrography/value-added-attributes-vaas

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
remove(list=ls())

# library
library(sf)
library(ggplot2)
library(tidyverse)
library(patchwork)

# download data
df <- read_csv('output//vaa.csv', 
          col_names = c("XCoord","YCoord","OBJECTID","permanent_identifier","fdate",
          "resolution","gnis_id","gnis_name","lengthkm","reachcode","flowdir",
          "wbarea_permanent_identifier","ftype","fcode","mainpath","innetwork",
          "visibilityfilter","nhdplusid","vpuid","streamleve","streamorde",
          "streamcalc","fromnode","tonode","hydroseq","levelpathi","pathlength",
          "terminalpa","arbolatesu","divergence","startflag","terminalfl",
          "uplevelpat","uphydroseq","dnlevel","dnlevelpat","dnhydroseq","dnminorhyd",
          "dndraincou","frommeas","tomeas","rtndiv","thinner","vpuin","vpuout",
          "areasqkm","totdasqkm","divdasqkm","maxelevraw","minelevraw","maxelevsmo",
          "minelevsmo","slope","slopelenkm","elevfixed","hwtype","hwnodesqkm",
          "statusflag","qama","vama","qincrama","qbma","vbma","qincrbma","qcma",
          "vcma","qincrcma","qdma","vdma","qincrdma","qema","vema","qincrema","qfma",
          "qincrfma","arqnavma","petma","qlossma","qgadjma","qgnavma","gageadjma",
          "avgqadjma","gageidma","gageqma","Shape_Length"))

#back that shit up
backup <- df
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: tidy data ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select variables of interest
df <- df %>% 
  select(
    nhdplusid, 
    reachcode,
    streamorde, 
    areasqkm, 
    totdasqkm, 
    slope, 
    slopelenkm
  )

#Define HUC02 and HUC12
df <- df %>% 
  #Define HUC levels
  mutate(HUC12 = substr(reachcode, 1,12)) 

#Define headwaters
df <- df %>% 
  mutate(SO12 = if_else(streamorde <=2, 1, 0))

#Remove -9999
df <- df %>% 
  mutate(
    streamorde = if_else(streamorde>0,   streamorde, NA), 
    areasqkm   = if_else(areasqkm>0,     areasqkm,   NA),
    totdasqkm  = if_else(totdasqkm>0,    totdasqkm,  NA), 
    slope      = if_else(slope>0,        slope,      NA), 
    slopelenkm = if_else(slopelenkm > 0, slopelenkm, NA)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Estimate metrics by HUC ----------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate metrics by HUC12 and SO12
df <- df %>% 
  group_by(HUC12, SO12) %>% 
  summarise(
    stream_slope_mean = mean(slope, na.rm=T), 
    stream_slope_cv   = sd(slope, na.rm=T)/mean(slope, na.rm=T), 
    stream_length_km  = sum(slopelenkm, na.rm=T),
    area_km2          = sum(areasqkm, na.rm=T)) %>% 
  mutate(
    stream_density = stream_length_km/area_km2
  )

#Create wide tibble
df <- df %>% 
  mutate(
    SO12 = if_else(SO12 == 1, "headwater", "downstream")
  ) %>% 
  pivot_wider(
    names_from = SO12, 
    values_from = c(stream_slope_mean, stream_slope_cv, stream_length_km, area_km2, stream_density))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Export csv -----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(df, 'output//vaa_huc12.csv')
