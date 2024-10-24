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
pnts <- read_csv('output//vaa.csv', 
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

#Convert to spatial points
pnts <- pnts[is.na(pnts$XCoord)!=T,]
pnts <- st_as_sf(pnts, coords = c("XCoord", "YCoord"), crs = '+proj=longlat +datum=WGS84 +no_defs')
pnts <- pnts %>% select(vpuid, reachcode)

# load WBD dataset
sheds <- st_read('data//NHDPlus_H_National_Release_1_GDB//NHDPlus_H_National_Release_1_GDB.gdb', layer = 'WBDHU12')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Convert VAA table to spatial points ----------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to spatial join by huc04

#~~~~~~~~~~~~~~
# it looks like VPUID is NULL throughout HUC12 dataset. Fall back to huc4
# as unique id for analysis
# going to bed....
#~~~~~~~~~~~~~~




fun <- function(vpu_id){
  #reduce size of data
  pnts <- pnts %>% filter(vpuid == vpu_id)
  sheds <- sheds %>% filter(vpuid == vpu_id) %>% select(huc12)
  
  #convert spatial reference system
  sheds <- st_transform(sheds, st_crs(pnts))
  
  #spatial join
  pnts <- st_join(pnts, sheds)
  
  #export points
  pnts %>% st_drop_geometry()
}

#Create list of VPU_IDs
vpu_ids <- sheds %>% st_drop_geometry %>% select(vpuid) %>% pull()
