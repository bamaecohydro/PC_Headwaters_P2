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
pnts <- pnts %>% select(reachcode)

# load WBD dataset
sheds <- st_read('data//NHDPlus_H_National_Release_1_GDB//NHDPlus_H_National_Release_1_GDB.gdb', layer = 'WBDHU12')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Overlay reaches with HUC12s ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to spatial join by huc04
fun <- function(huc04){

  #reduce size of data
  pnts  <- pnts  %>% filter(substr(reachcode,1,4) == huc04)
  sheds <- sheds %>% filter(substr(huc12,1,4)     == huc04) %>% select(huc12)
  
  #convert spatial reference system
  sheds <- st_transform(sheds, st_crs(pnts))
  
  #spatial join
  pnts <- st_join(pnts, sheds)
  
  #export points
  pnts %>% st_drop_geometry()
}

#Create list of huc04s
huc04s <- sheds %>% st_drop_geometry() %>% mutate(huc04 = substr(huc12, 1,4)) %>% select(huc04) %>% distinct()

#apply 
t0 <- Sys.time()
output <- lapply(
  X = huc04s$huc04, 
  FUN = fun) %>% bind_rows()
tf <- Sys.time()
tf-t0

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Export cross walk table ----------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(output, "output//reachcode2huc12.csv")
