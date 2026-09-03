# PROJECT: Hare tracking app
# SCRIPT: 01 - Prepare data for visualization
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 03 Sep 2026
# COMPLETED: 
# LAST MODIFIED: 03 Sep 2026
# R VERSION: 4.5.2

# ______________________________________________________________________________
# 0. Purpose ----
# ______________________________________________________________________________

# I want to be able to visualize any hare's tracking data from a drop-down menu
# including points, paths, and HR contours

# we'll need
  # lookup table with individual attributes
  # GPS relocations
  # HR contours

# ______________________________________________________________________________
# 1. Load packages ----
# ______________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)

# ______________________________________________________________________________
# 2. Read in data ----
# ______________________________________________________________________________

# relocation database
dir.db <- "D:/hare_project/data_analysis/General/hare-gps-processing-new/database/"

# establish connection
db.gps <- dbConnect(SQLite(), paste0(dir.db, "gps.db"))

# read table
tbl.gps <- dbReadTable(db.gps, "gps_clean4") 

# contours
# directory
dir.hr <- "D:/hare_project/data_analysis/General/hare-gps-processing-new/data_cleaned/spatial/"

all.hr <- st_read(paste0(dir.hr, "all_contours.shp"))

# ______________________________________________________________________________
# 3. Clean ----

# to mirror chapter 3, each track will be split by track, season, and period (TSP)

# ______________________________________________________________________________

# create lookup table
lookup.tsp <- tbl.gps |>
  
  group_by(track_season_post) |>
  
  slice(1) |>
  
  ungroup() |>
  
  # keep relevant columns
  dplyr::select(track_season_post, site, sex, MRID, year, season, trt)

# clean GPS relocations
tbl.gps.1 <- tbl.gps |>
  
  dplyr::select(track_season_post, MRID, lat, lon, timestamp, site, year, season, trt) |>
  
  mutate(timestamp = ymd_hms(timestamp, tz = "America/Los_Angeles"))

# HR contours
all.hr$contour <- rep(c("full", "core"), times = 196) 

all.hr.1 <- all.hr |>
  
  mutate(track_season_post = paste0(trackID, "_", season, "_", ifelse(year == "PRE", "PRE", "POST")))

# ______________________________________________________________________________
# 4. Save to local files ----
# ______________________________________________________________________________

# lookup table
saveRDS(lookup.tsp, "data_cleaned/lookup.rds")

# relocations
saveRDS(tbl.gps.1, "data_cleaned/gps.rds")

# contours
st_write(all.hr.1, "data_cleaned/hr.shp", append = F)

# close connection
dbDisconnect(db.gps)
