# This script saves the outputs from the model simulations

# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ncdf4)
library(ncdf4.helpers)

# 412 ppm ----

#BiomeE_P0_FIN_aCO2_annual_tile    <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv"))
#BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv"))
BiomeE_P0_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_tile.csv"))
BiomeE_P0_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P1_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS1_FIN_aCO2_annual_tile.csv"))
BiomeE_P1_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS1_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P2_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS2_FIN_aCO2_annual_tile.csv"))
BiomeE_P2_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS2_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P3_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS3_FIN_aCO2_annual_tile.csv"))
BiomeE_P3_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS3_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P4_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS4_FIN_aCO2_annual_tile.csv"))
BiomeE_P4_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS5_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P5_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS5_FIN_aCO2_annual_tile.csv"))
BiomeE_P5_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS5_FIN_aCO2_annual_cohorts.csv"))

BiomeE_P6_FIN_aCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_tile.csv"))
BiomeE_P6_FIN_aCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS6_FIN_aCO2_annual_cohorts.csv"))

# order PFTs
PFT_species <- c("Pinus_sylvestris_PFT1","Picea_abies_PFT2","Betula_pendula_PFT3","Grasses_PFT8") 
PFT <- c(3,4,2,1)
PFT_reorder <- c(1,2,3,8)
PFT_order <- tibble(PFT_species,PFT,PFT_reorder)
PFT_order

source("/home/laura/DBEN/analysis/04_DBEN_variables.R")

# Carbon budget closure ----
## A. cwood_tile ----
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output
cwood_tile_df(BiomeE_P0_FIN_aCO2_annual_tile)

## B. WDgrowth_tile ----
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output
WDgrowth_tile_df(BiomeE_P0_FIN_aCO2_annual_tile)

## C. cmort_tile ----
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output
cmort_tile_df(BiomeE_P0_FIN_aCO2_annual_tile)

# Pools ----
# 1. Carbon mass in vegetation by PFT ----
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
cveg_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

#for (i in 0:6) {
#  cveg_df(noquote(paste0("BiomeE_P", i,"_aCO2_annual_cohorts")))
#}
#assign(paste("dataset_id_", i),  subset(data, id == i) ) 

# 2. Aboveground woody biomass ----
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# cohorts output
AGcwood_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
AGcwood_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 3. Carbon mass in wood by PFT ----
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
cwood_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 4. Carbon mass in wood by size class ----
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
cwood_size_df(BiomeE_P6_FIN_aCO2_annual_cohorts)
  
# 5. Stem number by size class ----
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
nstem_size_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 6. Leaf area index ----
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
lai_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 7. Crown area ----
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
CA_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 8. Basal area ----
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
BA_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 9. 95th percentile of tree height ----
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
height_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# Fluxes ----
# 10. Woody biomass growth ----
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
WBgrowth_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 11. Basal area growth ----
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
BAgrowth_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
cmort_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
cmort_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

cmort_size_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
cmort_size_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ----
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
stemmort_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

stemmort_size_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
stemmort_size_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ----
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
gpp_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ----
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp_df(BiomeE_P0_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P1_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P2_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P3_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P4_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P5_FIN_aCO2_annual_cohorts)
npp_df(BiomeE_P6_FIN_aCO2_annual_cohorts)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ----
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output
nbp_df(BiomeE_P0_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P1_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P2_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P3_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P4_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P5_FIN_aCO2_annual_tile)
nbp_df(BiomeE_P6_FIN_aCO2_annual_tile)

# 562 ppm ----

BiomeE_P0_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_P0_FIN_eCO2_annual_tile.csv"))
BiomeE_P0_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_P0_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P1_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS1_FIN_eCO2_annual_tile.csv"))
BiomeE_P1_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS1_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P2_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS2_FIN_eCO2_annual_tile.csv"))
BiomeE_P2_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS2_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P3_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS3_FIN_eCO2_annual_tile.csv"))
BiomeE_P3_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS3_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P4_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS4_FIN_eCO2_annual_tile.csv"))
BiomeE_P4_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS5_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P5_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS5_FIN_eCO2_annual_tile.csv"))
BiomeE_P5_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS5_FIN_eCO2_annual_cohorts.csv"))

BiomeE_P6_FIN_eCO2_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS6_FIN_eCO2_annual_tile.csv"))
BiomeE_P6_FIN_eCO2_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS6_FIN_eCO2_annual_cohorts.csv"))

source("/home/laura/DBEN/analysis/04_DBEN_variables.R")

# Pools ----
# 1. Carbon mass in vegetation by PFT ----
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
cveg_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

#for (i in 0:6) {
#  cveg_df(noquote(paste0("BiomeE_P", i,"_eCO2_annual_cohorts")))
#}
#assign(paste("dataset_id_", i),  subset(data, id == i) ) 

# 2. Aboveground woody biomass ----
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# cohorts output
AGcwood_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
AGcwood_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 3. Carbon mass in wood by PFT ----
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
cwood_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 4. Carbon mass in wood by size class ----
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
cwood_size_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 5. Stem number by size class ----
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
nstem_size_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 6. Leaf area index ----
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
lai_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 7. Crown area ----
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
CA_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 8. Basal area ----
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
BA_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 9. 95th percentile of tree height ----
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
height_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# Fluxes ----
# 10. Woody biomass growth ----
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
WBgrowth_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 11. Basal area growth ----
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
BAgrowth_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
cmort_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
cmort_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

cmort_size_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
cmort_size_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ----
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
stemmort_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

stemmort_size_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
stemmort_size_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ----
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
gpp_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ----
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp_df(BiomeE_P0_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P1_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P2_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P3_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P4_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P5_FIN_eCO2_annual_cohorts)
npp_df(BiomeE_P6_FIN_eCO2_annual_cohorts)

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ----
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output
nbp_df(BiomeE_P0_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P1_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P2_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P3_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P4_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P5_FIN_eCO2_annual_tile)
nbp_df(BiomeE_P6_FIN_eCO2_annual_tile)

