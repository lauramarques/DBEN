
# This script creates the functions to save the outputs as netCDF from the model simulations

# load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ncdf4)
library(ncdf4.helpers)

# C budget closure ----

## A. cwood_tile ----
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# tile output

cwood_tile_df <- function(data){
  cwood_tile <- data |>
    slice(595:nrow(data)) |> 
    mutate(year = 1:936, 
           cwood = SapwoodC+WoodC) |>
    select(year, cwood) 
  cwood_tile_wid <- cwood_tile |> select(-year)
  # create the netCDF filename 
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                          "/BiomeEP_CBudget_cwood_",name, ".nc", sep=""))
  # define dimensions
  time <- as.array(seq(1,935,1))
  time_dim <- ncdim_def("time","years",as.double(time)) 
  # define variables
  cwood_tile_var <- ncvar_def("cwood","kg C m-2",list(time_dim),-999,
                       "Carbon mass in wood",prec="single")
  # create netCDF file and put arrays
  cwood_tile_ncout <- nc_create(ncfname,list(cwood_tile_var),force_v4=TRUE)
  # put variables
  cwood_tile_array <- simplify2array(cwood_tile_wid)
  ncvar_put(cwood_tile_ncout,cwood_tile_var,cwood_tile_array)
  # Get a summary of the created file
  cwood_tile_ncout
  # close the file, writing data to disk
  nc_close(cwood_tile_ncout)
  return(cwood_tile_var)
}

# B. WDgrowth_tile ----
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output

WDgrowth_tile_df <- function(data){
  WDgrowth_tile <- data |>
    slice(595:nrow(data)) |> 
    mutate(year = 1:936, 
           WDgrowth = WDgrow+WDrepr) |>
    select(year, WDgrowth) 
  WDgrowth_tile_wid <- WDgrowth_tile |> select(-year)
  # create the netCDF filename 
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                          "/BiomeEP_CBudget_WDgrowth_",name, ".nc", sep=""))
  # define dimensions
  time <- as.array(seq(1,935,1))
  time_dim <- ncdim_def("time","years",as.double(time)) 
  # define variables
  WDgrowth_tile_var <- ncvar_def("WDgrowth","kg C m-2 yr-1",list(time_dim),-999,
                              "Woody biomass growth",prec="single")
  # create netCDF file and put arrays
  WDgrowth_tile_ncout <- nc_create(ncfname,list(WDgrowth_tile_var),force_v4=TRUE)
  # put variables
  WDgrowth_tile_array <- simplify2array(WDgrowth_tile_wid)
  ncvar_put(WDgrowth_tile_ncout,WDgrowth_tile_var,WDgrowth_tile_array)
  # Get a summary of the created file
  WDgrowth_tile_ncout
  # close the file, writing data to disk
  nc_close(WDgrowth_tile_ncout)
  return(WDgrowth_tile_var)
}

# C. cmort_tile ----
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output

cmort_tile_df <- function(data){
  cmort_tile <- data |>
    slice(595:nrow(data)) |> 
    mutate(year = 1:936, 
           cmort = WDmort+WDkill) |>
    select(year, cmort) 
  cmort_tile_wid <- cmort_tile |> select(-year)
  # create the netCDF filename 
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                          "/BiomeEP_CBudget_cmort_",name, ".nc", sep=""))
  # define dimensions
  time <- as.array(seq(1,935,1))
  time_dim <- ncdim_def("time","years",as.double(time)) 
  # define variables
  cmort_tile_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim),-999,
                                 "Carbon Mass Flux lost from wood",prec="single")
  # create netCDF file and put arrays
  cmort_tile_ncout <- nc_create(ncfname,list(cmort_tile_var),force_v4=TRUE)
  # put variables
  cmort_tile_array <- simplify2array(cmort_tile_wid)
  ncvar_put(cmort_tile_ncout,cmort_tile_var,cmort_tile_array)
  # Get a summary of the created file
  cmort_tile_ncout
  # close the file, writing data to disk
  nc_close(cmort_tile_ncout)
  return(cmort_tile_var)
}

# Pools ----

# 1. Carbon mass in vegetation by PFT ----
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output

#substr(deparse(substitute(BiomeE_P0_FIN_aCO2_annual_cohorts)), start = 8, stop = 18)
cveg_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  cveg <- data |> #BiomeE_P0_FIN_aCO2_annual_cohorts |> 
    group_by(PFT,year) |>
    summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) |> 
    filter(year>510) |>
    mutate(PFT=as.double(PFT)) |>
    mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
  if(site == "FIN") {
  cveg_wid <- cveg |> select(c(year,cveg,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) |> arrange(year) |>
    mutate(`4`=0,`5`=0,`6`=0,`7`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
  }
  if(site == "BIA") {
    cveg_wid <- cveg |> select(c(year,cveg,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
      pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) |> arrange(year) |>
      mutate(`1`=0,`5`=0,`6`=0,`7`=0) |> 
      relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
      relocate(`8`,.after =`7`) |> select(-year)
  }
  if(site == "BCI") {
    cveg_wid <- cveg |> select(c(year,cveg,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
      pivot_wider(names_from = PFT, values_from = cveg,values_fill = 0) |> arrange(year) |>
      mutate(`1`=0,`2`=0,`3`=0,`4`=0,) |> 
      relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
      relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
      relocate(`8`,.after =`7`) |> select(-year)
  }
  # create the netCDF filename 
  ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                          "/BiomeEP_cveg_",name, ".nc", sep=""))
  # define dimensions
  time <- as.array(seq(1,1020,1))
  pft <- as.array(seq(1,8,1))
  time_dim <- ncdim_def("time","years",as.double(time)) 
  pft_dim <- ncdim_def("pft","-",as.double(pft)) 
  # define variables
  cveg_var <- ncvar_def("cveg","kg C m-2",list(time_dim,pft_dim),-999,
                        "Carbon mass in vegetation by PFT",prec="single")
  # create netCDF file and put arrays
  cveg_ncout <- nc_create(ncfname,list(cveg_var),force_v4=TRUE)
  # put variables
  cveg_array <- simplify2array(cveg_wid)
  ncvar_put(cveg_ncout,cveg_var,cveg_array)
  # Get a summary of the created file
  cveg_ncout
  # close the file, writing data to disk
  nc_close(cveg_ncout)
  return(cveg_var)
}

# 2. Aboveground woody biomass ----
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# cohort output
AGcwood_df <- function(data){
AGcwood <- data |> 
  filter(PFT != 1) |>
  group_by(year) |>
  summarise(AGcwood=sum((sapwC+woodC)*0.75*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ungroup()
AGcwood_wid <- AGcwood |> select(-year)
# create the netCDF filename 
name <- substr(deparse(substitute(data)), start = 8, stop = 18)
ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
site <- substr(deparse(substitute(data)), start = 11, stop = 13)
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_AGcwood_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
AGcwood_var <- ncvar_def("AGcwood","kg C m-2",list(time_dim),-999,
                         "Aboveground woody biomass",prec="single")
# create netCDF file and put arrays
AGcwood_ncout <- nc_create(ncfname,list(AGcwood_var),force_v4=TRUE)
# put variables
AGcwood_array <- simplify2array(AGcwood_wid)
ncvar_put(AGcwood_ncout,AGcwood_var,AGcwood_array)
# Get a summary of the created file
AGcwood_ncout
# close the file, writing data to disk
nc_close(AGcwood_ncout)
return(AGcwood_var)
}

# 3. Carbon mass in wood by PFT ----
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
cwood <- data |> 
  filter(PFT != 1) |>
  group_by(PFT,year) |>
  summarise(cwood=sum((sapwC+woodC)*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.double(PFT)) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
cwood_wid <- cwood |> select(c(year,cwood,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  cwood_wid <- cwood |> select(c(year,cwood,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  cwood_wid <- cwood |> select(c(year,cwood,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = cwood,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_cwood_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cwood_var <- ncvar_def("cwood","kg C m-2",list(time_dim,pft_dim),-999,
                       "Carbon mass in wood by PFT",prec="single")
# create netCDF file and put arrays
cwood_ncout <- nc_create(ncfname,list(cwood_var),force_v4=TRUE)
# put variables
cwood_array <- simplify2array(cwood_wid)
ncvar_put(cwood_ncout,cwood_var,cwood_array)
# Get a summary of the created file
cwood_ncout
# close the file, writing data to disk
nc_close(cwood_ncout)
return(cwood_var)
}

# 4. Carbon mass in wood by size class ----
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  sim = substr(deparse(substitute(data)), start = 8, stop = 9)
  co2 = substr(deparse(substitute(data)), start = 15, stop = 18)
cwood_size <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) |> ungroup()
if(site == "FIN") {
  if(sim == "P0") {
  cwood_size_wid <- cwood_size |> 
    pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
    select(-year) |> 
    mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    cwood_size_wid <- cwood_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[100,150)`=0)
  }
}
if(site == "BIA") {
  if(sim == "P0") {
    cwood_size_wid <- cwood_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    cwood_size_wid <- cwood_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
  }
}
if(site == "BCI") {
  if(co2 == "aCO2") {
    cwood_size_wid <- cwood_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  if(co2 == "eCO2") {
    if(sim == "P0"|sim == "P1") {
      cwood_size_wid <- cwood_size |> 
        pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0)
    }
    else{
      cwood_size_wid <- cwood_size |> 
        pivot_wider(names_from = dbh_bins, values_from = cwood_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
    }
  }
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_cwood_size_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
sizeclass <- as.array(c(1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cwood_size_var <- ncvar_def("cwood_size","kg C m-2",list(time_dim,sizeclass_dim),-999,
                            "Carbon mass in wood by size class",prec="single")
# create netCDF file and put arrays
cwood_size_ncout <- nc_create(ncfname,list(cwood_size_var),force_v4=TRUE)
# put variables
cwood_size_array <- simplify2array(cwood_size_wid)
ncvar_put(cwood_size_ncout,cwood_size_var,cwood_size_array)
# Get a summary of the created file
cwood_size_ncout
# close the file, writing data to disk
nc_close(cwood_size_ncout)
return(cwood_size_var)
}

# 5. Stem number by size class ----
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_size_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  sim = substr(deparse(substitute(data)), start = 8, stop = 9)
  co2 = substr(deparse(substitute(data)), start = 15, stop = 18)
nstem_size <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  summarise(nstem_size=sum(density)) |> ungroup()
unique(nstem_size$dbh_bins)
length(unique(nstem_size$dbh_bins))
if(site == "FIN") {
  if(sim == "P0") {
 nstem_size_wid <- nstem_size |> 
  pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
  select(-year) |> 
  mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    nstem_size_wid <- nstem_size |> 
      pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[100,150)`=0)
  }
}
if(site == "BIA") {
  if(sim == "P0") {
  nstem_size_wid <- nstem_size |> 
    pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
    select(-year) |> 
    mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    nstem_size_wid <- nstem_size |> 
      pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
  }
}
if(site == "BCI") {
  if(co2 == "aCO2") {
    nstem_size_wid <- nstem_size |> 
      pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  if(co2 == "eCO2") {
   if(sim == "P0"|sim == "P1") {
    nstem_size_wid <- nstem_size |> 
      pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
   }
   else{
    nstem_size_wid <- nstem_size |> 
      pivot_wider(names_from = dbh_bins, values_from = nstem_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
  }
 }
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_nstem_size_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
sizeclass <- as.array(c(1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
nstem_size_var <- ncvar_def("nstem_size","count ha-1",list(time_dim,sizeclass_dim),-999,
                            "Stem number by size class",prec="single")
# create netCDF file and put arrays
nstem_size_ncout <- nc_create(ncfname,list(nstem_size_var),force_v4=TRUE)
# put variables
nstem_size_array <- simplify2array(nstem_size_wid)
ncvar_put(nstem_size_ncout,nstem_size_var,nstem_size_array)
# Get a summary of the created file
nstem_size_ncout
# close the file, writing data to disk
nc_close(nstem_size_ncout)
return(nstem_size_var)
}

# 6. Leaf area index ----
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
lai <- data |> 
  group_by(PFT,year) |>
  summarise(lai=sum(Aleaf*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.double(PFT)) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
lai_wid <- lai |> select(c(year,lai,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  lai_wid <- lai |> select(c(year,lai,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  lai_wid <- lai |> select(c(year,lai,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = lai,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_lai_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
lai_var <- ncvar_def("lai","m2 m-2",list(time_dim,pft_dim),-999,
                     "Leaf area index",prec="single")
# create netCDF file and put arrays
lai_ncout <- nc_create(ncfname,list(lai_var),force_v4=TRUE)
# put variables
lai_array <- simplify2array(lai_wid)
ncvar_put(lai_ncout,lai_var,lai_array)
# Get a summary of the created file
lai_ncout
# close the file, writing data to disk
nc_close(lai_ncout)
return(lai_var)
}

# 7. Crown area ----
# CA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
CA <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(CA=sum(Acrown*density)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
CA_wid <- CA |> select(c(year,CA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  CA_wid <- CA |> select(c(year,CA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  CA_wid <- CA |> select(c(year,CA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = CA,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_CA_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
CA_var <- ncvar_def("CA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Crown area",prec="single")
# create netCDF file and put arrays
CA_ncout <- nc_create(ncfname,list(CA_var),force_v4=TRUE)
# put variables
CA_array <- simplify2array(CA_wid)
ncvar_put(CA_ncout,CA_var,CA_array)
# Get a summary of the created file
CA_ncout
# close the file, writing data to disk
nc_close(CA_ncout)
return(CA_var)
}

# 8. Basal area ----
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
BA <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) |>
  summarise(BA=sum(BA*density)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
BA_wid <- BA |> select(c(year,BA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  BA_wid <- BA |> select(c(year,BA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  BA_wid <- BA |> select(c(year,BA,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = BA,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_BA_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BA_var <- ncvar_def("BA","m2 ha-1",list(time_dim,pft_dim),-999,
                    "Basal area",prec="single")
# create netCDF file and put arrays
BA_ncout <- nc_create(ncfname,list(BA_var),force_v4=TRUE)
# put variables
BA_array <- simplify2array(BA_wid)
ncvar_put(BA_ncout,BA_var,BA_array)
# Get a summary of the created file
BA_ncout
# close the file, writing data to disk
nc_close(BA_ncout)
return(BA_var)
}

# 9. 95th percentile of tree height ----
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
height <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(height=quantile(height, probs = 0.95)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
height_wid <- height |> select(c(year,height,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = height,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  height_wid <- height |> select(c(year,height,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = height,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  height_wid <- height |> select(c(year,height,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = height,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_height_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
height_var <- ncvar_def("height","m",list(time_dim,pft_dim),-999,
                        "95th percentile of tree height",prec="single")
# create netCDF file and put arrays
height_ncout <- nc_create(ncfname,list(height_var),force_v4=TRUE)
# put variables
height_array <- simplify2array(height_wid)
ncvar_put(height_ncout,height_var,height_array)
# Get a summary of the created file
height_ncout
# close the file, writing data to disk
nc_close(height_ncout)
return(height_var)
}

# Fluxes ----
# 10. Woody biomass growth ----
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  
  params_tile$f_initialBSW <- 0.2
  fecundity_growth <- data |>
    filter(PFT != 1) |>
    filter(layer == 1)  |>
    mutate(fecundity = params_tile$f_initialBSW*seedC*density/10000) |>
    select(cohort, year,cID, PFT, layer, fecundity)
  
WBgrowth <- data |> 
  filter(PFT != 1) |>
  left_join(fecundity_growth) |>
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(WBgrowth=sum(fwood*treeG*density/10000 + fecundity, na.rm = T)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()

if(site == "FIN") {
WBgrowth_wid <- WBgrowth |> select(c(year,WBgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  WBgrowth_wid <- WBgrowth |> select(c(year,WBgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  WBgrowth_wid <- WBgrowth |> select(c(year,WBgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = WBgrowth,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_WBgrowth_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
WBgrowth_var <- ncvar_def("WBgrowth","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Woody biomass growth",prec="single")
# create netCDF file and put arrays
WBgrowth_ncout <- nc_create(ncfname,list(WBgrowth_var),force_v4=TRUE)
# put variables
WBgrowth_array <- simplify2array(WBgrowth_wid)
ncvar_put(WBgrowth_ncout,WBgrowth_var,WBgrowth_array)
# Get a summary of the created file
WBgrowth_ncout
# close the file, writing data to disk
nc_close(WBgrowth_ncout)
return(WBgrowth_var)
}

# 11. Basal area growth ----
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
BAgrowth <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) |>
  #summarise(BAgrowth=sum(dBA*density)) |> 
  filter(year>510) |>
  mutate(year = year-510,
         BAgrowth = ifelse(BAgrowth >2, NA, BAgrowth)) |> 
  left_join(PFT_order) |> 
  ungroup() 
if(site == "FIN") {
BAgrowth_wid <- BAgrowth |> select(c(year,BAgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  BAgrowth_wid <- BAgrowth |> select(c(year,BAgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  BAgrowth_wid <- BAgrowth |> select(c(year,BAgrowth,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = BAgrowth,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_BAgrowth_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
BAgrowth_var <- ncvar_def("BAgrowth","m2 ha-1 yr-1",list(time_dim,pft_dim),-999,
                          "Basal area growth",prec="single")
# create netCDF file and put arrays
BAgrowth_ncout <- nc_create(ncfname,list(BAgrowth_var),force_v4=TRUE)
# put variables
BAgrowth_array <- simplify2array(BAgrowth_wid)
ncvar_put(BAgrowth_ncout,BAgrowth_var,BAgrowth_array)
# Get a summary of the created file
BAgrowth_ncout
# close the file, writing data to disk
nc_close(BAgrowth_ncout)
return(BAgrowth_var)
}

# 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  
kill_low_density <- data |>
    filter(PFT != 1) |>
    mutate(nindivs = density/10000) |>
    filter(nindivs <= 1.0E-6)  |> 
    mutate(cmort_low = (sapwC+woodC)*(1 - deathrate)*density/10000)  |>
    select(cohort, year,cID, PFT, layer,cmort_low)
  
cmort <- data |> 
  filter(PFT != 1) |>
  left_join(kill_low_density) |>
  mutate(cmort_low = ifelse(is.na(cmort_low),0,cmort_low)) |> 
  group_by(PFT,year) |>
  reframe(cmort=sum((sapwC+woodC)*deathrate*density/10000 + cmort_low)) |>
  filter(year>510) |>
  mutate(PFT=as.double(PFT)) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()

if(site == "FIN") {
cmort_wid <- cmort |> select(c(year,cmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  cmort_wid <- cmort |> select(c(year,cmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  cmort_wid <- cmort |> select(c(year,cmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = cmort,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_cmort_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
cmort_var <- ncvar_def("cmort","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_ncout <- nc_create(ncfname,list(cmort_var),force_v4=TRUE)
# put variables
cmort_array <- simplify2array(cmort_wid)
ncvar_put(cmort_ncout,cmort_var,cmort_array)
# Get a summary of the created file
cmort_ncout
# close the file, writing data to disk
nc_close(cmort_ncout)
return(cmort_var)
}

# cmort by sizeclass
cmort_size_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  sim = substr(deparse(substitute(data)), start = 8, stop = 9)
  co2 = substr(deparse(substitute(data)), start = 15, stop = 18)
  
  kill_low_density <- data |>
    filter(PFT != 1) |>
    mutate(nindivs = density/10000) |>
    filter(nindivs <= 1.0E-6)  |> 
    mutate(cmort_low = (sapwC+woodC)*(1 - deathrate)*density/10000)  |>
    select(cohort, year,cID, PFT, layer,cmort_low)
  
cmort_size <- data |> 
  filter(PFT != 1) |>
  left_join(kill_low_density) |>
  mutate(cmort_low = ifelse(is.na(cmort_low),0,cmort_low)) |> 
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  #summarise(cmort_size=sum(c_deadtrees)) |> 
  summarise(cmort_size=sum((sapwC+woodC)*deathrate*density/10000 + cmort_low)) |> 
  ungroup()

if(site == "FIN") {
  if(sim == "P0") {
  cmort_size_wid <- cmort_size |> 
    pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
    select(-year) |> 
    mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    cmort_size_wid <- cmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[100,150)`=0)
  }
}
if(site == "BIA") {
  if(sim == "P0") {
    cmort_size_wid <- cmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    cmort_size_wid <- cmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
  }
}
if(site == "BCI") {
  if(co2 == "aCO2") {
    cmort_size_wid <- cmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  if(co2 == "eCO2") {
    if(sim == "P0"|sim == "P1") {
      cmort_size_wid <- cmort_size |> 
        pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0)
    }
    else{
      cmort_size_wid <- cmort_size |> 
        pivot_wider(names_from = dbh_bins, values_from = cmort_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
    }
  }
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_cmort_size_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
sizeclass <- as.array(c(1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
cmort_size_var <- ncvar_def("cmort_size","kg C m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                       "Carbon Mass Flux lost from live wood due to mortality",prec="single")
# create netCDF file and put arrays
cmort_size_ncout <- nc_create(ncfname,list(cmort_size_var),force_v4=TRUE)
# put variables
cmort_size_array <- simplify2array(cmort_size_wid)
ncvar_put(cmort_size_ncout,cmort_size_var,cmort_size_array)
# Get a summary of the created file
cmort_size_ncout
# close the file, writing data to disk
nc_close(cmort_size_ncout)
return(cmort_size_var)
}

# 13. Stem number Flux lost from vegetation due to mortality or other turnover process ----
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, (pft)
# cohort output
stemmort_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
stemmort <- data |> 
  filter(PFT != 1) |>
  group_by(PFT,year) |>
  #summarise(stemmort=sum(n_deadtrees)) |> 
  summarise(stemmort=sum(deathrate*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.double(PFT)) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
stemmort_wid <- stemmort |> select(c(year,stemmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  stemmort_wid <- stemmort |> select(c(year,stemmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  stemmort_wid <- stemmort |> select(c(year,stemmort,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = stemmort,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0,`8`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_stemmort_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
stemmort_var <- ncvar_def("stemmort","Count m-2 yr-1",list(time_dim,pft_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_ncout <- nc_create(ncfname,list(stemmort_var),force_v4=TRUE)
# put variables
stemmort_array <- simplify2array(stemmort_wid)
ncvar_put(stemmort_ncout,stemmort_var,stemmort_array)
# Get a summary of the created file
stemmort_ncout
# close the file, writing data to disk
nc_close(stemmort_ncout)
return(stemmort_var)
}

# stemmort by sizeclass
stemmort_size_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
  sim = substr(deparse(substitute(data)), start = 8, stop = 9)
  co2 = substr(deparse(substitute(data)), start = 15, stop = 18)
stemmort_size <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  summarise(stemmort_size=sum(deathrate*density/10000)) |> ungroup()
if(site == "FIN") {
  if(sim == "P0") {
  stemmort_size_wid <- stemmort_size |> 
    pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
    select(-year) |> 
    mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    stemmort_size_wid <- stemmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[100,150)`=0)
  }
}
if(site == "BIA") {
  if(sim == "P0") {
    stemmort_size_wid <- stemmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  else{
    stemmort_size_wid <- stemmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
  }
}
if(site == "BCI") {
  if(co2 == "aCO2") {
    stemmort_size_wid <- stemmort_size |> 
      pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
      select(-year) |> 
      mutate(`[150,200)`=0,`[200,250)`=0)
  }
  if(co2 == "eCO2") {
    if(sim == "P0"|sim == "P1") {
      stemmort_size_wid <- stemmort_size |> 
        pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0)
    }
    else{
      stemmort_size_wid <- stemmort_size |> 
        pivot_wider(names_from = dbh_bins, values_from = stemmort_size,values_fill = 0) |> arrange(year) |>
        select(-year) |> 
        mutate(`[150,200)`=0,`[200,250)`=0,`[90,100)`=0)
    }
  }
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_stemmort_size_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
sizeclass <- as.array(c(1,5,10,15,20,30,40,50,60,70,80,90,100,150,200,250))
time_dim <- ncdim_def("time","years",as.double(time)) 
sizeclass_dim <- ncdim_def("sizeclass","cm",as.double(sizeclass)) 
# define variables
stemmort_size_var <- ncvar_def("stemmort_size","Count m-2 yr-1",list(time_dim,sizeclass_dim),-999,
                          "Stem number Flux lost from vegetation due to mortality",prec="single")
# create netCDF file and put arrays
stemmort_size_ncout <- nc_create(ncfname,list(stemmort_size_var),force_v4=TRUE)
# put variables
stemmort_size_array <- simplify2array(stemmort_size_wid)
ncvar_put(stemmort_size_ncout,stemmort_size_var,stemmort_size_array)
# Get a summary of the created file
stemmort_size_ncout
# close the file, writing data to disk
nc_close(stemmort_size_ncout)
return(stemmort_size_var)
}

# 14. Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ----
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
gpp <- data |> 
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(gpp=sum(GPP*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
gpp_wid <- gpp |> select(c(year,gpp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  gpp_wid <- gpp |> select(c(year,gpp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  gpp_wid <- gpp |> select(c(year,gpp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = gpp,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_gpp_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
gpp_var <- ncvar_def("gpp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Gross Primary Production",prec="single")
# create netCDF file and put arrays
gpp_ncout <- nc_create(ncfname,list(gpp_var),force_v4=TRUE)
# put variables
gpp_array <- simplify2array(gpp_wid)
ncvar_put(gpp_ncout,gpp_var,gpp_array)
# Get a summary of the created file
gpp_ncout
# close the file, writing data to disk
nc_close(gpp_ncout)
return(gpp_var)
}

# 15. Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ----
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp_df <- function(data){
  name <- substr(deparse(substitute(data)), start = 8, stop = 18)
  ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
  site <- substr(deparse(substitute(data)), start = 11, stop = 13)
npp <- data |> 
  mutate(PFT=as.double(PFT)) |>
  group_by(PFT,year) |>
  summarise(npp=sum(NPP*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |> left_join(PFT_order) |> ungroup()
if(site == "FIN") {
npp_wid <- npp |> select(c(year,npp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
  pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) |> arrange(year) |>
  mutate(`4`=0,`5`=0,`6`=0,`7`=0) |> 
  relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
  relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BIA") {
  npp_wid <- npp |> select(c(year,npp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`5`=0,`6`=0,`7`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`8`,.after =`7`) |> select(-year)
}
if(site == "BCI") {
  npp_wid <- npp |> select(c(year,npp,PFT_reorder)) |> rename(PFT=PFT_reorder) |>
    pivot_wider(names_from = PFT, values_from = npp,values_fill = 0) |> arrange(year) |>
    mutate(`1`=0,`2`=0,`3`=0,`4`=0) |> 
    relocate(`1`,.after =year) |> relocate(`2`,.after =`1`) |> relocate(`3`,.after =`2`) |>
    relocate(`4`,.after =`3`) |> relocate(`5`,.after =`4`) |> relocate(`6`,.after =`5`) |> 
    relocate(`8`,.after =`7`) |> select(-year)
}
# create the netCDF filename 
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_npp_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
pft <- as.array(seq(1,8,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
pft_dim <- ncdim_def("pft","-",as.double(pft)) 
# define variables
npp_var <- ncvar_def("npp","kg C m-2 yr-1",list(time_dim,pft_dim),-999,
                     "Net Primary Production",prec="single")
# create netCDF file and put arrays
npp_ncout <- nc_create(ncfname,list(npp_var),force_v4=TRUE)
# put variables
npp_array <- simplify2array(npp_wid)
ncvar_put(npp_ncout,npp_var,npp_array)
# Get a summary of the created file
npp_ncout
# close the file, writing data to disk
nc_close(npp_ncout)
return(npp_var)
}

# 16. Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ----
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# nbp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# tile output
nbp_df <- function(data){
nbp <- data |>
  slice(510+1:nrow(data)) |> 
  mutate(year = 1:1020, nbp = GPP-Rauto-Rh) |>
  select(year, nbp) 
nbp_wid <- nbp |> select(-year)
# create the netCDF filename 
name <- substr(deparse(substitute(data)), start = 8, stop = 18)
ppm <- substr(deparse(substitute(data)), start = 15, stop = 18)
site <- substr(deparse(substitute(data)), start = 11, stop = 13)
ncfname <- paste(paste0(here::here(),"/data/outputs_mod/nc_files/",ppm,"/",site, 
                        "/BiomeEP_nbp_",name, ".nc", sep=""))
# define dimensions
time <- as.array(seq(1,1020,1))
time_dim <- ncdim_def("time","years",as.double(time)) 
# define variables
nbp_var <- ncvar_def("nbp","kg C m-2 yr-1",list(time_dim),-999,
                     "Net Biospheric Production",prec="single")
# create netCDF file and put arrays
nbp_ncout <- nc_create(ncfname,list(nbp_var),force_v4=TRUE)
# put variables
nbp_array <- simplify2array(nbp_wid)
ncvar_put(nbp_ncout,nbp_var,nbp_array)
# Get a summary of the created file
nbp_ncout
# close the file, writing data to disk
nc_close(nbp_ncout)
return(nbp_var)
}
