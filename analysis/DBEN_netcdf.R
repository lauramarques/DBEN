# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ncdf4)
library(ncdf4.helpers)
library(tidyverse)


# Read example netCDF file ####

# open a netCDF file
example_netcdf <- nc_open("/home/laura/Documents/Collaborations/DBEN/example_output_for_netcdf_formatting/LPJGUESS_BA_S1_FIN2_fullcolumn.nc")
example_netcdf
attributes(example_netcdf$var)
attributes(example_netcdf$dim)
# get dimensions
Time <- ncvar_get(example_netcdf, "Time")
pft <- ncvar_get(example_netcdf, "pft")
# get variables
ba_var <- ncvar_get(example_netcdf,"BA")
dlname <- ncatt_get(example_netcdf,"BA","long_name")
dunits <- ncatt_get(example_netcdf,"BA","units")
fillvalue <- ncatt_get(example_netcdf,"BA","_FillValue")

# open a netCDF file
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_BAgrowth_P0_BIA_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_nbp_P0_BIA_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cwood_size_P0_FIN_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cveg_P0_FIN_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_stemmort_pft_P0_FIN_412ppm.nc")
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_WBgrowth_P0_FIN_412ppm.nc")
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
time <- ncvar_get(var1_netcdf, "time")
sizeclass <- ncvar_get(var1_netcdf, "sizeclass")
pft <- ncvar_get(var1_netcdf, "pft")
# get variables
dvar <- ncvar_get(var1_netcdf,"WBgrowth")
dlname <- ncatt_get(var1_netcdf,"WBgrowth","long_name")
dunits <- ncatt_get(var1_netcdf,"WBgrowth","units")
fillvalue <- ncatt_get(var1_netcdf,"WBgrowth","_FillValue")

# read WBgrowth for pft
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_WBgrowth_P0_FIN_412ppm.nc")
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "pft")
# get variables
dvar <- ncvar_get(var1_netcdf,"WBgrowth")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_WBgrowth <- dvar %>% mutate(total=V1+V2+V3) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_WBgrowth)
dvar_WBgrowth %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# read cmort for pft
var1_netcdf <- nc_open("/home/laura/DBEN/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cmort_pft_P0_FIN_412ppm.nc")
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "pft")
# get variables
dvar <- ncvar_get(var1_netcdf,"cmort")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cmort <- dvar %>% mutate(total=V1+V2+V3) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cmort)
dvar_cmort %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# plot carbon balance
CB <- data.frame(dvar_WBgrowth$year, dvar_WBgrowth$total, dvar_cmort$total) %>%
  rename(year=dvar_WBgrowth.year, WBgrowth=dvar_WBgrowth.total, cmort=dvar_cmort.total)
CB %>%
  ggplot() + 
  geom_line(aes(x=year, y=WBgrowth-cmort)) 
