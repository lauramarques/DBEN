# This script reads and tests the outputs as netcdf files

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
example_netcdf <- nc_open(paste0(here::here(), "/data/example_output_for_netcdf_formatting/LPJGUESS_BA_S1_FIN2_fullcolumn.nc"))
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

# open a netCDF file  ####

# read WBgrowth for pft
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_WBgrowth_P0_FIN_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_WBgrowth_P0_BIA_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BCI/BiomeEP_WBgrowth_P0_BCI_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BCI/BiomeEP_WBgrowth_P0_BCI_412ppm.nc"))

var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "pft")
# get variables
dvar <- ncvar_get(var1_netcdf,"cveg")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_WBgrowth <- dvar %>% mutate(total = rowSums(.[1:8])) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_WBgrowth)
dvar_WBgrowth %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# read cmort for pft
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cmort_pft_P0_FIN_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_cmort_pft_P0_BIA_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BCI/BiomeEP_cmort_pft_P0_BCI_412ppm.nc"))
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
dvar_cmort <- dvar %>% mutate(total = rowSums(.[1:8])) %>%
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

# check cmort pft and sizeclass for FIN at 562 ppm  ####
# read cmort for pft
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/FIN/BiomeEP_cmort_pft_PS4_FIN_562ppm.nc"))
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
dvar_cmort <- dvar %>% # mutate(total=V1+V2+V3) %>%
  mutate(total=rowSums(across(where(is.numeric))))  %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cmort)
dvar_cmort %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# read cmort for size class
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/FIN/BiomeEP_cmort_size_P0_FIN_562ppm.nc"))
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "sizeclass")
# get variables
dvar <- ncvar_get(var1_netcdf,"cmort")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cmort <- dvar %>% mutate(total=rowSums(across(where(is.numeric)))) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cmort)
dvar_cmort %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# check turnover 412 vs. 562 ppm  ####

## 412 ppm ####

# read cwood per size
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/aCO2/FIN/BiomeEP_nstem_size_P0_FIN_aCO2.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_cwood_size_P0_BIA_412ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BCI/BiomeEP_cwood_size_P0_BCI_412ppm.nc"))
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "sizeclass")
# get variables
dvar <- ncvar_get(var1_netcdf,"nstem_size")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cwood_size <- dvar %>% mutate(total = rowSums(.[1:16])) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cwood_size)
eq_cwood_size_ambient = dvar_cwood_size %>% filter(year>=390) %>%
  summarise(mean=mean(total))
eq_cwood_size_ambient

dvar_cwood_size %>%
  ggplot() + 
  geom_line(aes(x=year, y=total)) 

# read cmort
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/FIN/BiomeEP_cmort_pft_P0_FIN_412ppm.nc"))
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BIA/BiomeEP_cmort_pft_P0_BIA_412ppm.nc"))
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/412ppm/BCI/BiomeEP_cmort_pft_P0_BCI_412ppm.nc"))
var2_netcdf
attributes(var2_netcdf$var)
attributes(var2_netcdf$dim)
# get dimensions
ncvar_get(var2_netcdf, "time")
ncvar_get(var2_netcdf, "pft")
# get variables
dvar <- ncvar_get(var2_netcdf,"cmort")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cmort <- dvar %>% mutate(total = rowSums(.[1:8])) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cmort)
eq_cmort_ambient = dvar_cmort %>% filter(year>=390) %>%
  summarise(mean=mean(total))
eq_cmort_ambient
residence_time_ambient = eq_cwood_size_ambient/eq_cmort_ambient 
residence_time_ambient

## 562 ppm ####

# read cwood per size
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/FIN/BiomeEP_cwood_size_P0_FIN_562ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/BIA/BiomeEP_cwood_size_P0_BIA_562ppm.nc"))
var1_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/BCI/BiomeEP_cwood_size_P0_BCI_562ppm.nc"))
var1_netcdf
attributes(var1_netcdf$var)
attributes(var1_netcdf$dim)
# get dimensions
ncvar_get(var1_netcdf, "time")
ncvar_get(var1_netcdf, "sizeclass")
# get variables
dvar <- ncvar_get(var1_netcdf,"cwood_size")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cwood_size <- dvar %>% mutate(total = rowSums(.[1:16])) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cwood_size)
eq_cwood_size_ambient = dvar_cwood_size %>% filter(year>=390) %>%
  summarise(mean=mean(total))
eq_cwood_size_ambient

# read cmort
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/FIN/BiomeEP_cmort_pft_P0_FIN_562ppm.nc"))
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/BIA/BiomeEP_cmort_pft_P0_BIA_562ppm.nc"))
var2_netcdf <- nc_open(paste0(here::here(), "/data/outputs_mod/nc_files/562ppm/BCI/BiomeEP_cmort_pft_P0_BCI_562ppm.nc"))
var2_netcdf
attributes(var2_netcdf$var)
attributes(var2_netcdf$dim)
# get dimensions
ncvar_get(var2_netcdf, "time")
ncvar_get(var2_netcdf, "pft")
# get variables
dvar <- ncvar_get(var2_netcdf,"cmort")
str(dvar)
dvar <- as.data.frame(dvar)
dvar_cmort <- dvar %>% mutate(total = rowSums(.[1:8])) %>%
  rownames_to_column("year") %>% mutate(year=as.numeric(year)) 
str(dvar_cmort)
eq_cmort_ambient = dvar_cmort %>% filter(year>=390) %>%
  summarise(mean=mean(total))
eq_cmort_ambient
residence_time_ambient = eq_cwood_size_ambient/eq_cmort_ambient 
residence_time_ambient
