# This script runs the simulations for the DBEN project
# P0: Baseline run (P0 spinup + 30 + clear cut + 420)
# PS1-PS6: Sensitivity runs with an increase in disturbance rate. 
# We implemented the subroutine reset_vegn_initial and call it with a frequency of 100,75,50,25,15,10 yrs (see biosphere.biomee.mod.f90)

# load packages
library(rsofun)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyverse)

# Define drivers ---- 

# Lon 23.75°, Lat 52.75° Temperate: Bialowieza, Poland (BIA): 

sitename <- "BIA"

site_info <- tibble(
  sitename="BIA",
  lon = 23.75,
  lat = 52.75,
  elv = NA,
  year_start = 1991,
  year_end = 2020,
  classid = NA,
  c4 = FALSE,
  whc = NA,
  koeppen_code = NA,
  igbp_land_use = "Mixed Forests",
  plant_functional_type = "Broadleaf trees"
)

site_info <- site_info %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_siml <- tibble(
  spinup                = TRUE,
  spinupyears           = 510, 
  recycle               = 30,  
  firstyeartrend        = 0, 
  nyeartrend            = 1020, #450
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  do_reset_veg          = TRUE, # TRUE
  dist_frequency        = 10, # 0, 100, 75, 50, 25, 15, 10
  method_photosynth     = "pmodel",
  method_mortality      = "dbh"
)

params_tile <- tibble(
  soiltype     = 3,     
  FLDCAP       = 0.4,   
  WILTPT       = 0.05,  
  K1           = 2.0,   
  K2           = 0.05,  
  K_nitrogen   = 2.4, 
  MLmixRatio   = 0.8, 
  etaN         = 0.0, 
  LMAmin       = 0.02,
  fsc_fine     = 1.0,   
  fsc_wood     = 0.0,   
  GR_factor    = 0.33,  
  l_fract      = 0.0,   
  retransN     = 0.0,   
  f_initialBSW = 0.2,
  f_N_add      = 0.02,  
  # add calibratable params
  tf_base        = 1,
  par_mort       = 0.15,
  par_mort_under = 1
)

# In reverse order from the params:
# Picea abies - shade tolerant needleleaf (PFT2)
# Betula pendula - shade intolerant broadleaf deciduous (PFT3)
# Carpinus betulus/Tilia cordata - shade tolerant broadleaf deciduous (PFT4)
# Grasses combined (PFT8) C3

# Shade tolerant-low Vcmax, low resp. rate and low mortality rate
# Shade intolerant-high Vcmax, high resp. rate and high mortality rate

params_species <- tibble(
  # species         0 1 2 3 4    ...
  lifeform      = c(9999,0,1,1,1,rep(1,11)),        
  phenotype     = c(9999,0,0,0,1,rep(1,11)),        
  pt            = rep(0,16),                      
  # Root parameters
  alpha_FR      = rep(1.2,16),                    
  rho_FR        = rep(200,16),                    
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),               
  leaf_size     = rep(0.04,16), 
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),               
  Vannual       = rep(1.2,16),                   
  wet_leaf_dreg = rep(0.3,16),                   
  m_cond        = c(9999,7.0,9.0,9.0,9.0,rep(9.0,11)),                 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),                
  gamma_SW      = rep(0.08,16),                 
  gamma_FR      = rep(12.0,16),                 
  tc_crit       = c(9999,12,15,15,0,rep(0,11)), 
  tc_crit_on    = c(9999,10,12,12,0,rep(15,11)),
  gdd_crit      = c(9999,80,120,120,0,rep(0,11)),
  betaON        = c(9999,0.2,0.2,0.2,0.2,rep(0.2,11)),     
  betaOFF       = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),
  # Allometry parameters
  alphaHT       = rep(36,16),                   
  thetaHT       = rep(0.5,16),                   
  alphaCA       = rep(150,16),                   
  thetaCA       = rep(1.5,16),                   
  alphaBM       = rep(5200,16),                   
  thetaBM       = rep(2.5,16), 
  # Reproduction parameters
  seedlingsize  = c(9999,0.01,0.05,0.05,0.05,rep(0.05,11)),  # c(9999,0.005,0.02,0.02,0.06,rep(0.05,11)), 
  maturalage    = c(9999,0,5,5,5,rep(5,11)),   
  v_seed        = c(9999,0.1,0.1,0.1,0.1,rep(0.1,11)),  # c(9999,0.4,0.1,0.1,0.1,rep(0.1,11)),   
  # Mortality parameters
  mortrate_d_c  = c(9999,0.05,0.03,0.02,0.01,rep(0.02,11)),  
  mortrate_d_u  = rep(0.075,16), 
  # Leaf parameters
  LMA           = c(9999,0.025,0.025,0.025,0.14,rep(0.14,11)), 
  leafLS        = rep(1,16), 
  LNbase        = c(9999,1.0E-3,0.8E-3,0.7E-3,0.4E-3,rep(0.5E-3,11)),  
  CNleafsupport = rep(80,16),
  rho_wood      = c(9999,120,320,350,300,rep(300,11)), #c(9999,80,300,320,350,rep(300,11)),  #BiomeEP #c(9999,120,350,350,300,rep(300,11)), 
  taperfactor   = rep(0.75,16),
  lAImax        = c(9999,2.0,3.2,3.2,3.5,rep(3.5,11)), # biomeEP: c(9999,2.0,3.0,3.0,3.5,rep(3.5,11)), 
  tauNSC        = rep(3,16), 
  fNSNmax       = rep(5,16),                      
  phiCSA        = rep(0.25E-4,16),  
  # C/N ratios for plant pools
  CNleaf0      = rep(25,16),  
  CNsw0        = rep(350,16),  
  CNwood0      = rep(350,16),  
  CNroot0      = rep(40,16),  
  CNseed0      = rep(20,16),  
  Nfixrate0     = rep(0,16),   
  NfixCost0     = rep(0,16),
  internal_gap_frac      = rep(0.1,16),
  # calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),  # c(9999,1.0,1.0,1.1,1.5,rep(1.2,11)), 
  LAI_light     = rep(3.5,16)   
) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790),
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), 
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_n_cohorts = 4,   # number of PFTs
  init_cohort_species = seq(1, 10, 1),    # indicates sps (e.g. 1 - Fagus sylvatica)
  init_cohort_nindivs = rep(0.008, 10),  # initial individual density, individual/m2 ! 1 indiv/m2 = 10.000 indiv/ha #c(2.0,rep(.05, 9)),
  init_cohort_bl      = rep(0.0, 10),   # initial biomass of leaves, kg C/individual
  init_cohort_br      = rep(0.0, 10),  # initial biomass of fine roots, kg C/individual
  init_cohort_bsw     = rep(0.2, 10),  # initial biomass of sapwood, kg C/individual #c(0.01,rep(0.2, 9)),
  init_cohort_bHW     = rep(0.0, 10),  # initial biomass of heartwood, kg C/tree
  init_cohort_seedC   = rep(0.0, 10),  # initial biomass of seeds, kg C/individual
  init_cohort_nsc     = rep(0.5, 10)   # initial non-structural biomass #c(0.04,rep(0.5, 9))
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,      # BiomeE: 0.5,  
  init_slow_soil_C    = 0.0,       # BiomeE: 40,
  init_Nmineral       = 0.015,  # BiomeE: 50.0E-3
  N_input             = 0.0008    # BiomeE: 11.E-3
)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

# Define forcing data ----
#biomee_forcing_BIA <- read.csv(paste0(here::here(), "/data/inputs/biomee_forcing_BIA.csv"))
biomee_forcing_BIA <- read.csv("/home/laura/DBEN/data/inputs/biomee_forcing_BIA.csv")
biomee_forcing_BIA
df_forcing <- biomee_forcing_BIA

# Define CO2 ----
df_forcing$co2 <- 562 # CO2 levels: 412 ppm and 562 ppm

# Repeat mean seasonal cycle `nyears` times # Add harvest forcing to drivers. 
nyears <- params_siml$nyeartrend/length(unique(biomee_forcing_BIA$year))
df_forcing <- df_forcing |> 
  slice(rep(1:n(), nyears)) |> 
  rename(yearID=year) |>
  mutate(year = rep(1:1020, each = 365),
         hour = 11.5,
         harv = 0, nox = 0, nhy = 0) |> 
  relocate(year, .after=yearID) 

## for versions above 4.0
df_drivers <-tibble(sitename = site_info$sitename,
                    site_info = list(tibble(site_info)),
                    params_siml = list(tibble(params_siml)),
                    params_tile = list(tibble(params_tile)),
                    params_species=list(tibble(params_species)),
                    params_soil=list(tibble(params_soil)),
                    init_cohort=list(tibble(init_cohort)),
                    init_soil=list(tibble(init_soil)),
                    forcing=list(tibble(df_forcing)),
                    .name_repair = "unique")

# Run the model ----
out <- runread_biomee_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

biomee_annual_tile <- out$data[[1]]$output_annual_tile
biomee_annual_cohorts <- out$data[[1]]$output_annual_cohorts

biomee_annual_tile <- read.csv("/home/laura/DBEN/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv")
biomee_annual_cohorts <- read.csv("/home/laura/DBEN/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv")

# Figures ----
source("/home/laura/DBEN/analysis/03_DBEN_figures.R")

## Plant C (Biomass) ----
plantC_tile_fig(biomee_annual_tile)

## GPP ----
GPP_tile_fig(biomee_annual_tile)

## Rauto ----
Rauto_tile_fig(biomee_annual_tile)

## Soil C ----
soilC_tile_fig(biomee_annual_tile)

## Woody biomass growth
WDgrow_tile_fig(biomee_annual_tile)

biomee_annual_tile |> 
  mutate(WDgrow = ifelse(WDgrow <0, 0, WDgrow)) |>
  ggplot() + 
  geom_line(aes(x=year, y=WDgrow),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Woody biomass growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 

## Mortality
WDmort_tile_fig(biomee_annual_tile)

biomee_annual_tile |>  
  mutate(WDmort = ifelse(WDmort <0, 0, WDmort)) |>
  mutate(WDmort = ifelse(WDmort >10, 0, WDmort)) |>
  ggplot() + 
  geom_line(aes(x=year, y=WDmort),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 

gpp_fig(biomee_annual_cohorts)
BAgrowth_fig(biomee_annual_cohorts)



## Carbon balance
CBal_tile_fig(biomee_annual_tile)

## Carbon budget closure
CBud_tile_fig(biomee_annual_tile)

biomee_annual_tile |> 
  filter(year > 560) |>
  mutate(WDgrow = ifelse(WDgrow <0, 0, WDgrow),
         WDmort = ifelse(WDmort <0, 0, WDmort),
         Carbon_balance=WDgrow-WDmort,
         cwood = SapwoodC+WoodC,
         #sumCB = ifelse(row_number() == 1, cwood, WDgrow-WDmort-WDkill),
         sumCB = ifelse(row_number() == 1, cwood, WDgrow+WDrepr-WDmort-WDkill),
         cumsumCB = cumsum(sumCB)) |>
  ggplot() + 
  geom_line(aes(x=year, y=cumsumCB),col="#377EB8",size =2) + 
  geom_line(aes(x=year, y=cwood),col="purple",size=1) +
  labs(x = "year", y = "Carbon budget") + 
  scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(limits = c(542,900)) +
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 

# Carbon balance - cohort
CBal_cohort_fig(biomee_annual_cohorts)

## Carbon budget closure - cohort
CBud_cohort_fig(biomee_annual_cohorts)  +
  scale_x_continuous(limits = c(542,900))

## Pools: ----

## 1. Carbon mass in vegetation by PFT ----
cveg_fig(biomee_annual_cohorts)

## 2. Aboveground woody biomass ----
AGcwood_fig(biomee_annual_cohorts)

## 3. Carbon mass in wood by PFT ----
cwood_fig(biomee_annual_cohorts)

cwood_all_fig(biomee_annual_cohorts)

## 4. Carbon mass in wood by size class ----
cwood_size_fig(biomee_annual_cohorts)

## 5. Stem number by size class ----
nstem_fig(biomee_annual_cohorts)

## 6. Leaf area index ----
lai_fig(biomee_annual_cohorts)

## 7. Crown area ----
CA_fig(biomee_annual_cohorts)

## 8. Basal area ----
BA_fig(biomee_annual_cohorts)

## 9. 95th percentile of tree height ----
height_fig(biomee_annual_cohorts)

## Fluxes: ----
## 10. Woody biomass growth ----
WBgrowth_fig(biomee_annual_cohorts)

## 11. Basal area growth ----
BAgrowth_fig(biomee_annual_cohorts)

## 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
cmort_fig(biomee_annual_cohorts)

## Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
cmort_size_fig(biomee_annual_cohorts)

## 13. Stem number Flux lost from vegetation due to mortality or other turnover process ----
stemmort_fig(biomee_annual_cohorts)

## 14. GPP - Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ----
gpp_fig(biomee_annual_cohorts)

## 15. NPP - Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ----
npp_fig(biomee_annual_cohorts)

## 16. NBP - Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ----
nbp_fig(biomee_annual_tile)

# Outputs 412 ppm ----
### P0 ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_P0_BIA_aCO2_annual_cohorts.csv"))

### PS-1 (0.01 or nfrequency=100) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS1_BIA_aCO2_annual_cohorts.csv"))

### PS-2 (0.02 or nfrequency=75) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS2_BIA_aCO2_annual_cohorts.csv"))

### PS-3 (0.04 or nfrequency=50) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS3_BIA_aCO2_annual_cohorts.csv"))

### PS-4 (0.08 or nfrequency=25) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS4_BIA_aCO2_annual_cohorts.csv"))

### PS-5 (0.20 or nfrequency=15) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS5_BIA_aCO2_annual_cohorts.csv"))

### PS-6 (0.40 or nfrequency=10) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/412ppm/BiomeE_PS6_BIA_aCO2_annual_cohorts.csv"))

# Outputs 562 ppm ----
### P0 ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_P0_BIA_eCO2_annual_cohorts.csv"))

### PS-1 (0.01 or nfrequency=100) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS1_BIA_eCO2_annual_cohorts.csv"))

### PS-2 (0.02 or nfrequency=75) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS2_BIA_eCO2_annual_cohorts.csv"))

### PS-3 (0.04 or nfrequency=50) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS3_BIA_eCO2_annual_cohorts.csv"))

### PS-4 (0.08 or nfrequency=25) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS4_BIA_eCO2_annual_cohorts.csv"))

### PS-5 (0.20 or nfrequency=15) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS5_BIA_eCO2_annual_cohorts.csv"))

### PS-6 (0.40 or nfrequency=10) ----
write.csv(out$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_annual_tile.csv"))
write.csv(out$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/562ppm/BiomeE_PS6_BIA_eCO2_annual_cohorts.csv"))
