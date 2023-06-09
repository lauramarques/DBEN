library(rsofun)
library(dplyr)
library(ggplot2)
library(patchwork)
library(multidplyr)

# Define drivers #### 
#biomee_p_model_drivers$params_siml[[1]]

sitename <- "FIN"

site_info <- tibble(
  sitename="FIN",
  lon = 23.25,
  lat = 62.25,
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
  nyeartrend            = 450,
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  method_photosynth     = "pmodel",
  method_mortality      = "dbh"
)

params_tile <- tibble(
  soiltype     = 3,     # Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
  FLDCAP       = 0.4,   # soil property: field capacity 
  WILTPT       = 0.05,  # soil property: wilting point
  K1           = 2.0,   # turnover rate of fast SOM per year
  K2           = 0.05,  # turnover rate of slow SOM per year
  K_nitrogen   = 2.4, #2.4,   # mineral Nitrogen turnover rate !8.0, ! 2.4,
  MLmixRatio   = 0.8,   # the ratio of C and N returned to litters from microbes
  etaN         = 0.0,   # loss rate with runoff ! 0.025
  LMAmin       = 0.02,  # minimum LMA, boundary condition
  fsc_fine     = 1.0,   # fraction of fast turnover carbon in fine biomass
  fsc_wood     = 0.0,   # fraction of fast turnover carbon in wood biomass
  GR_factor    = 0.33,  # growth respiration factor
  l_fract      = 0.0,   # fraction of the carbon retained after leaf drop
  retransN     = 0.0,   # retranslocation coefficient of Nitrogen
  f_initialBSW = 0.2,
  f_N_add      = 0.02,   # re-fill of N for sapwood
  # add calibratable params
  tf_base        = 1,
  par_mort       = 0.1,    # param_dbh=1 param_csv=1 param_gr=1 CAI_MAX=2
  par_mort_under = 1
)

# Run site simulations
# Lon 23.25°, Lat 62.25° Boreal: Finland (FIN): 
# Pinus sylvestris - shade intolerant needleleaf (PFT1)
# Picea abies - shade tolerant needleleaf (PFT2)
# Betula pendula - shade intolerant broadleaf deciduous (PFT3)
# Grasses combined (PFT8) C3

# Shade tolerant-low Vcmax, low resp. rate and low mortality rate
# Shade intolerant-high Vcmax, high resp. rate and high mortality rate

params_species <- tibble(
  # species         0 1 2 3 4    ...
  lifeform      = c(9999,0,1,1,1,rep(1,11)),         # 0 for grasses; 1 for trees
  phenotype     = c(9999,0,0,1,1,rep(1,11)),         # 0 for Deciduous; 1 for Evergreen
  pt            = rep(0,16),                      # 0 for C3; 1 for C4
  # Root parameters
  alpha_FR      = rep(1.2,16),                    # Fine root turnover rate yr-1
  rho_FR        = rep(200,16),                    # material density of fine roots (kgC m-3)
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),               # mol /(s m2 Mpa)
  leaf_size     = rep(0.04,16), 
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),               # mol m-2 s-1 
  Vannual       = rep(1.2,16),                   # kgC m-2 yr-1
  wet_leaf_dreg = rep(0.3,16),                   # wet leaf photosynthesis down-regulation: wet leaf is 30% less than dry leaf
  m_cond        = c(9999,7.0,9.0,9.0,9.0,rep(9.0,11)),                   # factor of stomatal conductance
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),                 # kgC kgN-1 yr-1
  gamma_SW      = rep(0.08,16),                  # kgC m-2 Acambium yr-1
  gamma_FR      = rep(12.0,16),                  # kgC kgN-1 yr-1 
  tc_crit       = c(9999,10,12,0,0,rep(0,11)),    #rep(283.16,16),                # OFF degree C, converted to K in spdata
  tc_crit_on    = c(9999,8,10,0,0,rep(15,11)), #rep(280.16,16),                # ON degree C, converted to K in spdata
  gdd_crit      = c(9999,50,60,0,0,rep(0,11)),  #rep(280.0,16),   
  seedlingsize  = c(9999,0.01,0.05,0.05,0.05,rep(0.05,11)),    # initial size of seedlings #In Ensheng BiomeE: 0.05
  LNbase         = c(9999,1.0E-3,0.8E-3,0.5E-3,0.5E-3,rep(0.5E-3,11)),  # kgN m-2 leaf, Vmax = 0.03125*LNbase !rep(0.8E-3,16), 
  lAImax         = c(9999,2.0,3.0,3.5,3.5,rep(3.5,11)),  # maximum crown LAI !rep(3.5,16),
  Nfixrate0     = rep(0,16),                      # 0.03 kgN kgRootC-1 yr-1
  NfixCost0     = rep(0,16),                     # 12, 24 gC/gN
  phiCSA        = rep(0.25E-4,16),                # ratio of sapwood area to leaf area
  mortrate_d_c  = c(9999,0.05,0.025,0.02,0.02,rep(0.02,11)),  # canopy tree mortality rate, year-1 !rep(0.01,16),
  mortrate_d_u  = rep(0.075,16),                  # understory tree mortality rate, year-1
  maturalage    = c(9999,0,5,5,5,rep(5,11)),        # the age that can reproduce
  fNSNmax       = rep(5,16),                      # multiplier for NSNmax as sum of potential bl and br
  LMA           = c(9999,0.025,0.025,0.14,0.14,rep(0.14,11)),  # Leaf mass per unit area. In Ensheng rep(0.035,16)
  rho_wood      = c(9999,120,350,300,300,rep(300,11)),         # wood density In Ensheng rep(300,16),
  alphaBM       = rep(5200,16),                   
  thetaBM       = rep(2.5,16), 
  # add calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),  # ! Root/Leaf area ratio
  LAI_light     = rep(3.5,16)               # Light-limited crown LAI
) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790), # Pa
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), # mol/(s MPa m)
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_cohort_species = seq(1,10,1),   # indicates different species. The number taken is = init_n_cohorts defined in the model!
  init_cohort_nindivs = rep(0.008,10),  # initial individual density, individual/m2 ! 1 indiv/m2 = 10.000 indiv/ha
  init_cohort_bsw     = rep(0.2,10),  # initial biomass of sapwood, kg C/individual
  init_cohort_bHW     = rep(0.0, 10),  # initial biomass of heartwood, kg C/tree
  init_cohort_nsc     = rep(0.5,10)   # initial non-structural biomass
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,    # initial fast soil C, kg C/m2
  init_slow_soil_C    = 0.0,    # initial slow soil C, kg C/m2
  init_Nmineral       = 0.015,  # Mineral nitrogen pool, (kg N/m2)
  N_input             = 0.0008  # annual N input to soil N pool, kgN m-2 yr-1
)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

### Disturbance regime ####

#This contains the forcing time series data frame where the disturbance is to be defined as the fraction 
#of aboveground biomass harvested (`harv`). Additional specifications of the disturbance forcing, 
#which are more specific to the simulations done here, are hard-coded (see below). 

#Model is first run to steady state. Then for another 100 years undisturbed (continued steady-state) 
#until first disturbance. After first disturbance left undisturbed for 900 years to allow for 
#recovery (in some simulations recovery may take long). Then a regime of repeated disturbance after 
#simulation year 1000 to investigate disturbance-recovery (non-steady state) dynamics, 
#first at low frequency for 1000 years (disturbance every 250 years), then at high frequency for 1000 
#years (disturbance every 25 years).

#Disturbance is implemented by:
# - year 100 first disturbance applied, then recovery until year 1000
# - year 1000 second disturbance, then every 250 years disturbed for 1000 years
# - ... then every 25 years disturbed for another 1000 years

#To be handled by model by (new) forcing time series as input (`harv`)
fharv <- 0.9
harv_vec <- rep(0, 999)
harv_vec[100] <- fharv
harv_vec <- c(harv_vec, rep(c(fharv, rep(0, 249)), 4), rep(c(fharv, rep(0, 24)), 40), 0)
df_harv <- tibble(year = seq(length(harv_vec)), harv = harv_vec)

df_harv <- tibble(year = seq(1:450), harv = c(rep(0,200),0,rep(0,249)))
#df_harv <- tibble(year = seq(1:450), harv = c(rep(0,100),rep(c(fharv, rep(0, 69)), 5)))

df_harv %>% 
  ggplot(aes(year, harv)) +
  geom_line() +
  ylim(0, 1)

# Define forcing data ####
#biomee_p_model_drivers$forcing[[1]]
biomee_forcing_FIN <- read.csv("~/Documents/Collaborations/DBEN/cru_jra_1901-2020/biomee_forcing_FIN.csv")
biomee_forcing_FIN
df_forcing <- biomee_forcing_FIN

# Set fix values of co2 412 and 562
df_forcing$co2 <- 412 # 562

## get mean seasonal cycle and repeat this every year of all simulations
#df_forcing <- biomee_p_model_drivers$forcing[[1]] %>% mutate(doy = lubridate::yday(date)) %>% 
#  group_by(doy) %>% 
#  summarise(across(is.numeric, mean))

# Repeat mean seasonal cycle `nyears` times where `nyears` corresponds to the length of the harvest time 
# series (rows in `df_harv`). The column `year` now signifies simulation year and goes from 1 to `nyears`.
# Add harvest forcing to drivers. 
#nyears <- nrow(df_harv)
#df_forcing <- df_forcing %>% 
#  slice(rep(1:n(), nyears)) %>% 
#  mutate(year = rep(1:nyears, each = 365))
nyears <- nrow(df_harv)/length(unique(biomee_forcing_FIN$year))
df_forcing <- df_forcing %>% 
  slice(rep(1:n(), nyears)) %>% rename(yearID=year) %>%
  mutate(year = rep(1:450, each = 365)) %>% relocate(year, .after=yearID) %>%
  mutate(hour=11.5)

# Add 2020 for running spinup
#spinup_forcing <- biomee_forcing_FIN %>% filter(year==2020) %>% rename(yearID=year) 
#df_forcing <- spinup_forcing %>% bind_rows(df_forcing) %>%
#  mutate(year = rep(1:451, each = 365)) %>% relocate(year, .after=yearID) 

# Add harvest to forcing, assuming harvest on Jan 1st.
df_forcing_disturb <- df_forcing %>% 
  left_join(
    df_harv %>% 
      mutate(doy = 1),
    by = c("doy", "year")
  ) %>% 
  mutate(harv = ifelse(is.na(harv), 0, harv))

## for control simulation
df_forcing <- df_forcing %>%
  mutate(harv = 0)

# Add N deposition as NOx and NHy.
df_forcing <- df_forcing %>% 
  mutate(nox = 0, nhy = 0)

df_forcing_disturb <- df_forcing_disturb %>% 
  mutate(nox = 0, nhy = 0)

## for versions above 4.0
df_drivers_disturb <-tibble(sitename = site_info$sitename,
                            site_info = list(tibble(site_info)),
                            params_siml = list(tibble(params_siml)),
                            params_tile = list(tibble(params_tile)),
                            params_species=list(tibble(params_species)),
                            params_soil=list(tibble(params_soil)),
                            init_cohort=list(tibble(init_cohort)),
                            init_soil=list(tibble(init_soil)),
                            forcing=list(tibble(df_forcing_disturb)),
                            .name_repair = "unique")

## simulations with disturbance
#df_drivers_disturb <- biomee_p_model_drivers
#df_drivers_disturb$forcing[[1]] <- df_forcing_disturb
#df_drivers_disturb$params_siml[[1]]$firstyeartrend <- 0
#df_drivers_disturb$params_siml[[1]]$nyeartrend <- 3000

### Run the model
out_sc1 <- runread_biomee_f(
  df_drivers_disturb,
  makecheck = TRUE,
  parallel = FALSE
)

g1 <- out_sc1$data[[1]]$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC") 

g2 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(sumBA=sum(dbh*dbh*pi/4*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = sumBA,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "BA") + 
  scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))

g3 <- out_sc1$data[[1]]$output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(CrownArea=sum(Acrown*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = CrownArea,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "CrownArea") #+ 
#scale_colour_discrete(labels = c("Grass","Broadleaf","Needleleaf1","Needleleaf2"))

print(g1/g2/g3)

xx<- out_sc1$data[[1]]$output_annual_cohorts %>% filter(PFT==1)
