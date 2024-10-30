
## Plant C (Biomass) ----
# tile level
plantC_tile_fig <- function(data){
  fig <- data |> #BiomeE_P0_FIN_aCO2_annual_tile |>
  #slice(510+1:nrow(data)) |> 
  #mutate(year = 1:1020) |>
  ggplot() + 
  geom_line(aes(x=year, y=plantC),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Plant C (kg C ", m^-2, ") "))) + 
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  #scale_y_continuous(lim=c(0,25)) +
  theme_classic() 
return(fig)
}

## GPP----
# tile level
GPP_tile_fig <- function(data){
fig <- data |> 
  #slice(510+1:nrow(data)) |> 
  #mutate(year = 1:1020) |>
  ggplot() + 
  geom_line(aes(x=year, y=GPP),col="#377EB8") + 
  labs(x = "year", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  theme_classic() 
return(fig)
}

## Rauto ----
Rauto_tile_fig <- function(data){
fig <- data |> 
  #slice(510+1:nrow(data)) |> 
  #mutate(year = 1:1020) |>
  ggplot() + 
  geom_line(aes(x=year, y=Rauto),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Rauto (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  theme_classic() 
return(fig)
}

## Soil C ----
soilC_tile_fig <- function(data){
fig <- data |> 
  #slice(510+1:nrow(data)) |> 
  #mutate(year = 1:1020) |>
  ggplot() + 
  geom_line(aes(x=year, y=soilC),col="#377EB8") + 
  labs(x = "year", y =expression(paste("Soil C (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  theme_classic() 
return(fig)
}

## Woody biomass growth ----
WDgrow_tile_fig <- function(data){
fig <- data |> 
  ggplot() + 
  geom_line(aes(x=year, y=WDgrow),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Woody biomass growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## Mortality ----
WDmort_tile_fig <- function(data){
  fig <- data |>  
  ggplot() + 
  geom_line(aes(x=year, y=WDmort),col="#377EB8") + 
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}

## Carbon balance ----
CBal_tile_fig <- function(data){
fig <- data |> 
  #filter(!row_number() %in% c(1)) |>
  mutate(Carbon_balance=WDgrow-WDmort) |>
  ggplot() + 
  geom_line(aes(x=year, y=Carbon_balance),col="#377EB8") + 
  labs(x = "year", y = "WDgrow-WDmort") + 
  geom_hline(yintercept = 0, col="red", alpha=0.5) + 
  scale_y_continuous(lim=c(-1,1)) +
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}

## Carbon budget closure - tile ----
CBud_tile_fig <- function(data){
fig <- data |> 
  #filter(year > 542) |>
  mutate(Carbon_balance=WDgrow-WDmort,
         cwood = SapwoodC+WoodC,
         #sumCB = ifelse(row_number() == 1, cwood, WDgrow-WDmort-WDkill),
         sumCB = ifelse(row_number() == 1, cwood, WDgrow+WDrepr-WDmort-WDkill),
         cumsumCB = cumsum(sumCB)) |>
  ggplot() + 
  geom_line(aes(x=year, y=cumsumCB),col="#377EB8") + 
  geom_line(aes(x=year, y=cwood),col="purple") +
  labs(x = "year", y = "Carbon budget") + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## Carbon balance - cohorts ----
CBal_cohort_fig <- function(data){
  kill_low_density <- data |>
    filter(PFT != 1) |>
    mutate(nindivs = density/10000) |>
    filter(nindivs <= 1.0E-6)  |> 
    mutate(cmort_low = (sapwC+woodC)*(1 - deathrate)*density/10000)  |>
    select(cohort, year,cID, PFT, layer,cmort_low)
  
  fecundity_growth <- data |>
    filter(PFT != 1) |>
    filter(layer == 1)  |>
    mutate(fecundity = params_tile$f_initialBSW*seedC*density/10000) |>
    select(cohort, year,cID, PFT, layer, fecundity)
  
  fig <- data |> 
    filter(PFT != 1) |>
    left_join(kill_low_density) |>
    left_join(fecundity_growth) |>
    mutate(cmort_low = ifelse(is.na(cmort_low),0,cmort_low)) |> 
    group_by(year) |>
    summarise(#WBgrowth=sum((fwood*treeG)*density/10000, na.rm = T),
              WBgrowth=sum((fwood*treeG)*density/10000 + fecundity, na.rm = T),
              cmort=sum((sapwC+woodC)*deathrate*density/10000 + cmort_low)) |>
    #filter(year > 542) |>
    mutate(Carbon_balance=WBgrowth-cmort) |>
    ggplot() + 
    geom_line(aes(x=year, y=Carbon_balance),col="#377EB8") + 
    labs(x = "year", y = "Carbon_balance") +
    geom_hline(yintercept = 0, col="red", alpha=0.5) + 
    scale_y_continuous(lim=c(-1,1)) +
    theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}

## Carbon budget closure - cohorts ----
CBud_cohort_fig <- function(data){
kill_low_density <- data |>
  filter(PFT != 1) |>
  mutate(nindivs = density/10000) |>
  filter(nindivs <= 1.0E-6)  |> 
  mutate(cmort_low = (sapwC+woodC)*(1 - deathrate)*density/10000)  |>
  select(cohort, year,cID, PFT, layer,cmort_low)

fecundity_growth <- data |>
  filter(PFT != 1) |>
  filter(layer == 1)  |>
  mutate(fecundity = params_tile$f_initialBSW*seedC*density/10000) |>
  select(cohort, year,cID, PFT, layer, fecundity)

fig <- data |> 
  filter(PFT != 1) |>
  left_join(kill_low_density) |>
  left_join(fecundity_growth) |>
  mutate(cmort_low = ifelse(is.na(cmort_low),0,cmort_low)) |> 
  group_by(year) |>
  summarise(cwood=sum((sapwC+woodC)*density/10000),
            #WBgrowth=sum((fwood*treeG)*density/10000, na.rm = T),
            WBgrowth=sum((fwood*treeG)*density/10000 + fecundity, na.rm = T),
            cmort=sum((sapwC+woodC)*deathrate*density/10000 + cmort_low)) |>
  #filter(year > 542) |>
  mutate(sumCB = ifelse(row_number() == 1, cwood, WBgrowth-cmort),
         cumsumCB = cumsum(sumCB)) |>
  ggplot() + 
  geom_line(aes(x=year, y=cumsumCB),col="#377EB8") + 
  geom_line(aes(x=year, y=cwood),col="purple") +
  labs(x = "year", y = "Carbon budget") + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

# Pools ----

## 1. Carbon mass in vegetation by PFT ----
# cveg = Stem, coarse roots, fine roots, branches, leaves
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohorts output
cveg_fig <- function(data){
fig <- data |> 
  group_by(PFT,year) |>
  summarise(cveg=sum((nsc+seedC+leafC+rootC+sapwC+woodC)*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.factor(PFT)) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = cveg,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in vegetation (kg C ", m^-2, ") "))) + 
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  #scale_colour_discrete(labels = c("PFT8_Grasses","PFT3_Betula_pendula","PFT2_Picea_abies","PFT1_Pinus_sylvestris")) +
  theme_classic() 
return(fig)
}

## 2. Aboveground woody biomass ----
# AGcwood
# Units: kg C m-2
# Timestep: annual
# Dimensions: time
# cohort output
AGcwood_fig <- function(data){
fig <- data |>
  filter(PFT != 1) |>
  group_by(year) |>
  summarise(AGcwood=sum((sapwC+woodC)*0.75*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = AGcwood), col="#377EB8") +
  labs(x = "year", y = expression(paste("Aboveground woody biomass (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                          title = element_text(size = 10)) 
return(fig)
}

## 3. Carbon mass in wood by PFT ----
# cwood = Stem, coarse roots, branches
# Units: kg C m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
cwood_fig <- function(data){
fig <- data |>
  filter(PFT != 1) |>
  group_by(PFT,year) |>
  summarise(cwood=sum((sapwC+woodC)*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.factor(PFT)) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = cwood,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                          title = element_text(size = 10),legend.position="right") +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

# all PFTs together
cwood_all_fig <- function(data){
  fig <- data |>
    filter(PFT != 1) |>
    group_by(year) |>
    summarise(cwood=sum((sapwC+woodC)*density/10000)) |> 
    filter(year>510) |>
    mutate(year = year-510) |>
    ggplot() + 
    geom_line(aes(x = year, y = cwood)) +
    labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
    theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                            title = element_text(size = 10),legend.position="right") 
  return(fig)
}

## 4. Carbon mass in wood by size class ----
# cwood_size
# Units: kg C m-2
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cwood_size_fig <- function(data){
fig <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  #filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  summarise(cwood_size=sum((sapwC+woodC)*density/10000)) |>
  ggplot() + 
  geom_line(aes(x = year, y = cwood_size,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Carbon mass in wood (kg C ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## 5. Stem number by size class ----
# nstem_size
# Units: count ha-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
nstem_fig <- function(data){
fig <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  #filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  summarise(nstem_size=sum(density)) |> 
  ggplot() + 
  geom_line(aes(x = year, y = nstem_size,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Stem number (count ", ha^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

nstem_pft_fig <- function(data){
  fig <- data |> 
    filter(PFT != 1) |>
    group_by(PFT,year) |>
    summarise(nstem_size=sum(density)) |> 
    filter(year>510) |>
    mutate(PFT=as.factor(PFT)) |>
    mutate(year = year-510) |>
    ggplot() + 
    geom_line(aes(x = year, y = nstem_size,col=PFT)) +
    labs(x = "year", y = expression(paste("Stem number (count ", ha^-1, ") "))) + 
    scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                        labels = c(2,3,4)) +
    theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}

## 6. Leaf area index ----
# lai
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
lai_fig <- function(data) {
fig <- data |> 
  group_by(PFT,year) |>
  summarise(lai=sum(Aleaf*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.factor(PFT)) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = lai,col=PFT)) +
  labs(x = "year", y = expression(paste("Leaf area (", m^-2, " ", m^-2, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## 7. Crown area ----
# CA
# Units: m2 m-2
# Timestep: annual
# Dimensions: pft, time
# cohort output
CA_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(CA=sum(Acrown*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = CA,col=PFT)) +
  labs(x = "year", y = expression(paste("Crown area (", m^-2, " ", m^-2, ") "))) + 
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) +
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## 8. Basal area ----
# BA
# Units: m2 ha-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BA_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  #summarise(BA=sum(DBH*DBH*pi/4*density/10000)) |>
  summarise(BA=sum(BA*density)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = BA,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area (", m^-2, " ", ha^-1, ") "))) + 
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) +
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## 9. 95th percentile of tree height ----
# height
# Units: m
# Timestep: annual
# Dimensions: pft, time
# cohort output
height_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(height=quantile(height, probs = 0.95)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = height,col=PFT)) +
  labs(x = "year", y = "95th Height (m)") + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

# Fluxes ----
## 10. Woody biomass growth ----
# WBgrowth
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
WBgrowth_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(WBgrowth=sum(fwood*treeG*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x=year, y=WBgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Woody biomass growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

## 11. Basal area growth ----
# BAgrowth
# Units: m2 ha-1 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
BAgrowth_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(BAgrowth=sum(((DBH+dDBH)**2*pi/4-DBH**2*pi/4)*density/10000)) |>
  #summarise(BAgrowth=sum(dBA*density)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x=year, y=BAgrowth,col=PFT)) +
  labs(x = "year", y = expression(paste("Basal area growth (", m^2, " ",ha^-1, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

## 12. Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
cmort_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  group_by(PFT,year) |>
  #summarise(cmort=sum(c_deadtrees)) |> # c_deadtrees includes all organic pools!
  summarise(cmort=sum((sapwC+woodC)*deathrate*density/10000)) |>
  filter(year>510) |>
  mutate(PFT=as.factor(PFT)) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = cmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

### Carbon Mass Flux lost from live wood due to mortality or other turnover process ----
# cmort
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time
# cohort output
cmort_size_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  mutate(dbh_bins = cut(DBH, breaks = c(0,1,5,10,15,20,30,40,50,60,70,80,90,100,150,200),right=F)) |>
  #filter(dbh_bins!="[0,1)"&dbh_bins!="[1,5)") |>
  filter(year>510) |>
  mutate(year = year-510) |>
  group_by(dbh_bins,year) |>
  #summarise(cmort=sum(c_deadtrees)) |> 
  summarise(cmort=sum((sapwC+woodC)*deathrate*density/10000)) |>
  ggplot() + 
  geom_line(aes(x = year, y = cmort,col=dbh_bins)) +
  labs(x = "year", y = expression(paste("Carbon mass flux lost (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
return(fig)
}

## 13. Stem number Flux lost from vegetation due to mortality or other turnover process ----
# stemmort
# Units: Count m-2 yr-1
# Timestep: annual
# Dimensions: sizeclass, time, pft
# cohort output
stemmort_fig <- function(data) {
fig <- data |> 
  filter(PFT != 1) |>
  group_by(PFT,year) |>
  #summarise(stemmort=sum(n_deadtrees)) |>
  summarise(stemmort=sum(deathrate*density/10000)) |> 
  filter(year>510) |>
  mutate(PFT=as.factor(PFT)) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = stemmort,col=PFT)) +
  labs(x = "year", y = expression(paste("Stem number flux lost (count ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_colour_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c(2,3,4)) 
return(fig)
}

## 14. GPP - Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land ----
# gpp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
gpp_fig <- function(data) {
fig <- data |> 
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(gpp=sum(GPP*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = gpp,col=PFT)) +
  labs(x = "year", y = expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))
return(fig)
}

## 15. NPP - Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land ----
# npp
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: pft, time
# cohort output
npp_fig <- function(data) {
  fig <- data |> 
  mutate(PFT=as.factor(PFT)) |>
  group_by(PFT,year) |>
  summarise(npp=sum(NPP*density/10000)) |> 
  filter(year>510) |>
  mutate(year = year-510) |>
  ggplot() + 
  geom_line(aes(x = year, y = npp,col=PFT)) +
  labs(x = "year", y = expression(paste("NPP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}

## 16. NBP - Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land ----
#This is the net mass flux of carbon between land and atmosphere calculated as 
#photosynthesis MINUS the sum of plant and soil respiration, and carbon fluxes from fire. 
# (equivalent to NEP = NPP minus soil respiration)
# nbp 
# Units: kg C m-2 yr-1
# Timestep: annual
# Dimensions: time
# cohort tile
nbp_fig <- function(data) {
  fig <- data |> 
    slice(510+1:nrow(data)) |> 
  mutate(year = 1:1020, nbp = GPP-Rauto-Rh) |>
  select(year, nbp) |>
  ggplot() + 
  geom_line(aes(x = year, y = nbp),col="#377EB8") +
  labs(x = "year", y = expression(paste("NBP (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_classic() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
  return(fig)
}
