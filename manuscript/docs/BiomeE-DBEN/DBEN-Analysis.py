#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 01/23/2023
  for BiomeE plant hydraulics
@author: eweng
"""
import numpy as np
import netCDF4 as nc4
import csv
import matplotlib.pyplot as plt
from cycler import cycler

linestyle_cycler = cycler('linestyle',['-','-.','--',':'])

font = {'family' : 'serif',
        'color'  : 'darkred',
        'weight' : 'normal',
        'size'   : 14,
        }

#%% Path and file names
fpath = './HighIniDen/'
fout  = './'
dtype = ['Ecosystem_yearly','Cohort_yearly']
N_pfts = 4 # total PFTs at one site
PI = 3.1415926
DBHbins=[0.0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.5, 2.0, 999.9]

# Site and experiments
siteID = ['FIN','BIA','BCI']

# Ask for abrreviation
AllPFT = [['Grass (PFT8);',\
           'Shade intolerant needleleaf (PFT1);', \
           'Shade tolerant needleleaf( PFT2);', \
           'Shade intolerant broadleaf deciduous (PFT3)'], \
          ['Grass (PFT8);',
           'Shade tolerant needleleaf (PFT2);', \
           'Shade intolerant broadleaf deciduous (PFT3);',\
           'Shade tolerant broadleaf deciduous (PFT4)'], \
          ['Grass (PFT8);',\
           'Tropical shade intolerant evergreen (PFT5);', \
           'Tropical shade tolerant evergreen (PFT6);',\
           'Tropical deciduous (PFT7)']]
CO2 = ['aCO2','eCO2']
dstb = ['00','01','02','04','08','20','40']


PFTID = ['Grass','EG1','EG2','DC']     # FIN
#PFTID = ['Grass','EG-Shd','DC1','DC2'] # BIA
#PFTID = ['Grass','EG1','EG2','DC']     # BCI

#%% Retrieve data
# File name
expID = 'BiomeE_PS' + '_' + siteID[0] + '_' + CO2[0] + '_' + dstb[0] #'BiomeE_PS_BIA_eCO2_04' #'BiomeE_P0_FIN_aCO2' #'BiomeE_PS_FIN_aCO2_2' #

fname1 = fpath + expID + '_' + dtype[0] + '.csv'
fname2 = fpath + expID + '_' + dtype[1] + '.csv'


# Land data yearly
f=open(fname1)
next(f) # skip headings
LandYrV=(np.array(list(csv.reader(f,delimiter=','))))
f.close()

# Total cohort yearly
f=open(fname2)
next(f) # skip headings
CCYrV=(np.array(list(csv.reader(f,delimiter=','))))
f.close()


# Land Data dimensions
rows = len(LandYrV)
col  = len(LandYrV[1]) - 1
LandYr = LandYrV[0:rows,0:col].astype(np.float)
totYrs = rows

# Total cohort yearly dimensions
rows = len(CCYrV)
col  = len(CCYrV[1]) - 1
CCYr = CCYrV[0:rows,0:col].astype(np.float)
totCCL = rows

#% Data calculation and analysis
LandC = np.copy(LandYr[:,[5,6,7,14,15]]) # GPP, Rauto, Rh, PlantC, SoilC

AGB = np.zeros(totYrs)
Rh  = np.zeros(totYrs)
nbp = np.zeros(totYrs)

BA = np.zeros((totYrs, N_pfts))
CA = np.zeros((totYrs, N_pfts))
LAI = np.zeros((totYrs, N_pfts))
height = np.zeros((totYrs, N_pfts))
cveg = np.zeros((totYrs, N_pfts))
cwood = np.zeros((totYrs, N_pfts))
gpp = np.zeros((totYrs, N_pfts))
npp = np.zeros((totYrs, N_pfts))
WBgrowth = np.zeros((totYrs, N_pfts))
BAgrowth = np.zeros((totYrs, N_pfts))
WDmort = np.zeros((totYrs, N_pfts))
DENmort = np.zeros((totYrs, N_pfts))

Nstem1 = np.zeros((totYrs, N_pfts))
Nstem2 = np.zeros((totYrs, N_pfts))

cwood_size = np.zeros((totYrs,16))
nstem_size = np.zeros((totYrs,16))
cmort_size = np.zeros((totYrs,16))

stemmort_size = np.zeros((totYrs,16, N_pfts))

cveg3  = np.zeros((totYrs,3))
Size3  = np.zeros((totYrs,3))
for i in range(totCCL):
    iYr  = int(CCYr[i,0])-1
    iPFT = int(CCYr[i,3])-1
    Nstem1[iYr,iPFT] = Nstem1[iYr,iPFT]  + CCYr[i,6]
    BA[iYr,iPFT]  = BA[iYr,iPFT]  + CCYr[i,6]*PI*0.25*CCYr[i,8]**2
    CA[iYr,iPFT]  = CA[iYr,iPFT]  + CCYr[i,6]*CCYr[i,10]/10000
    LAI[iYr,iPFT] = LAI[iYr,iPFT] + CCYr[i,6]*CCYr[i,11]/10000
    gpp[iYr,iPFT] = gpp[iYr,iPFT] + CCYr[i,6]*CCYr[i,18]/10000
    npp[iYr,iPFT] = npp[iYr,iPFT] + CCYr[i,6]*CCYr[i,19]/10000
    cwood[iYr,iPFT] = cwood[iYr,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])/10000
    cveg[iYr,iPFT]  = cveg[iYr,iPFT]  + CCYr[i,6]*np.sum(CCYr[i,12:18])/10000
    WBgrowth[iYr,iPFT] = WBgrowth[iYr,iPFT] + CCYr[i,6]* CCYr[i,22] * CCYr[i,26]/10000
    BAgrowth[iYr,iPFT] = BAgrowth[iYr,iPFT] + CCYr[i,6]* CCYr[i,21] #/10000.0
    WDmort[iYr,iPFT] = WDmort[iYr,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])*CCYr[i,27]/10000.
    DENmort[iYr,iPFT] = DENmort[iYr,iPFT] + CCYr[i,6] * CCYr[i,27]/10000.
    for j in range(len(DBHbins)-1):
        if CCYr[i,8]>=DBHbins[j] and CCYr[i,8]<DBHbins[j+1] and CCYr[i,3]>1:
            cwood_size[iYr,j] = cwood_size[iYr,j] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])/10000.0 # KgC/m2
            nstem_size[iYr,j] = nstem_size[iYr,j] + CCYr[i,6]
            cmort_size[iYr,j] = cmort_size[iYr,j] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])*CCYr[i,27]/10000. # KgC/m2/yr
            stemmort_size[iYr,j,iPFT] = stemmort_size[iYr,j,iPFT] + CCYr[i,6] * CCYr[i,27]/10000. # stems/yr
            break
for i in range(totCCL):
    iYr  = int(CCYr[i,0])-1
    iPFT = int(CCYr[i,3])-1
    Nstem2[iYr,iPFT] = Nstem2[iYr,iPFT]  + CCYr[i,6]

    if height[iYr,iPFT] < 0.00001 and Nstem2[iYr,iPFT]/Nstem1[iYr,iPFT]>=0.05:
        height[iYr,iPFT] = CCYr[i,9]


AGB[:] = np.sum(cveg[:,1:4],axis=1)*0.75
Rh[:]  = LandC[:,2]
nbp[:] = LandC[:,0]-LandC[:,1]-LandC[:,2] # NBP = GPP - NPP - Rh

meanWDCclass = np.mean(cwood_size[totYrs-60:totYrs,:],axis=0)
meanDen = np.mean(nstem_size[totYrs-60:totYrs,:],axis=0)
meanDen[0:2] = 0.0
Size3[:,0] = np.sum(nstem_size[:,0:5],axis=1)
Size3[:,1] = np.sum(nstem_size[:,5:10],axis=1)
Size3[:,2] = np.sum(nstem_size[:,10:15],axis=1)
cveg3[:,0] = np.sum(cwood_size[:,0:5],axis=1)
cveg3[:,1] = np.sum(cwood_size[:,5:10],axis=1)
cveg3[:,2] = np.sum(cwood_size[:,10:15],axis=1)

#%% Write to NetCDF files
# Create a netcdf dataset

path1 = fout
var_name = 'cwood'
fout1 = expID + '_' + var_name + '.nc'
# Compare two files
sf1 = path1 + fout1
# Open this netcdf file
f1 = nc4.Dataset(sf1,'w', format='NETCDF4') #'w' write

f1.createDimension('time', totYrs)
f1.createDimension('PFT', 4)

TotalYears = f1.createVariable('Years', 'f4', 'time')
TotalYears[:] = np.arange(totYrs)

WoodC = f1.createVariable(var_name, 'f4', ('time','PFT'), fill_value=-999)
WoodC.long_name = "Carbon mass in wood by PFT"
WoodC.units = 'kgC m-2'
WoodC[:,:] = cwood


# get time in days since Jan 01,01
from datetime import datetime
today = datetime.today()
time_num = today.toordinal()

#Add global attributes
f1.description = expID + '. PFTs: ' + ' '.join(AllPFT[0])
f1.history = "Created " + today.strftime("%d/%m/%y")

#Closing the dataset
f1.close()


#%% Plot
xyear = np.arange(totYrs)
SizeID= ['<20 cm','20-70 cm','>70 cm']

# Vegetation structure
plt.figure(1) #
plt.clf()
plt.subplot(221)
plt.plot(xyear, BA)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Basal area\n (m$^{2}$ ha$^{-1}$)', fontdict=font)

plt.subplot(222)
plt.plot(xyear, CA)
plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Crown area\n (m$^{2}$ m$^{-2}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, LAI)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Leaf area\n (m$^{2}$ m$^{-2}$)', fontdict=font)

plt.subplot(224)
plt.plot(xyear, height)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Height (95%) (m)', fontdict=font)

# Vegetation carbon
plt.figure(2) #
plt.clf()
plt.subplot(221)
plt.plot(xyear,gpp)
plt.xlabel('Year', fontdict=font)
plt.ylabel('GPP\n (KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(222)
plt.plot(xyear, npp)
plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('NPP\n (KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, WBgrowth)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Wood growth \n(KgC m$^{2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(224)
plt.plot(xyear, cveg)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Biomass\n (KgC m$^{-2}$)', fontdict=font)

# Ecosystem C
plt.figure(3) #
plt.clf()
plt.subplot(221)
plt.plot(xyear, LandC[:,0])
plt.xlabel('Year', fontdict=font)
plt.ylabel('GPP\n(KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(222)
plt.plot(xyear, LandC[:,1])
plt.xlabel('Year', fontdict=font)
plt.ylabel('Rauto\n(KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, LandC[:,3])
plt.xlabel('Year', fontdict=font)
plt.ylabel('Plant C \n(KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

plt.subplot(224)
plt.plot(xyear, LandC[:,4])
plt.xlabel('Year', fontdict=font)
plt.ylabel('Soil C \n(KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)


# Size classes
xbins = np.arange(16)
plt.figure(4) #
plt.clf()
plt.subplot(221)
plt.scatter(xbins, meanDen)
#plt.yscale("log")
plt.xlabel('Bin', fontdict=font)
plt.ylabel('Stems/ha', fontdict=font)

plt.subplot(222)
plt.scatter(xbins, meanWDCclass)
plt.xlabel('Bin', fontdict=font)
plt.ylabel('Wood C (KgC m$^{-2}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, Size3)
plt.yscale("log")
plt.xlabel('Year', fontdict=font)
plt.ylabel('Stems/ha', fontdict=font)

plt.subplot(224)
plt.plot(xyear, cveg3)
#plt.legend((SizeID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Wood C (KgC m$^{-2}$)', fontdict=font)

# Growth and mortality
plt.figure(5) #
plt.clf()
plt.subplot(221)
plt.plot(xyear, WBgrowth)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Wood growth \n(KgC m$^{2}$ yr$^{-1}$)', fontdict=font)

BAgrowth[:,0] = 0.0
plt.subplot(222)
plt.plot(xyear,BAgrowth)
plt.xlabel('Year', fontdict=font)
plt.ylabel('dBA\n (m$^{2}$ ha$^{-1}$ yr$^{-1}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, WDmort)
plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('WD mort\n (KgC m$^{-2}$ yr$^{-1}$)', fontdict=font)

DENmort[:,0] = 0.0
plt.subplot(224)
plt.plot(xyear, DENmort)
#plt.legend((PFTID),loc=0,ncol=1)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Den mort\n (n m$^{-2}$ yr$^{-1}$)', fontdict=font)

# Size class
plt.figure(6) #
plt.clf()
plt.subplot(221)
plt.plot(xyear, Size3)
plt.yscale("log")
plt.xlabel('Year', fontdict=font)
plt.ylabel('Stems/ha', fontdict=font)

plt.subplot(222)
plt.plot(xyear, cveg3)
plt.legend((SizeID),loc=0,ncol=1)
plt.yscale("log")
plt.xlabel('Year', fontdict=font)
plt.ylabel('Wood C (KgC m$^{-2}$)', fontdict=font)

plt.subplot(223)
plt.plot(xyear, nstem_size)
plt.yscale("log")
plt.xlabel('Year', fontdict=font)
plt.ylabel('Stems/ha', fontdict=font)

plt.subplot(224)
plt.plot(xyear, cwood_size)
plt.xlabel('Year', fontdict=font)
plt.ylabel('Wood C (KgC m$^{-2}$)', fontdict=font)
