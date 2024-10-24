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

# get time in days
from datetime import datetime
today = datetime.today()
time_num = today.toordinal()

font = {'family' : 'serif',
        'color'  : 'darkred',
        'weight' : 'normal',
        'size'   : 14,
        }

#%% Path and file names
fpath = './output/'
fpath = '../Umort1.2m/'
path_out  = '../Umort1.2m/NetCDF/' # './BiomeESimulations/SingleVars/'

# Constants
dtype = ['Ecosystem_yearly','Cohort_yearly']
N_pfts = 4 # PFTs at one site
N_totpfts = 8 # total PFTs at the three sietes
N_bins = 16
PI = 3.1415926
DBHbins=[0.0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.5, 2.0, 999.9]

# Site and experiments
modelID= 'BiomeE'
siteID = ['FIN','BIA','BCI']
CO2    = ['aCO2','eCO2']
testID = ['PS','PS','PS','PS','PS','PS','PS'] # first: P0
dstbID = ['00','01','02','04','08','20','40']
#dstbID = ['00']

# Ask for abrreviation of PFTs
AllPFT = [['1 Grass (PFT8);',\
           '2 Shade intolerant needleleaf (PFT1);', \
           '3 Shade tolerant needleleaf( PFT2);', \
           '4 Shade intolerant broadleaf deciduous (PFT3)'], \
          ['1 Grass (PFT8);',
           '2 Shade tolerant needleleaf (PFT2);', \
           '3 Shade intolerant broadleaf deciduous (PFT3);',\
           '4 Shade tolerant broadleaf deciduous (PFT4)'], \
          ['1 Grass (PFT8);',\
           '2 Tropical shade intolerant evergreen (PFT5);', \
           '3 Tropical shade tolerant evergreen (PFT6);',\
           '4 Tropical deciduous (PFT7)']]
PFT_No = np.array([[8,1,2,3], \
                   [8,2,3,4], \
                   [8,5,6,7]])-1

varID_yr = ['AGB','Rh','nbp','Hmax','Hstar']
LongID_yr = ['Aboveground biomass','Heterotrophic respiration','Net biospheric production',\
             'Maximum tree height','Critical tree height']
Unit_yr = ['KgC m-2 yr-1','KgC m-2 yr-1','KgC m-2 yr-1','m','m']

varID_yr_PFT = ['cveg','cwood','lai','CA','BA','height',\
                 'WBgrowth','BAgrowth','gpp','npp']
LongID_yr_PFT = ['Carbon mass in vegetation by PFT',\
                  'Carbon mass in wood by PFT',\
                  'Leaf area index',\
                  'Crown area',\
                  'Basal Area',\
                  '95 th percentile of tree height',\
                  'Woody biomass growth',\
                  'Basal area growth',\
                  'Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Land',\
                  'Carbon Mass Flux out of Atmosphere due to Net Primary Production on Land']
Unit_yr_PFT = ['KgC m-2','KgC m-2','m2/m2','m2/ha','m2/ha','m',\
                'kg C m-2 yr-1','m2 ha-1 yr-1','KgC m-2 yr-1','KgC m-2 yr-1']

varID_yr_Class_PFT = ['nstem_size','cwood_size','stemmort_size','cmort_size','cdstb_size']
LongID_yr_Class_PFT = ['Stem number by size class',\
                       'Carbon mass in wood by size class',\
                       'Stem number loss due to mortality',\
                       'Woody carbon mass loss due to mortality',\
                       'Woody carbon mass loss due to disturbance']
Unit_yr_Class_PFT = ['count ha-1','KgC m-2','Count ha-1 yr-1','kg C m-2 yr-1','kg C m-2 yr-1']

PFTID = ['Grass','EG1','EG2','DC']     # FIN
#PFTID = ['Grass','EG-Shd','DC1','DC2'] # BIA
#PFTID = ['Grass','EG1','EG2','DC']     # BCI

#%% Retrieve data
N_site = len(siteID)
N_CO2L = len(CO2)
N_dstb = len(dstbID)

for iSite in range(N_site):
    for iCO2 in range(N_CO2L):
        for iTest in range(N_dstb):
            # Setup file names
            expID = modelID + '_' + testID[iTest]  + '_' + siteID[iSite] + '_' + CO2[iCO2] + '_' + dstbID[iTest] #'BiomeE_PS_BIA_eCO2_04' #'BiomeE_P0_FIN_aCO2' #'BiomeE_PS_FIN_aCO2_2' #
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
            Hmax = np.zeros(totYrs)
            Hcrt = np.zeros(totYrs)

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

            nstem_size = np.zeros((totYrs,N_bins,N_pfts))
            cwood_size = np.zeros((totYrs,N_bins,N_pfts))
            cmort_size = np.zeros((totYrs,N_bins,N_pfts))
            stemmort_size = np.zeros((totYrs,N_bins,N_pfts))
            cdstb_size = np.zeros((totYrs,N_bins,N_pfts))

            for i in range(totCCL):
                iYr  = int(CCYr[i,0])-1
                iPFT = int(CCYr[i,3])-1
                Hmax[iYr] = max(CCYr[i,9],Hmax[iYr])
                if CCYr[i,5]<1.01:
                    Hcrt[iYr] = CCYr[i,9]
                Nstem1[iYr,iPFT] = Nstem1[iYr,iPFT]  + CCYr[i,6]
                cveg[iYr,iPFT]  = cveg[iYr,iPFT]  + CCYr[i,6]*np.sum(CCYr[i,12:18])/10000
                cwood[iYr,iPFT] = cwood[iYr,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])/10000
                LAI[iYr,iPFT] = LAI[iYr,iPFT] + CCYr[i,6]*CCYr[i,11]/10000
                CA[iYr,iPFT]  = CA[iYr,iPFT]  + CCYr[i,6]*CCYr[i,10]
                BA[iYr,iPFT]  = BA[iYr,iPFT]  + CCYr[i,6]*PI*0.25*CCYr[i,8]**2
                WBgrowth[iYr,iPFT] = WBgrowth[iYr,iPFT] + CCYr[i,6]* CCYr[i,22] * CCYr[i,26]/10000
                BAgrowth[iYr,iPFT] = BAgrowth[iYr,iPFT] + CCYr[i,6]* CCYr[i,21]
                gpp[iYr,iPFT] = gpp[iYr,iPFT] + CCYr[i,6]*CCYr[i,18]/10000
                npp[iYr,iPFT] = npp[iYr,iPFT] + CCYr[i,6]*CCYr[i,19]/10000
                WDmort[iYr,iPFT] = WDmort[iYr,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])*CCYr[i,27]/10000.
                DENmort[iYr,iPFT] = DENmort[iYr,iPFT] + CCYr[i,6] * CCYr[i,27]/10000.
                for j in range(len(DBHbins)-1):
                    if CCYr[i,8]>=DBHbins[j] and CCYr[i,8]<DBHbins[j+1] and CCYr[i,3]>1:
                        nstem_size[iYr,j,iPFT] = nstem_size[iYr,j,iPFT] + CCYr[i,6]
                        cwood_size[iYr,j,iPFT] = cwood_size[iYr,j,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])/10000.0 # KgC/m2
                        cmort_size[iYr,j,iPFT] = cmort_size[iYr,j,iPFT] + CCYr[i,6]*(CCYr[i,14]+CCYr[i,15])*CCYr[i,27]/10000. # KgC/m2/yr
                        stemmort_size[iYr,j,iPFT] = stemmort_size[iYr,j,iPFT] + CCYr[i,6] * CCYr[i,27]/10000. # stems/yr
                        break
            for i in range(totYrs-1):
                if LandC[i,3]*0.8 > LandC[i+1,3]:
                    cdstb_size[i,:,:] = np.copy(cwood_size[i,:,:]) # disturbance happened

            for i in range(totCCL):
                iYr  = int(CCYr[i,0])-1
                iPFT = int(CCYr[i,3])-1
                Nstem2[iYr,iPFT] = Nstem2[iYr,iPFT]  + CCYr[i,6]

                if height[iYr,iPFT] < 0.00001 and Nstem2[iYr,iPFT]/Nstem1[iYr,iPFT]>=0.05:
                    height[iYr,iPFT] = CCYr[i,9]

            AGB[:] = np.sum(cveg[:,1:4],axis=1)*0.75
            Rh[:]  = LandC[:,2]
            nbp[:] = LandC[:,0]-LandC[:,1]-LandC[:,2] # NBP = GPP - NPP - Rh

            # Set grass's BA growth as zero
            BAgrowth[:,0] = 0.0

            #%% Write to NetCDF files
            # Put data into the standard arrays
            out_yr = np.zeros((len(varID_yr),totYrs))
            out_yr_PFT = np.zeros((len(varID_yr_PFT),totYrs,N_totpfts))
            out_yr_Class_PFT = np.zeros((len(varID_yr_Class_PFT),totYrs,N_bins,N_totpfts))

            out_yr[0,:] = AGB[:]
            out_yr[1,:] = Rh[:]
            out_yr[2,:] = nbp[:]
            out_yr[3,:] = Hmax[:]
            out_yr[4,:] = Hcrt[:]
            #['cveg','cwood','lai','CA','BA','height',\
            # 'WBgrowth','BAgrowth','gpp','npp']
            for iPFT in range(4):
                out_yr_PFT[0,:,PFT_No[iSite,iPFT]] = cveg[:,iPFT]
                out_yr_PFT[1,:,PFT_No[iSite,iPFT]] = cwood[:,iPFT]
                out_yr_PFT[2,:,PFT_No[iSite,iPFT]] = LAI[:,iPFT]
                out_yr_PFT[3,:,PFT_No[iSite,iPFT]] = CA[:,iPFT]
                out_yr_PFT[4,:,PFT_No[iSite,iPFT]] = BA[:,iPFT]
                out_yr_PFT[5,:,PFT_No[iSite,iPFT]] = height[:,iPFT]
                out_yr_PFT[6,:,PFT_No[iSite,iPFT]] = WBgrowth[:,iPFT]
                out_yr_PFT[7,:,PFT_No[iSite,iPFT]] = BAgrowth[:,iPFT]
                out_yr_PFT[8,:,PFT_No[iSite,iPFT]] = gpp[:,iPFT]
                out_yr_PFT[9,:,PFT_No[iSite,iPFT]] = npp[:,iPFT]

                #['nstem_size','cwood_size','stemmort','cmort','cdstb']
                out_yr_Class_PFT[0,:,:,PFT_No[iSite,iPFT]] = nstem_size[:,:,iPFT]
                out_yr_Class_PFT[1,:,:,PFT_No[iSite,iPFT]] = cwood_size[:,:,iPFT]
                out_yr_Class_PFT[2,:,:,PFT_No[iSite,iPFT]] = stemmort_size[:,:,iPFT]
                out_yr_Class_PFT[3,:,:,PFT_No[iSite,iPFT]] = cmort_size[:,:,iPFT]
                out_yr_Class_PFT[4,:,:,PFT_No[iSite,iPFT]] = cdstb_size[:,:,iPFT]

            # Create a netcdf dataset, one dimension, time
            for i in range(len(varID_yr)):
                sf1 = path_out + expID + '_' + varID_yr[i] + '.nc'
                f1 = nc4.Dataset(sf1,'w', format='NETCDF4') #'w' write
                f1.createDimension('time', totYrs)

                TotalYears = f1.createVariable('Years', 'f4', 'time')
                TotalYears[:] = np.arange(1,totYrs+1)

                NCout = f1.createVariable(varID_yr[i], 'f4', 'time')
                NCout.long_name = LongID_yr[i]
                NCout.units = Unit_yr[i]
                NCout[:] = out_yr[i,:]

                #Add global attributes
                f1.description = expID
                f1.history = "Created " + today.strftime("%d/%m/%y")
                #Closing the dataset
                f1.close()

            # Create a netcdf dataset, two dimensions, time and PFT
            for i in range(len(varID_yr_PFT)):
                sf1 = path_out + expID + '_' + varID_yr_PFT[i] + '.nc'
                f1 = nc4.Dataset(sf1,'w', format='NETCDF4') #'w' write
                f1.createDimension('time', totYrs)
                f1.createDimension('PFT', N_totpfts)

                TotalYears = f1.createVariable('Years', 'f4', 'time')
                TotalYears[:] = np.arange(1,totYrs+ 1)

                NCout = f1.createVariable(varID_yr_PFT[i], 'f4', ('time','PFT'))
                NCout.long_name = LongID_yr_PFT[i]
                NCout.units = Unit_yr_PFT[i]
                NCout[:,:] = out_yr_PFT[i,:,:]

                #Add global attributes and Close the dataset
                f1.description = expID + '. PFTs: ' + ' '.join(AllPFT[iSite])
                f1.history = "Created " + today.strftime("%d/%m/%y")
                f1.close()

            # Create netcdf datasets, three dimensions, time, Class, PFT
            for i in range(len(varID_yr_Class_PFT)):
                sf1 = path_out + expID + '_' + varID_yr_Class_PFT[i] + '.nc'
                f1 = nc4.Dataset(sf1,'w', format='NETCDF4') #'w' write
                f1.createDimension('time', totYrs)
                f1.createDimension('Class', N_bins)
                f1.createDimension('PFT', N_totpfts)

                TotalYears = f1.createVariable('Years', 'f4', 'time')
                TotalYears[:] = np.arange(1,totYrs+1)

                ClassBins = f1.createVariable('DBH_Bins', 'f4', 'Class')
                ClassBins[:] = DBHbins[0:N_bins]

                NCout = f1.createVariable(varID_yr_Class_PFT[i], 'f4', ('time','Class','PFT'))
                NCout.long_name = LongID_yr_Class_PFT[i]
                NCout.units = Unit_yr_Class_PFT[i]
                NCout[:,:,:] = out_yr_Class_PFT[i,:,:,:]

                #Add global attributes and close the dataset
                f1.description = expID + '. PFTs: ' + ' '.join(AllPFT[iSite])
                f1.history = "Created " + today.strftime("%d/%m/%y")
                f1.close()
            print(expID)
