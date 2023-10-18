# DBEN

This repository contains the scripts to run the DBEN simulations.

The repository is organized as follows:

    data - includes inputs (forcing data) and model outputs.
    analysis - includes the scripts for data analysis.
      DBEN_biomee_[site].R - runs the model
      DBEN_fig_[site].R - plots figures for requested variables
      DBEN_vars_[site].R - prepares the variables in netcdf format
      DBEN_netcdf.R - reads the netcdf files and tests outputs
    manuscript - includes documentation about the project and figures.
