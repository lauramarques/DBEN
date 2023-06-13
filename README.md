# DBEN

This repository contains the scripts to run the DBEN simulations.
BiomeE model from the rsofun version in lauramarques/rsofun_lm DBEN branch.

The repository is organized as follows:

    data - includes the inputs (forcing data)
    analysis - includes the scripts for data analysis.
      DBEN_biomee_[site].R - runs the model
      DBEN_vars_[site].R - prepares the variables in netcdf format
      DBEN_fig_[site].R - plots figures for requested variables
      DBEN_netcdf.R - reads the netcdf files and tests outputs
    docs - includes usefull documentation about the project and outputs.
