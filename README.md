# DBEN

This repository contains the scripts to run the DBEN simulations.

The repository is organized as follows:

    data - includes the forcing data (inputs), benchmarking and model outputs (outputs_mod).
    analysis - includes the scripts for data analysis.
      00_DBEN_forcing.R - prepares the forcing data as input for the modle
      01_DBEN_biomee_[site].R - runs the model
      02_DBEN_calibration.R - compares the model outputs against the benchmarking
      03_DBEN_figures.R - set of functions to plot the figures with outputs
      04_DBEN_variables.R - set of functions to save the outputs in netcdf format
      04_DBEN_vars_[site].R - saves the output variables in netcdf format
      05_DBEN_netcdf.R - reads the netcdf files and tests outputs
    manuscript - includes documentation about the project and figures.
