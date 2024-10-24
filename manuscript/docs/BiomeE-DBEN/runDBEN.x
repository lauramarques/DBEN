#!/bin/sh
FSRCS="src/datatypes.F90 \
       src/io_mod.F90 \
       src/soil.F90 \
       src/vegetation.F90 \
       src/BiomeE.F90 \
       src/main.F90"

CPPFLAGS=''
#CPPFLAGS+="-DHydro_test"
CPPFLAGS+="-DDBEN_run"

echo $FSRCS

#gfortran $FSRCS -o ess -I/opt/local/include -L/opt/local/lib -lnetcdff
#gfortran $FSRCS -o ess -I/Users/eweng/MACPORTS/gcc49-python3/include -L/Users/eweng/MACPORTS/gcc49-python3/lib -lnetcdff
#gfortran src/datatypes.F90 src/io_mod.F90 src/soil.F90 src/vegetation.F90 src/BiomeE.F90 src/main.F90 -DHydro_test -o ess

gfortran -o ess $FSRCS $CPPFLAGS

#for fparameter in $(ls ./para_files/MultiRuns/parameters_CRU_FIN_aCO2_02.nml); do
for fparameter in $(ls ./para_files/MultiRuns/parameters_CRU_*.nml); do
   echo $fparameter
   cat $fparameter > ./para_files/input.nml
   ./ess
   rm ./para_files/input.nml
done

rm ess
rm esdvm.mod
rm datatypes.mod
rm io_mod.mod
rm soil_mod.mod
rm biomee_mod.mod
