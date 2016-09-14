# ecoflux

The main purpose of this package is to provide tools to process ecological fluxes. The two major fluxes included in the package are soil/stem gas flux (CO<sub>2</sub> and CH<sub>4</sub>) and sap flux.

##Gas Flux 
The function efflux() provides an interactive way to clean data and fit a regression. It is especially useful for soil gas flux measurements from manual syringe sampling or automated systems such as teh PP Systems EGM-4. It can be demoed with a sample data set <a href=https://joeshannon.shinyapps.io/efflux/>here</a>. The function conc_to_flux allows for easy conversion from change in chamber concentration to gas or carbon flux from the soil or stem surface.

##Sap Flux
The methods laid out in Pataki (2011) are used for scaling sap flux to whole-tree sap flow when the sapwood depth extends beyond the length of the sap-flux probe.

##Selected Other Functions
read_dir - reads in a directory of identically formatted data files  
ggpanel - creates panels from ggplots that use shared functions

#To Do
Add hydrologic fluxes (load)
Add sap flow to transpiration conversion functions
