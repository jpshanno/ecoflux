# ecoflux

The main purpose of this package is to provide tools to process ecological fluxes. This includes soil/stem gas flux (CO<sub>2</sub>, CH<sub>4</sub>, and N<sub>2</sub>), sap flux, and changes in hydrology.

## Installation  
This package is only available via GitHub. To install run the following lines <code>install.packages('devtools'); devtools::install_github('jpshanno/ecoflux')</code>  

## Gas Flux   
The function <code>efflux</code> provides an interactive way to clean data and fit a regression. It is especially useful for soil gas flux measurements from manual syringe sampling or automated systems such as the PP Systems EGM-4. It can be demoed with a sample data set <a href=http://apps.streamlinedecology.com/efflux>here</a>.  
  
The function <code>conc_to_flux</code> allows for easy conversion from change in chamber concentration to gas or carbon flux from the soil or stem surface.

## Sap Flux  
The methods laid out in Pataki (2011) for scaling sap flux to whole-tree sap flow when the sapwood depth extends beyond the length of the sap-flux probe is available in <code>pataki_flow</code>.

## Hydrology  
The functions <code>read_xle</code> and <code>read_and_convert_xle</code> allow you to directly import levellogger and barologger data from Solinst loggers.

## Selected Other Functions  
- <code>read_dir</code> - reads in a directory of identically formatted data files  
- <code>scientific_10x</code> - Format numbers to scientific notation ('XX x 10^Y^')
