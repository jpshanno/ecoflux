# Concentration to Flux----------------------------------------------------------
#' Convert change in gas concentration over time to flux
#' 
#' This function uses the ideal gas law to convert changes in concentration 
#' within a sample chamber to flux rates for the gas species or for carbon. 
#' Conversion uses the ideal gas law to determine the convert the change in
#' concentration to the change in mass.
#' 
#' @return A vector giving flux of the gas species or of carbon in the selected 
#'   output units
#' @param delta.gas.ppm A vector of slopes representing the change in concentration 
#'   over time
#' @param chamber.volume.cm3 The gas chamber volume in cm^3; can be a vector of 
#'   the same length as delta.ppm or a single numeric value
#' @param collar.area.cm2 The area of the chamber footprint in cm^2; can be a vector
#'   of the same length as delta.ppm or a single numeric value
#' @param temperature.k The temperature within the chamber in Kelvin; can be a 
#'   vector of the same length as delta.ppm or a single numeric value
#' @param pressure.Pa The pressure within the chamber in Pascals;; can be a 
#'   vector of the same length as delta.ppm or a single numeric value
#' @param species The gas species of interest; 'CO2', 'CH4', or 'N2O'
#' @param input.time The time scale for delta.ppm; either 'seconds' or 'minutes'
#' @param ouput.units The desired output flux units; 'g/m2h1', 'mg/m2h1', 
#'   'g/m2d1', or 'mg/m2d1'; defaults to 'g/m2h1'
#' @param C.only A logical specificying if the output flux should be calculated 
#'   for the gas species or for carbon only; defaults to FALSE
#' @param delta.ppm Deprecated, use delta.gas.ppm
#' @export
#' @rdname conc_to_flux

conc_to_flux <- 
  function(delta.gas.ppm, 
           chamber.volume.cm3, 
           collar.area.cm2, 
           temperature.k, 
           pressure.pa, 
           species, 
           input.time, 
           output.units = "g/m2d1", 
           C.only = FALSE,
           delta.ppm = NULL){
    
    if(!is.null(delta.ppm)){
      if(!is.numeric(delta.ppm)){
        stop("delta.ppm must be a numeric vector")
      }
      message("delta.ppm has been deprecated in favor of delta.gas.ppm")
      delta.gas.ppm <- delta.ppm
    }
    
    if(!all(sapply(list(delta.gas.ppm, chamber.volume.cm3, collar.area.cm2, temperature.k, pressure.pa), is.numeric))){
      stop("chamber.volume, chamber.area, temperature, and pressure must be numeric vectors or single numeric values")
    }
    
    if(!(species %in% c("CO2", "CH4", "N2O"))){
      stop("species must be specified as 'CO2', 'CH4', or 'N2O'")
    }
    
    if(!(input.time %in% c("seconds", "minutes"))){
      stop("input.time must be specified as 'minutes' or 'seconds'")
    }
    
    if(!(output.units %in% c("g/m2h1", "mg/m2h1", "g/m2d1", "mg/m2d1"))){
      stop("output.units must be specified as 'g/m2h1', 'mg/m2h1', 'g/m2d1', or 'mg/m2d1'")
    }
    
    if(!(species %in% c("CO2", "CH4")) & C.only == TRUE){
      stop("'C.only' can only be calculated if species is CO2 or CH4")
    }
    
    
    molarMasses <- 
      readr::read_csv("speciesList, mass
                        CO2, 44.01
                        CH4, 16.04
                        N2O, 44.013")
    
    gasConstant_cm3Pa_Kmol <- 8314459.8
    molarMass_g_mol <- 
      molarMasses %>% 
      filter(speciesList == species) %>% 
      .[["mass"]]
    atomicWeightC_g_mol <- 12.011
    scalar <- ifelse(input.time == "seconds",
                     60*60,
                     60)
    unitConversion <- readr::read_csv({
      "unit, scaleFactor
      g/m2h1, 1
      mg/m2h1, 1000
      g/m2d1, 24
      mg/m2d1, 24000"
    })
    
    deltaGas_cm3 <- 
      1e-6 * delta.gas.ppm * chamber.volume.cm3
    
    molarVolume_cm3_mol <- 
      gasConstant_cm3Pa_Kmol * 
      temperature.k *
      1/pressure.pa
    
    if(C.only){
      output_g_m2h1 <- 
        scalar * 
        deltaGas_cm3 * 
        atomicWeightC_g_mol * 
        1/(0.0001 * collar.area.cm2) * 
        1/molarVolume_cm3_mol
    } else {
      output_g_m2h1 <- 
        scalar * 
        deltaGas_cm3 * 
        molarMass_g_mol * 
        1/(0.0001 * collar.area.cm2) * 
        1/molarVolume_cm3_mol
    }
    
    output <- output_g_m2h1 * unitConversion$scaleFactor[unitConversion$unit == output.units]
    
    return(output)
    }
