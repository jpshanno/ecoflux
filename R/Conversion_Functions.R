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
#' @param delta.ppm A vector of slopes representing the change in concentration 
#'   over time
#' @param chamber.volume.cm3 The gas chamber volume in cm^3; can be a vector of 
#'   the same length as delta.ppm or a single numeric value
#' @param collar.area The area of the chamber footprint in cm^2; can be a vector
#'   of the same length as delta.ppm or a single numeric value
#' @param temperature.k The temperature within the chamber in Kelvin; can be a 
#'   vector of the same length as delta.ppm or a single numeric value
#' @param pressure.Pa The pressure within the chamber in Pascals;; can be a 
#'   vector of the same length as delta.ppm or a single numeric value
#' @param species The gas species of interest; either 'CO2' or 'CH4'
#' @param input.time The time scale for delta.ppm; either 'seconds' or 'minutes'
#' @param ouput.units The desired output flux units; 'g/m2h1', 'mg/m2h1', 
#'   'g/m2d1', or 'mg/m2d1'; defaults to 'g/m2h1'
#' @param C.only A logical specificying if the output flux should be calculated 
#'   for the gas species or for carbon only; defaults to FALSE
#' @export
#' @rdname conc_to_flux

conc_to_flux <- 
  function(delta.ppm, 
           chamber.volume.cm3, 
           collar.area.cm2, 
           temperature.k, 
           pressure.Pa, 
           species, 
           input.time, 
           output.units = "g/m2d1", 
           C.only = FALSE){
    if(!all(sapply(list(delta.ppm, chamber.volume.cm3, collar.area.cm2, temperature.k, pressure.Pa), is.numeric))){
      stop("delta.ppm, chamber.volume, chamber.area, temperature, and pressure must be numeric vectors")
    }
    if(!(species %in% c("CO2", "CH4"))){
      stop("species must be specified as 'CO2' or 'CH4'")
    }
    if(!(input.time %in% c("seconds", "minutes"))){
      stop("input.time must be specified as 'minutes' or 'seconds'")
    }
    if(!(output.units %in% c("g/m2h1", "mg/m2h1", "g/m2d1", "mg/m2d1"))){
      stop("output.units must be specified as 'g/m2h1', 'mg/m2h1', 'g/m2d1', or 'mg/m2d1'")
    }
    
    gasConstant_cm3Pa_Kmol <- 8314459.8
    molarMass_g_mol <- ifelse(species == "CO2", 
                              44.01, 
                              16.04)
    atomicWeightC_g_mol <- 12.011
    scalar <- ifelse(input.time == "seconds",
                     60*60,
                     60)
    unitConversion <- read_csv({
      "unit, scaleFactor
      g/m2h1, 1
      mg/m2h1, 1000
      g/m2d1, 24
      mg/m2d1, 24000"
    })
    
    delta_cm3 <- 
      1e-6 * delta.ppm * chamber.volume.cm3
    
    molarVolume_cm3_mol <- 
      gasConstant_cm3Pa_Kmol * 
      temperature.k *
      1/pressure.Pa
    
    if(C.only){
      output_g_m2h1 <- 
        scalar * 
        delta_cm3 * 
        atomicWeightC_g_mol * 
        1/(0.0001 * collar.area.cm2) * 
        1/molarVolume_cm3_mol
    } else {
      output_g_m2h1 <- 
        scalar * 
        delta_cm3 * 
        molarMass_g_mol * 
        1/(0.0001 * collar.area.cm2) * 
        1/molarVolume_cm3_mol
    }
    
    output <- output_g_m2h1 * unitConversion$scaleFactor[unitConversion$unit == output.units]
    
    return(output)
    }
