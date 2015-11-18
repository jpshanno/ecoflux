
# Blank Day ---------------------------------------------------------------

#' Creates a ITime vector spanning a single day with a custom time step
#'
#' #' Creates a ITime vector spanning a single day with a custom time step
#' @param timestemp The desired time step in minutes
#' @keywords ITime
#' @export
#' @examples
#' blank_day()
blank_day <- function(timestep = NULL){
  timestep_seconds = 60 * timestep
  sort(data.table::as.ITime(seq(from = data.table::as.ITime("00:00:00"), to = data.table::as.ITime("23:59:59"), by=timestep_seconds), origin = "1970-01-01 00:00:00"))
}

#' Calculate the area of a circle
#'
#' This function calculates the area of a circle given the radius
#' @param r Radius of the circle
#' @keywords Sap flux
#' @export
#' @examples
#' Circular()
Circular<-function(r) pi*r^2

# Predicted Standard Error ------------------------------------------------
#' Predicted Standard Error
#'
#' Calcualtes the standard error of predicted y-values
#' @param model Any fitted model.
#' @param Xes the x-values used in prediction
#' @keywords standard error
#' @export
#' @examples
#' SEy()
SEy <- function(model, Xes) {
  Ux <- mean(Xes, na.rm=T)
  Ey <- resid(model)
  Ex <- Xes-Ux
  n <- length(resid(model))
  s <- sqrt((sum(Ey^2))/(n-2))
  Sx <- sum((Ex)^2,na.rm=T)
  s * sqrt(1+(1/n)+(Ex^2/Sx))}

# Blank Sequence ----------------------------------------------------------
#' Create blank time sequence for each unique subject
#'
#' Creates a data table with subject, date (IDate), and time (ITime) columns over
#' a specified range of days with a given time step
#' @param from a start date as a character string ("yyyy-mm-dd") or an IDate object
#' @param to a start date as a character string ("yyyy-mm-dd") or an IDate object
#' @param subjects a character vector of subject IDs
#' @param timestep the desired time step in minutes
#' @export
#' @examples
#' blank_seq()

blank_seq <- function(from = NULL, to = NULL, subjects = NULL, timestep = NULL){

  dates <- seq(from = as.IDate(from), to = as.IDate(to), by =1)
  times <- blank_day(timestep = timestep)
  
  n_subs <- length(subjects)
  n_dates <- length(dates)
  n_times <- length(times)
  
  full_sequence <- 
    data.table(
      subject = 
        rep(
          subjects, 
          each = n_dates * n_times),
      idateA = 
        rep(
          dates,
          each = n_times,
          times = n_subs),
      itimeA = 
        rep(
          times,
          times = n_subs * n_dates)
    )
  
  setkey(full_sequence, subject, idateA, itimeA)
  
  return(full_sequence)
  }
