#' Creates a ITime vector spanning a single day with a custom time step
#'
#' #' Creates a ITime vector spanning a single day with a custom time step
#' @param timestemp The desired time step in minutes
#' @keywords ITime
#' @export
#' @examples
#' blank_day()
blank_day <- function(timestep = NULL){
  timestep_sec = 60 * timestep
  sort(as.ITime(seq(from = as.ITime("00:00:00"), to = as.ITime("23:45:00"), by=timestep_sec), origin = "1970-01-01 00:00:00"))
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
