#' Calculate sap flow for a tree with sapwood depth greater than probe length
#'
#' This function uses the equations laid out in Pataki et al, 2011.
#' @param Flux Sap flux data (m3m-2d-1)
#' @param Sapwood_Area Sapwood area of the tree (m2)
#' @param Diameter of the tree at breast height (m2)
#' @param Type: 'Softwood' or 'Hardwood'
#' @keywords Sap flux
#' @export
#' @examples
#' pataki_flow()
pataki_flow <- function (Flux, Sapwood_Area, DBH, Type) {
  Radius <- DBH/2
  Sapwood_Radius <- DBH/2-sqrt(((pi*(DBH/2)^2)-Sapwood_Area)/pi)
  if(Sapwood_Radius > 0.02){
    Rings <- Sapwood_Radius / 0.02
    Full_Rings <- ceiling(Rings)
    Last_Ring <- 1 - (Full_Rings - Rings)
    for (X in 1:Full_Rings) {
      OuterRad <- Radius - ((X * 0.02) - 0.02)
      InnerRad <- Radius - (X * 0.02)
      Relative_D <- ((X * 0.02) - 0.02) / Sapwood_Radius
      JiJo <- if(Type=="Softwood") {
        1.257 * exp(-0.5 * ((Relative_D + 0.3724) / 0.662) ^ 2)}
      else {
        1.033 * exp(-0.5 * ((Relative_D - 0.09963) / 0.4263) ^ 2)}
      Area <- Circular(OuterRad) - Circular(InnerRad)
      R_Flow <- Flux * JiJo * Area
      if(X == 1) {Flow <- R_Flow}
      else {
        if(X != Full_Rings) {Flow <- Flow + R_Flow}
        else {
          if(Last_Ring == 0) {Flow <- Flow + R_Flow}
          else {
            Flow <- Flow + Last_Ring * R_Flow}}}
    }
  }
  else{
    Flow <- Flux * Sapwood_Area}
  Flow
}

#' Calculate sap flow for a tree with sapwood depth greater than probe length
#'
#' This function calculates the variance associated with the sap flow calculated using the equations laid out in Pataki et al, 2011.
#' @param Flux Sap flux data
#' @param Var Variance of sap flux data
#' @param Sapwood_Radius Radius of the sapwood of the tree
#' @param Radius Radius of the tree at breast height (DBH/2)
#' @param Type Softwood or hardwood
#' @keywords Sap flux
#' @export
#' @examples
#' pataki_var()
pataki_var <- function (Flux, Var, Sapwood_Radius, Radius, Type) {
  StdD <- sqrt(Var)
  Rings <- Sapwood_Radius / 0.02
  Full_Rings <- ceiling(Rings)
  Last_Ring <- 1 - (Full_Rings - Rings)
  SigR <- ifelse(Type=="Softwood", 0.1714, 0.2583)
  for (X in 1:Full_Rings) {
    OuterRad <- Radius - ((X * 0.02) - 0.02)
    InnerRad <- Radius - (X * 0.02)
    Relative_D <- ((X * 0.02) - 0.02) / Sapwood_Radius
    JiJo <- if(Type=="Softwood") {
      1.257 * exp(-0.5 * ((Relative_D + 0.3724) / 0.662) ^ 2)}
    else {1.033 * exp(-0.5 * ((Relative_D - 0.09963) / 0.4263) ^ 2)}
    Area <- Circular(OuterRad) - Circular(InnerRad)
    AreaSD <- 0.02 * pi * (OuterRad ^ 2 + InnerRad ^ 2) ^ 0.5
    R_Flow <- Flux * JiJo * Area
    if(X == 1) {Variance <- R_Flow ^ 2 * ((AreaSD / Area) ^ 2 + (StdD / Flux) ^ 2 + (SigR / JiJo) ^ 2)}
    else {
      if(X != Full_Rings) {Variance <- Variance + R_Flow ^ 2 * ((AreaSD / Area) ^ 2 + (StdD / Flux) ^ 2 + (SigR / JiJo) ^ 2)}
      else {
        if(Last_Ring == 0) {Variance <- Variance + R_Flow ^ 2 * ((AreaSD / Area) ^ 2 + (StdD / Flux) ^ 2 + (SigR / JiJo) ^ 2)}
        else {
          Variance <- Variance + Last_Ring ^ 2 * R_Flow ^ 2 * ((AreaSD / Area) ^ 2 + (StdD / Flux) ^ 2 + (SigR / JiJo) ^ 2)}}}
  }
  Variance
}

#' Sap-flux response to Dz
#'
#' This function fits a curve to sap flux data using Dz as a predictor.  It is meant to be used within nlmer().
#' @param a,w,x fitting paramters which need start values specified in nlmer.
#' @param z Dz
#' @return The fitted values for a,w, and x for the equation \deqn{J[s]=a+w(1-x^{-D[z]})}
#' @keywords sap flux Dz
#' @export
#' @examples
#' fluxmod()
fluxmod <- deriv(expression(a+w*(1-x^(-z))), c("a","w","x"),function(a, w, x, z) {})
