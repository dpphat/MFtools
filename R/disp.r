#' Calculate the longitudinal dispersivity value based on the approach developed by Xu and Eckstein (1995)
#' 
#' This function takes plume length (PLUME_L) and length units (UNITS) as arguments to calculate 
#' longitudinal dispersivity (α_L). Length units can be meters or feet. The function will return the 
#' longitudinal dispersivity in the same length units as plume length.
#' 
#' disp(PLUME_L, UNITS)
#' @param PLUME_L is the plume length
#' @param UNITS is the length unit for plume length and longitudinal dispersivity
#' @export
#' @examples
#' disp(PLUME_L = 880, UNITS = "feet")
#' [1] 23.18827

disp <- function(PLUME_L = 1, UNITS = "feet"){
    CONV    <- ifelse(toupper(UNITS) %in% c("FEET", "FT"), 1 * 12 * 2.54 / 100, 
               ifelse(toupper(UNITS) %in% c("METER", "METERS", "M"), 1, NA))
    ALPHA   <- 0.83 * log10(PLUME_L * CONV)^2.414 / CONV
    return(ALPHA)
}