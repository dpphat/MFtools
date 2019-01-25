#' Plot the Observed Versus Simulated plot based on 
#' the PEST .res file
#' 
#' This function reads in the PEST .res file and generates an Observed
#' Versus Simulated plot
#' @param PATH This is the full file path to the .res file. 
#' When PATH is NA this function will look in the current working 
#' directory for the .res file.
#' @export
ggres <- function(PATH = NA){
    if(is.na(PATH)){
            PATH <- getwd()
        }
    RES <- suppressWarnings(MFtools::readpstres(PATH))
    SD  <- sd(RES$`Weight*Residual`)
    LBL <- RES %>% dplyr::filter(abs(Residual) > SD)
    RANGE <- max(RES$Modelled, RES$Measured) - min(RES$Modelled, RES$Measured)
    P <- RES %>% ggplot2::ggplot(aes(x = Measured)) +
                 ggplot2::coord_fixed() +
                 ggplot2::geom_abline(aes(intercept = 0, slope = 1), 
                                      size = 1) +
                 ggplot2::geom_abline(aes(intercept = -SD, slope = 1), 
                                      lty = 4, size = 1, colour = "red") +
                 ggplot2::geom_abline(aes(intercept = SD, slope = 1), 
                                     lty = 4, size = 1, colour = "red") +
                 ggplot2::geom_point(aes(y = Modelled), 
                                     size = 2, 
                                     colour = "blue", 
                                     alpha = 0.5) + 
                 ggplot2::geom_text_repel(data = LBL, aes(label = TARG_ID, y = Modelled), 
                                    nudge_x = 0.025 * RANGE) + 
                 ggplot2::labs(x = "Observed", y = "Simulated") + 
                 ggplot2::theme_bw()
         print(P)
     }