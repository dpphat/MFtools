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
    RES <- suppressWarnings(readpstres(PATH))
    SD  <- sd(RES$`Weight*Residual`)
    LBL <- RES %>% filter(abs(Residual) > SD)
    RANGE <- max(RES$Modelled, RES$Measured) - min(RES$Modelled, RES$Measured)
    P <- RES %>% ggplot(aes(x = Measured)) +
                 coord_fixed() +
                 geom_abline(aes(intercept = 0, slope = 1), 
                             size = 1) +
                 geom_abline(aes(intercept = -SD, slope = 1), 
                             lty = 4, size = 1, colour = "red") +
                 geom_abline(aes(intercept = SD, slope = 1), 
                             lty = 4, size = 1, colour = "red") +
                 geom_point(aes(y = Modelled), 
                            size = 2, 
                            colour = "blue", 
                            alpha = 0.5) + 
                 geom_text(data = LBL, aes(label = Name, y = Modelled), 
                           nudge_x = 0.025 * RANGE) + 
                 labs(x = "Observed", y = "Simulated") + 
                 theme_bw()
         print(P)
     }