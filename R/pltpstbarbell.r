#' Plot a barbell plot of the observed (blue) and simulated (red) groundwater elevations as listed
#' in the pest .res file. This function requires that both a .res file and targpest.dat file are
#' located in the PATH directory.
#' 
#' @param PATH This is the full file path to the .res and targpest.dat files. They need to be located in the same directory.
#' @param MAIN This is text to be printed as the main figure title
#' @param colour_obs the colour of the observed groundwater elevation
#' @param colour_sim the colour of the simulated groundwater elevation
#' @param colour the colour of the bar
#' @param size_obs the size of the observed groundwater elevation
#' @param size_sim the size of the simulated groundwater elevation
#' @param size the size of the bar
#' @param alpha the level of transparency of the ends
#' @export

pltpstbarbell <- function(PATH = NA, 
                          MAIN = NA, 
                          colour = "#a3c4dc", 
                          colour_obs = "blue", 
                          colour_sim = "red", 
                          size_obs = 3, 
                          size_sim = 3, 
                          alpha = 0.6, 
                          size = 1, 
                          xlab = "", 
                          ylab = ""){
    if(is.na(PATH)){
        PATH <- getwd()
    }
    
    res  <- MFtools::readpstres(PATH) %>%
            dplyr::mutate(SQUARE_RES = `Weight*Residual`^2)
            
    max_val <- round(max(max(res$Measured, na.rm = TRUE), max(res$Modelled, na.rm = TRUE), na.rm = TRUE) / 10, 0) * 10
    min_val <- round(min(min(res$Measured, na.rm = TRUE), min(res$Modelled, na.rm = TRUE), na.rm = TRUE) / 10, 0) * 10
    range_val <- max_val - min_val
    if(range_val > 100){
        maj_div <- 20
        min_div <- 5
    }else if(range_val > 25 & range_val <= 100){
        maj_div <- 10
        min_div <- 1
    }else if(range_val > 5 & range_val <= 25){
        maj_div <- 1
        min_div <- 0.1
    }else if(range_val <= 5){
        maj_div <- 0.1
        min_div <- 0.01
    }
    
    STATS <- res %>% dplyr::summarise(SSQ = sum(SQUARE_RES), 
                                      MEAN_RES = mean(`Weight*Residual`, na.rm = TRUE), 
                                      MEAN_ABS_RES = mean(abs(`Weight*Residual`), na.rm = TRUE), 
                                      ST_DEV = sd(`Weight*Residual`, na.rm = TRUE), 
                                      SCALED_ST_DEV = ST_DEV / (max(Measured, na.rm = TRUE) - min(Measured, na.rm = TRUE)))
    
    p <- res %>% ggplot2::ggplot(aes(y = reorder(TARG_ID, -Measured), x = Measured, xend = Modelled)) +
                 ggalt::geom_dumbbell(size = size, 
                                      colour = colour, 
                                      alpha = alpha, 
                                      size_x = size_obs, 
                                      size_xend = size_sim, 
                                      colour_x = colour_obs, 
                                      colour_xend = colour_sim) +
                 ggplot2::ylab(xlab) +
                 ggplot2::scale_x_continuous(limits = c(min_val, max_val), 
                                             breaks = seq(min_val, max_val, maj_div), 
                                             minor_breaks = seq(min_val, max_val, min_div), 
                                             expand = c(0, 0), 
                                             name = ylab) +
                 ggplot2::coord_flip() +
                 ggplot2::geom_label(x = max_val, y = nrow(res), 
                                     label = paste0("Sum of Squared Residuals: ", round(STATS$SSQ[[1]], 1), "\n", 
                                                   "Mean Residual: ", round(STATS$MEAN_RES[[1]], 1), "\n", 
                                                   "Mean Absolute Residual: ", round(STATS$MEAN_ABS_RES[[1]], 1), "\n", 
                                                   "Standard Deviation: ", round(STATS$ST_DEV[[1]], 1), "\n", 
                                                   "Scaled Standard Deviation: ", round(STATS$SCALED_ST_DEV[[1]], 3)), 
                           family = "sans", 
                           fontface = "plain",
                           hjust = 1, 
                           vjust = 1) +
                 ggplot2::labs(title = MAIN) +
                 ggplot2::theme_bw() +
                 ggplot2::theme(axis.text.x = element_text(angle = 90), 
                                legend.position = "bottom")
    return(p)
}