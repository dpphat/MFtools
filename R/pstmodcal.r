#' Report model diagnostic statistics based on the PEST .res file.
#' This function requires a PEST .res file to be located in the PATH directory.
#' 
#' @param PATH this is the full file path to the .res file.
#' @export

pstmodcal <- function(PATH){

    if(is.na(PATH)){
        PATH <- getwd()
    }
    
    res  <- MFtools::readpstres(PATH) %>%
            dplyr::mutate(SQUARE_RES = `Weight*Residual`^2)

    max_obs <- max(res$Measured, na.rm = TRUE)
    min_obs <- min(res$Measured, na.rm = TRUE)
    range_val <- max_obs - min_obs

    STATS <- res %>% dplyr::summarise(MEAN_RES = mean(`Weight*Residual`, na.rm = TRUE), 
                                      MEAN_ABS_RES = mean(abs(`Weight*Residual`), na.rm = TRUE), 
                                      ST_DEV = sd(`Weight*Residual`, na.rm = TRUE), 
                                      SSQ = sum(SQUARE_RES), 
                                      RMSE = mean(SQUARE_RES)^0.5, 
                                      MIN = min(`Weight*Residual`, na.rm = TRUE), 
                                      MAX = max(`Weight*Residual`, na.rm = TRUE), 
                                      nOBS = n(), 
                                      RANGE = range_val, 
                                      SCALED_ST_DEV = ST_DEV / range_val, 
                                      SCALED_MEAN_ABS_RES = MEAN_ABS_RES / range_val, 
                                      SCALED_RMSE = RMSE / range_val, 
                                      SCALED_MEAN_RES = MEAN_RES / range_val)
    return(STATS)
    }