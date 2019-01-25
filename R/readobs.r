#' Read the .obs MT3DMS output file
#' 
#' This function reads the .obs file and generates a tibble data frame of concentrations over time and step at each observation
#' well location. Column headings represent the Layer, Row, Column indices of the observation wells.
#' @param INFL This is the full path name of the .obs file.
#' @param NOBS This is the number of observation wells in the .obs file. This value tells readobs how to parse the .obs table.
#' @export

readobs <- function(INFL = NA, NOBS){
    OBS_ROWS <- ceiling(NOBS/16)
    LAST_LINE <- 16 - (OBS_ROWS * 16 - NOBS)
    obs_hdgs <- c("STEP", "TOTAL_TIME", 
                  suppressWarnings(read.fwf(INFL, skip = 1, n = 1, widths = c(rep(list(c(-17, rep(14, 16))), 3), list(c(-17, rep(14, LAST_LINE)))))) %>% 
                  gplyr::gather("BLANK", "OBS", 1:length(.)) %>% 
                  dplyr::select(OBS) %>% 
                  .[["OBS"]])
                  
    if(OBS_ROWS > 2){
       REP <- c(list(c(6, 14, rep(14, 16))), 
                rep(list(c(-20, rep(14, 16))), OBS_ROWS - 2), 
                list(c(-20, rep(14, LAST_LINE))))
                }else if(OBS_ROWS == 2){
       REP <- c(list(c(6, 14, rep(14, 16))), 
                list(c(-20, rep(14, 16))))
                }else if(OBS_ROWS == 1){
       REP <- c(6, 14, rep(14, NOBS))
                }
    
    obs <- suppressWarnings(read.fwf(INFL, skip = OBS_ROWS + 1, widths = REP)) %>% tibble::as_tibble()
    colnames(obs) <- obs_hdgs
    return(obs)
    }
