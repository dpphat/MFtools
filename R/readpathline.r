#' Read the MODPATH pathline file and create a tibble of the contents
#' 
#' This function reads in the MODPATH pathline file and generates a tibble of the contents.
#' The tibble includes the following columns
#' \describe{
#' \item{PART}{Particle Index Number}
#' \item{GLOBAL_X}{Global coordinate in the x-direction}
#' \item{GLOBAL_Y}{Global coordinate in the y-direction}
#' \item{LOCAL_Z}{Local coordinate in the z-direction within the cell}
#' \item{GLOBAL_Z}{Global coordinate in the z-direction}
#' \item{CUMULATIVE_TIME}{Cumulative tracking time}
#' \item{COL}{J index of cell containing the point (i.e., Column Number)}
#' \item{ROW}{I index of cell containing the point (i.e., Row Number)}
#' \item{LAY}{K index of cell containing the point (i.e., Layer Number)}
#' \item{TIME_STEP}{Cumulative MODFLOW time step number}
#' }
#' @param PATH This is the full file path to the pathline file. 
#' When PATH is NA this function will look in the current working 
#' directory for the pathline file.
#' @export

readpathline <- function(PATH = NA){
    if(is.na(PATH)){
            PATH <- getwd()
        }
    pthfl <- paste(PATH, "pathline", sep = "\\")
    pth <- read.table(pthfl, skip = 1, 
                      col.names = c("PART", 
                                    "GLOBAL_X", 
                                    "GLOBAL_Y", 
                                    "LOCAL_Z", 
                                    "GLOBAL_Z", 
                                    "CUMULATIVE_TIME", 
                                    "COL", 
                                    "ROW", 
                                    "LAY", 
                                    "TIME_STEP")) %>% 
           as_tibble()
    return(pth)
}