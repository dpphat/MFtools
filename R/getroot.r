#' Get the rootname of a MODFLOW Model in a working directory
#' 
#' This function is used for extracting the rootname of a MODFLOW model
#' It can be used as a utility function for the other MFtools functions
#' 
#' @export
#' @examples
#' rtnm <- getroot()
#' rtnm
#' [1] "F78RM"
#' 

getroot <- function(){
mf_fl <- list.files(pattern = ".in") %>% .[grep("mf2", .)]
nmfl  <- readLines(mf_fl) %>% gsub("\\.nam$", "", .)
return(nmfl)
}