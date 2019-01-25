#' Read PEST .res file
#' 
#' This function reads in the PEST .res file
#' @param PATH This is the full file path to the .res file. 
#' When PATH is NA this function will look in the current working 
#' directory for the .res file.
#' @export
readpstres <- function(PATH = NA){
    if(is.na(PATH)){
        PATH <- getwd()
    }
    resfl <- list.files(PATH, "\\.res", full.names = TRUE)
    out <- readr::read_table(resfl, col_types = readr::cols()) %>% 
           dplyr::rename(PEST_ID = Name) %>%
           dplyr::left_join(targid(PATH), by = "PEST_ID")
    return(out)
    }