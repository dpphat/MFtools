#' Link model target identification to PEST target identification as listed in 
#' Groundwater Vistas targpest.dat file
#' This function reads the targpest.dat file and gets the target ID based on the
#' PEST target ID. This assumes that the PEST target ID is in the format of o#, where `#` is the
#' target number. The first PEST target ID must be o1 and the preceding line in the targpest.dat
#' file needs to be the number of targets.
#' @param PATH This is the full file path to the targpest.dat file. 
#' When PATH is NA this function will look in the current working 
#' directory for the targpest.dat file.
#' @export

targid <- function(PATH = NA){
    if(is.na(PATH)){
        PATH <- getwd()
    }
    infl <- paste(PATH, "targpest.dat", sep = "\\")
    linin <- readr::read_lines(infl)
    tabloc <- grep("o1 ", linin)
    ntarg <- linin[tabloc - 1] %>% as.integer()
    targid <- linin[tabloc:(tabloc + ntarg - 1)] %>% 
              stringr::str_trim(side = "both") %>%
              stringr::str_split_fixed("\\s+", n = 12) %>%
              tibbles::as_tibble() %>%
              dplyr::select(V1, V2) %>%
              dplyr::rename(TARG_ID = V1, PEST_ID = V2)
    return(targid)
    }