#' Get the dimensions of a MODFLOW Model without reading the entire .dis file
#' 
#' This function is used for extracting the number of rows, columns, layers, stress periods of a model
#' without reading the entire discritization file
#' It can be used as a utility function for the other MFtools functions
#' 
#' @param rootname This is the root name of the dis file
#' @export
#' @examples
#' get_dims(rootname)
#' # A tibble: 1 x 4
#'    NLAY  NROW  NCOL  NPER
#'   <int> <int> <int> <int>
#' 1    34   328   291     1
#' 
#' D <- get_dims(rootname)
#' D$NLAY
#' [1] 34
#' D$NCOL
#' [1] 291
#' D$NLAY * D$NCOL
#' [1] 9894

get_dims <- function(rootname){
    infl <- paste0(rootname, ".dis")
    linin <- read_lines(infl, n_max = 10) %>% .[-grep("#", .)]              # READ IN DIS FILE BUT REMOVE COMMENTED LINES
                                                                # THIS IS IN PREPARATION FOR MODFLOW 6
    indx <- 1
    NLAY <- linin[indx] %>% parse_MF_FW_ELMT(1) %>% as.integer()
    NROW <- linin[indx] %>% parse_MF_FW_ELMT(2) %>% as.integer()
    NCOL <- linin[indx] %>% parse_MF_FW_ELMT(3) %>% as.integer()
    NPER <- linin[indx] %>% parse_MF_FW_ELMT(4) %>% as.integer()
    DIMS <- data_frame(NLAY = NLAY, 
                       NROW = NROW, 
                       NCOL = NCOL, 
                       NPER = NPER)
    return(DIMS)
    }