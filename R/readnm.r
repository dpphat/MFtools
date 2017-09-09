#' Read the namefile of a MODFLOW Model
#' 
#' This function is used for extracting the Package ID, Unit Number, and Filename of a MODFLOW model
#' It can be used as a utility function for the other MFtools functions
#' 
#' @param rootname This is the root name of the lpf file
#' @export
#' @examples
#' readnm(rnm)
#' # A tibble: 47 x 3
#'         PACKAGE  UNIT       FILE
#'           <chr> <int>      <chr>
#'  1         LIST     7  F78RM.lst
#'  2         BAS6     1  F78RM.bas
#'  3          DIS    29  F78RM.dis
#'  4          LPF    11  F78RM.lpf
#'  5         ZONE    40 F78RM.zone
#'  6          WEL    12  F78RM.wel
#'  7          RCH    18  F78RM.rch
#'  8           OC    22   F78RM.oc
#'  9          GMG    19  F78RM.gmg
#' 10 DATA(BINARY)    50  F78RM.cbb
#' # ... with 37 more rows

readnm <- function(rootname = NA){
    if(is.na(rootname)){
        rootname <- getroot()
    }
    nmfl <- paste0(rootname, ".nam")
    mf_fls <- read.table(nmfl, header = FALSE, stringsAsFactors = FALSE) %>% 
              as_tibble() %>%
              dplyr::rename(PACKAGE = V1, 
                            UNIT = V2, 
                            FILE = V3)
    return(mf_fls)
    }