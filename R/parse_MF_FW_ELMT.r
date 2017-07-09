#' Parse the elements of a line of a fixed width MODFLOW file
#' 
#' This function is used for extracting individual elements of a single line from a fixed width MODFLOW file
#' It is being used as a utility function for the other MFtools functions
#' 
#' @param LINE this is the individual fixed width MODFLOW file line
#' @param ELEMENT this is the integer number of the element the user wishes to select
#' from the MODFLOW fixed width file
#' @export
#' @examples
#' infl <- paste0(rootname, ".dis")
#' linin <- read_lines(infl) %>% .[-grep("#", .)]              # READ IN DIS FILE BUT REMOVE COMMENTED LINES
#'                                                             # THIS IS IN PREPARATION FOR MODFLOW 6
#' indx <- 1
#' NLAY <- linin[indx] %>% parse_MF_FW_ELMT(1) %>% as.integer()
#' NLAY
#' [1] 15

parse_MF_FW_ELMT <- function(LINE = inline, ELEMENT = 1){
    LINE %>% stringr::str_split("\\s+") %>% unlist() %>% subset(. != "") %>% .[[ELEMENT]]
}