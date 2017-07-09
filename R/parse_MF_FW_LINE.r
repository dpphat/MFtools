#' Parse the elements of a line of a fixed width MODFLOW file
#' 
#' This function is used for extracting all of the elements of a single line from a fixed width MODFLOW file
#' It is being used as a utility function for the other MFtools functions
#' 
#' @param LINE this is the individual fixed width MODFLOW file line
#' from the MODFLOW fixed width file
#' @export
#' @exmaples
#' dY <- linin[indx + seq(1:BLOCKEND) - 1] %>% parse_MF_FW_LINE()

parse_MF_FW_LINE <- function(LINE = inline){
    LINE %>% stringr::str_split("\\s+") %>% unlist() %>% subset(. != "")
}