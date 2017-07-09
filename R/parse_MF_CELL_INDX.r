#' Parse the elements of a line of a fixed width MODFLOW file
#' 
#' This function is used for extracting all of the elements of a single line from a fixed width MODFLOW file
#' It is being used as a utility function for the other MFtools functions
#' 
#' @param LINE this is the individual fixed width MODFLOW file line
#' from the MODFLOW fixed width file
#' @export
#' @exmaples
#' CELL_INDX    <- Map(seq, BLOCK_START, BLOCK_END)
#' VAL <- CELL_INDX %>% parse_MF_CELL_INDX(LINE = linin)

parse_MF_CELL_INDX <- function(INDX = CELLINDX, LINE = inline){
    INDX[4:length(INDX)] %>% 
    unlist() %>% 
    .[. > 0] %>% 
    LINE[.] %>% 
    stringr::str_split("\\s+") %>% 
    unlist() %>% 
    subset(. != "")
}