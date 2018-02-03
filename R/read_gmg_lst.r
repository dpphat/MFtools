#' Extract Head-Change Values and Cell Locations from the .lst file
#' 
#' This function extracts the Head-Change value and the locations of the cells exhibiting the
#' maximum head-change value from the .lst file.
#' 
#' @param LISTFL This is the full name (including filepath) of the .lst file.
#' @export

read_gmg_lst <- function(LISTFL){
    linin <- read_lines(LISTFL) %>% .[-grep("#", .)]
    HDCHNG_LOC <- grep("MAX HEAD CHANGE   ", linin) # THIS WILL EXCLUDE CELL LOCATIONS
    HDCHNG_CELL_LOC <- grep("MAX HEAD CHANGE AT", linin)
    
    HDCHNG_VAL  <- linin[HDCHNG_LOC] %>% gsub("^.*\\:", "", .) %>% parse_double()
    HDCHNG_CELL <- as_tibble(linin[HDCHNG_CELL_LOC] %>% 
                   gsub("^.*\\:", "", .)) %>% 
                   separate(value, into = c("ITER", "COL", "ROW", "LAY"), sep = "[^[:digit:]]+", convert = TRUE, extra = "drop") %>%
                   mutate(ITER = row_number())
    out         <- dplyr::bind_cols(HDCHNG_CELL, data_frame(HDCHNG = HDCHNG_VAL))
    return(out)
    }