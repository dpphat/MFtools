#' Extract Head-Change Values and Cell Locations from the .lst file
#' 
#' This function extracts the Head-Change value and the locations of the cells exhibiting the
#' maximum head-change value from the .lst file.
#' 
#' @param LISTFL This is the full name (including filepath) of the .lst file.
#' @export

read_pcg_lst <- function(LISTFL){
    linin <- readr::read_lines(LISTFL) %>% .[-grep("#", .)]
    HDCHNG_LOC <- grep("MAXIMUM HEAD CHANGE FOR EACH ITERATION", linin) + 5# THIS WILL EXCLUDE CELL LOCATIONS
    HDCHNG_LOC_END <- grep("MAXIMUM RESIDUAL FOR EACH ITERATION", linin) - 1
    hdchng_array <- linin[seq(HDCHNG_LOC, HDCHNG_LOC_END, by = 2)] %>% 
                    stringr::str_trim() %>% 
                    strsplit("\\s+") %>% 
                    unlist()
    IN_OUT <- MFtools::hdchng_array[seq(1, length(hdchng_array), by = 2)] %>% as.integer()
    HDCHNG_VAL <- MFtools::hdchng_array[seq(2, length(hdchng_array), by = 2)] %>% as.numeric()
    hdchng_cell_array <- linin[seq(HDCHNG_LOC + 1, HDCHNG_LOC_END, by = 2)] %>% 
                         stringr::str_trim() %>% 
                         strsplit(")") %>% 
                         unlist()

    HDCHNG_CELL <- tibble::as_tibble(hdchng_cell_array %>% 
                                     gsub("^.*\\:", "", .)) %>% 
                                     tidyr::separate(value, into = c("ITER", "LAYER", "ROW", "COL"), sep = "[^[:digit:]]+", convert = TRUE, extra = "drop") %>%
                                     dplyr::mutate(ITER = row_number())
    out         <- dplyr::bind_cols(HDCHNG_CELL, data_frame(HDCHNG = HDCHNG_VAL, INNER_OUTER = IN_OUT))
}