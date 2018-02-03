#' Extract Head-Change Values and Cell Locations from the .lst file
#' 
#' This function extracts the Head-Change value and the locations of the cells exhibiting the
#' maximum head-change value from the .lst file.
#' 
#' @param LISTFL This is the full name (including filepath) of the .lst file.
#' @export

read_nwt_lst <- function(LISTFL){
    linin <- read_lines(LISTFL) %>% .[-grep("#", .)]
    HDCHNG_LOC_STRT <- grep("Residual-Control", linin) 
    HDCHNG_LOC_END <- lead(HDCHNG_LOC_STRT) - 2
    HDCHNG_LOC_END[length(HDCHNG_LOC_END)] <- grep("NWT REQUIRED", linin) - 3
    HDCHNG_LOCS <- purrr::map2(HDCHNG_LOC_STRT + 1, HDCHNG_LOC_END, ~seq(.x, .y)) %>% unlist()
    COL_NAMES <- linin[HDCHNG_LOCS[1] - 1] %>%
                    str_trim() %>% 
                    strsplit("\\s+") %>% 
                    unlist()
                    
    COL_NAMES[COL_NAMES == "Column"][1] <- "COL"
    COL_NAMES[COL_NAMES == "Row"][1]    <- "ROW"
    COL_NAMES[COL_NAMES == "Layer"][1]  <- "LAY"
    COL_NAMES[COL_NAMES == "Column"]    <- "MAX_FLUX_COL"
    COL_NAMES[COL_NAMES == "Row"]       <- "MAX_FLUX_ROW"
    COL_NAMES[COL_NAMES == "Layer"]     <- "MAX_FLUX_LAY"
    COL_NAMES[COL_NAMES == "Outer-Iter."]     <- "ITER"
    COL_NAMES[COL_NAMES == "Inner-Iter."]     <- "INNER_ITER"
    COL_NAMES[COL_NAMES == "Max.-Head-Change"]     <- "HDCHNG"
    COL_NAMES[COL_NAMES == "Max.-Flux-Residual"]     <- "FLXCHNG"
    
    HDCHNG <- linin[HDCHNG_LOCS] %>% 
                    str_trim() %>% 
                    str_split_fixed("\\s+", n = Inf) %>% 
                    as_tibble()
    colnames(HDCHNG) <- COL_NAMES
                    
    out <- HDCHNG %>% mutate(`Residual-Control` = as.numeric(`Residual-Control`), 
                             ITER = as.integer(ITER), 
                             INNER_ITER = as.integer(INNER_ITER), 
                             COL = as.integer(COL), 
                             ROW = as.integer(ROW), 
                             LAY = as.integer(LAY), 
                             MAX_FLUX_COL = as.integer(MAX_FLUX_COL), 
                             MAX_FLUX_ROW = as.integer(MAX_FLUX_ROW), 
                             MAX_FLUX_LAY = as.integer(MAX_FLUX_LAY), 
                             HDCHNG = as.numeric(HDCHNG), 
                             FLXCHNG = as.numeric(FLXCHNG), 
                             `L2-New` = as.numeric(`L2-New`), 
                             `L2-Old` = as.numeric(`L2-Old`), 
                             `Solver-Max-Delh` = as.numeric(`Solver-Max-Delh`))
    return(out)
}