#' Read a no-flow matrix text file
#'
#' This function reads a no-flow matrix text file. It first reads the model .dis file to
#' obtain the dimensions of the finite-diference grid.
#' 
#' readnoflow(FILE, rootname)
#' @param FILE is the name of the no-flow matrix file
#' @param rootname is the rootname of the groundwater flow model.
#' This will be used to find and read the discretization file
#' @export
#' @examples
#' readnoflow(FILE = NF.dat, rootname = "FF_INIT")

readnoflow <- function(FILE, rootname){
    infl <- FILE    
    d <- readdis(rootname)
    conn <- file(infl, open = "r")
    linin <- readLines(conn)
    close(conn)
    NFin <- vector(mode = "integer", length = d$NCOL * d$NROW * d$NLAY)
    NFin <- linin[seq(1:(d$NROW * d$NLAY))] %>%
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()
    NF <- tibble::data_frame(
                  LAY = rep(1:d$NLAY, each = d$NCOL * d$NROW) %>% as.integer(), 
                  ROW = rep(rep(1:d$NROW, each = d$NCOL), d$NLAY) %>% as.integer(), 
                  COL = rep(rep(seq(1, d$NCOL, 1), d$NROW), d$NLAY) %>% as.integer(), 
                  NF = NFin %>% as.integer())
    return(NF)
}
