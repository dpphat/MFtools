 #' Write MT3DMS .ucn File
#'
#' This function writes a ucn dataframe created in R (i.e., from readucn) to a binary ucn file
#' composed of the following elements:
#' 
#' writeucn(ucn, ofl)
#' @param ucn is the ucn data frame
#' @param ofl is the output ucn file name
#' @export
#' @examples
#' writeucn(ucn = u, ofl = "T04R1.ucn")

writeucn <- function(ucn, ofl){

    NTTS <- ucn$TIME %>% data.frame() %>% dplyr::distinct() %>% nrow()
    NLAY <- ucn$LAY %>% data.frame() %>% dplyr::distinct() %>% nrow()
    NC <- ucn$COL %>% data.frame() %>% dplyr::distinct() %>% nrow()
    NR <- ucn$ROW %>% data.frame() %>% dplyr::distinct() %>% nrow()
    FROM <- outer(((1:NLAY - 1) * NR * NC + 1), ((1:NTTS - 1) * NLAY * NC * NR), FUN = "+") %>% as.vector()
    TO <- FROM + NR * NC - 1
    INDX <- rep(1:NTTS * NLAY * NR * NC, each = NLAY)
    LAY <- rep(1:NLAY, NTTS)
    obin <- file(ofl, "wb")
    for(j in 1:(NTTS * NLAY)){
            op1   <- c(ucn$TRANS[INDX[j]] %>% as.integer(), 
                       ucn$STP[INDX[j]] %>% as.integer(), 
                       ucn$PER[INDX[j]] %>% as.integer())
            op2   <- ucn$TIME[INDX[j]] %>% as.double()
            opTxt <- "CONCENTRATION   " 
            op3   <- c(NC %>% as.integer(), 
                       NR %>% as.integer(), 
                       LAY[j] %>% as.integer()) 
            op4   <- ucn$CONC[FROM[j]:TO[j]] %>% as.double()
            writeBin(op1, obin)
            writeBin(op2, obin, size = 4)
            writeChar(opTxt, obin, nchars = 16, eos = NULL)
            writeBin(op3, obin)
            writeBin(op4, obin, size = 4)
    }
    close(obin)

}