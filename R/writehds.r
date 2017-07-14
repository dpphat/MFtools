#' Write MT3DMS .hds File
#'
#' This function writes a hds dataframe created in R (i.e., from readhds) to a binary hds file
#' composed of the following elements:
#' 
#' writehds(hds, ofl)
#' @param hds is the hds data frame
#' @param ofl is the output hds file name
#' @export
#' @examples
#' writehds(hds = h, ofl = "T04R1.hds")

writehds <- function(hds, ofl){

    NTTS <- hds$TIME %>% data.frame() %>% distinct() %>% nrow()
    NLAY <- hds$LAY %>% data.frame() %>% distinct() %>% nrow()
    NC <- hds$COL %>% data.frame() %>% distinct() %>% nrow()
    NR <- hds$ROW %>% data.frame() %>% distinct() %>% nrow()
    FROM <- outer(((1:NLAY - 1) * NR * NC + 1), ((1:NTTS - 1) * NLAY * NC * NR), FUN = "+") %>% as.vector()
    TO <- FROM + NR * NC - 1
    INDX <- rep(1:NTTS * NLAY * NR * NC, each = NLAY)
    LAY <- rep(1:NLAY, NTTS)
    obin <- file(ofl, "wb")
    for(j in 1:(NTTS * NLAY)){
    		op1   <- c(hds$TRANS[INDX[j]] %>% as.integer(), 
    		         hds$STP[INDX[j]] %>% as.integer(), 
    		 		 hds$PERLEN[INDX[j]] %>% as.integer())
    		op2   <- hds$TIME[INDX[j]] %>% as.double()
    		opTxt <- "GW ELEVATION    " 
    		op3   <- c(NC %>% as.integer(), 
    				 NR %>% as.integer(), 
    				 LAY[j] %>% as.integer()) 
    		op4   <- hds$GWE[FROM[j]:TO[j]] %>% as.double()
    		writeBin(op1, obin)
    		writeBin(op2, obin, size = 4)
    		writeChar(opTxt, obin, nchars = 16, eos = NULL)
    		writeBin(op3, obin)
    		writeBin(op4, obin, size = 4)
    }
    close(obin)

}