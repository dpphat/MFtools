#' Read MT3DMS .ucn File
#'
#' This function reads in a binary ucn file and creates a data frame
#' composed of the following elements:
#' \describe{
#' \item{TRANS}{Transport Time Step}
#' \item{STP}{Flow Time Step}
#' \item{PER}{Stress Period}
#' \item{TIME}{Elapsed Time}
#' \item{LAY}{Model Layer}
#' \item{ROW}{Model Row}
#' \item{COL}{Model Column}
#' \item{CONC}{Concentration}
#' }
#' @param ucname This is the name of the ucn file
#' @param NLAY This is the number of layers assigned to the model
#' @param NTTS This is the number of transport time steps that are printed to the .ucn file. This can be obtained from the .btn file. 
#' This will cause an error if the value assigned is greater than the correct value. Future versions will need to be developed so that NTTS is not
#' needed for this function to operate properly.
#' @export
#' @examples
#' readucn("MT3DMS001S", NLAY = 8, NTTS = 22)

readucn <- function(ucrootname, NLAY, NTTS){
    # NLAY IS THE NUMBER OF LAYERS IN THE MODEL
    # NTTS IS THE NUMBER OF TRANSPORT TIME STEPS IN THE MODEL
    ucname <- paste(ucrootname, ".ucn", sep = "")    
    to.read <- file(ucname, "rb")    
    TRANS <- c()
    STP <- c()
    PER <- c()
    TIME <- c()
    TEXT <- c()
    LAY <- c()
    CONC <- c()
    dat <- c()
    readblock <- function(){
            TRANS <- readBin(to.read, integer(), n = 1)    
            STP <- readBin(to.read, integer(), n = 1)
            PER <- readBin(to.read, integer(), n = 1)
            TIME <- readBin(to.read, double(), size = 4, n = 1)
            TEXT <- readChar(to.read, 16)
            NC <- readBin(to.read, integer(), n = 1)
            NR <- readBin(to.read, integer(), n = 1)
            LAY <- readBin(to.read, integer(), n = 1)
            CONC <- readBin(to.read, double(), size = 4, n = NR * NC, endian = "little")
            out <- list(TRANS, STP, PER, TIME, TEXT, NC, NR, LAY, CONC)
            return(out)            
        }    
    for(Q in 1:NTTS){
        for(K in 1:NLAY){
            dat[[length(dat) + 1]] <- readblock()
        }
    }
    close(to.read)
    TRANS <- sapply(dat, "[[", 1)
    STP <- sapply(dat, "[[", 2)
    PER <- sapply(dat, "[[", 3)
    TIME <- sapply(dat, "[[", 4)
    LAY <- sapply(dat, "[[", 8)
    NC <- dat[[1]][6] %>% as.integer
    NR <- dat[[1]][7] %>% as.integer
    CONC <- sapply(dat, "[[", 9)
    UCN <- tibble::data_frame(
           TRANS = rep(TRANS, each = (NC * NR)) %>% as.integer(),
           STP = rep(STP, each = (NC * NR)) %>% as.integer(),
           PER = rep(PER, each = (NC * NR)) %>% as.integer(), 
           TIME = rep(TIME, each = (NC * NR)) %>% as.double(),  
           LAY = rep(LAY, each = (NC * NR)) %>% as.integer(),
           ROW = rep(rep(rep(1:NR, each = NC), NLAY), NTTS) %>% as.integer(), 
           COL = rep(rep(rep(seq(1, NC, 1), NR), NLAY), NTTS) %>% as.integer(),
           CONC = CONC %>% as.double()
           )
    rm(TRANS)
    rm(STP)
    rm(CONC)
    rm(LAY)
    rm(NR)
    rm(NC)
    rm(TEXT)
    rm(TIME)
    rm(PER)    
    rm(dat)
    gc()
    return(UCN)        
    }