#' Read MODFLOW .hds File
#'
#' This function reads in a binary hds file and creates a data frame
#' composed of the following columns:
#' \describe{
#' \item{TRANS}{Transport Time Step}
#' \item{STP}{Flow Time Step}
#' \item{PERLEN}{Stress Period Length}
#' \item{TIME}{Elapsed Time}
#' \item{LAY}{Model Layer}
#' \item{ROW}{Model Row}
#' \item{COL}{Model Column}
#' \item{GWE}{Groundwater Elevation}
#' }
#' @param rootname This is the rootname of the flow model (i.e., "F95")
#' @param NLAY This is the number of layers assigned to the model and needs to be 
#'             assigned
#' @param NTTS This is the number of transport time steps that are printed to the .hds file. This number
#' can be obtained from the .dis file
#' @export
#' @examples
#' readhds("F95", NLAY = 8, NTTS = 16)

readhds <- function(rootname, NLAY, NTTS){
    # NLAY IS THE NUMBER OF LAYERS IN THE MODEL
	# NTTS IS THE NUMBER OF TRANSPORT TIME STEPS IN THE MODEL
    hdsname <- paste(rootname, ".hds", sep = "")	
	to.read <- file(hdsname, "rb")	
	TRANS <- c()
	STP <- c()
	PERLEN <- c()
	TIME <- c()
	TEXT <- c()
	LAY <- c()
	GWE <- c()
	dat <- c()
    readblock <- function(){
	      TRANS <- readBin(to.read, integer(), n = 1)	
	      STP <- readBin(to.read, integer(), n = 1)
	      PERLEN <- readBin(to.read, double(), size = 4, n = 1)
	      TIME <- readBin(to.read, double(), size = 4, n = 1)
	      TEXT <- readChar(to.read, 16)
	      NC <- readBin(to.read, integer(), n = 1)
	      NR <- readBin(to.read, integer(), n = 1)
	      LAY <- readBin(to.read, integer(), n = 1)
	      GWE = readBin(to.read, double(), size = 4, n = NR * NC, endian = "little")
	out <- list(TRANS, STP, PERLEN, TIME, TEXT, NC, NR, LAY, GWE)
	return(out)			
    }	
	for(Q in 1:NTTS){
	    for(K in 1:NLAY){
		    dat[[length(dat) + 1]] <- readblock()
		}
	}	
	close(to.read)
	TRANS  <- sapply(dat, "[[", 1)
	STP    <- sapply(dat, "[[", 2)
	PERLEN <- sapply(dat, "[[", 3)
	TIME   <- sapply(dat, "[[", 4)
	LAY    <- sapply(dat, "[[", 8)
	NC     <- dat[[1]][6] %>% as.integer
	NR     <- dat[[1]][7] %>% as.integer
	GWE    <- sapply(dat, "[[", 9)
	HDS    <- tibble::data_frame(
	       TRANS = rep(TRANS, each = (NC * NR)) %>% as.integer(),
		   STP = rep(STP, each = (NC * NR)) %>% as.integer(),
		   PERLEN = rep(PERLEN, each = (NC * NR)) %>% as.numeric(), 
		   TIME = rep(TIME, each = (NC * NR)) %>% as.numeric(), 
		   LAY = rep(LAY, each = (NC * NR)) %>% as.integer(),
		   ROW = rep(rep(rep(1:NR, each = NC), NLAY), NTTS) %>% as.integer(), 
		   COL = rep(rep(rep(seq(1, NC, 1), NR), NLAY), NTTS) %>% as.integer(),
		   GWE = GWE %>% as.numeric()
	       )
    rm(TRANS)
    rm(STP)
    rm(PERLEN)
    rm(TIME)
    rm(TEXT)
    rm(NC)
    rm(NR)
    rm(LAY)
    rm(GWE)	
	gc()
    return(HDS)		
    }
	