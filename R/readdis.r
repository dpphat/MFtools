#' Read MODFLOW .dis File
#'
#' This function reads in a dis file and creates a list
#' composed of the following vectors:
#' \describe{
#' \item{NLAY}{Atomic Vector of the Number of Layers in the Model Grid}
#' \item{NROW}{Atomic Vector of the Number of Rows in the Model Grid}
#' \item{NCOL}{Atomic Vector of the Number of Columns in the Model Grid}
#' \item{NPER}{Atomic Vector of the Number of Stress Periods in the Model Grid}
#' \item{ITMUNI}{Atomic Vector Indicating the Time Unit of Model Data : 0 = Undefined, 1 = Seconds, 2 = Minutes, 3 = Hours, 4 = Days, 5 = Years}
#' \item{LENUNI}{Atomic Vector Indicating the Length Unit of Model Data : 0 = Undefined, 1 = Feet, 2 = Meters, 3 = Centimeters}
#' \item{LAYCBD}{Flag With One Value for Each Model Layer that Indicates Whether or Not a Layer has a Quasi-3D Confining Bed Below it. 0 Indicates No Confining Bed and Not 0 Indicates Confining Bed}
#' \item{dX}{Cell Width Along Columns}
#' \item{X}{Cell Center Coordinate in the Model Coordinate System}
#' \item{dY}{Cell Width Along Rows}
#' \item{Y}{Cell Center Coordinate in the Model Coordinate System}
#' \item{TOP}{Top Elevation of Layer 1}
#' \item{BOT}{Bottom Elevation of Each Model Cell or a Quasi-3D Confining Bed}
#' \item{PERLEN}{Length of Each Stress Period}
#' \item{NSTP}{Number of Time Steps in a Stress Period}
#' \item{TSMULT}{Multiplier Length of Successive Time Steps. See the MODFLOW2005 Manual For a Description.}
#' \item{SS}{Character Vector Indicating Whether The Stressperiod is Transient ("TR") or Steady State ("SS")}
#' }
#' @param rootname This is the root name of the dis file
#' @export
#' @examples
#' readdis("F95")
#' $NLAY
#' [1] 8
#' 
#' $NCOL
#' [1] 610
#' 
#' $NROW
#' [1] 414
#' 
#' $NPER
#' [1] 16
#' 
#' $ITMUNI
#' [1] 4
#' 
#' $LENUNI
#' [1] 1
#' 
#' $LAYCBD
#'   L1 L2 L3 L4 L5 L6 L7 L8
#'   0  0  0  0  0  0  0  0
#' etc.


readdis <- function(rootname){
    infl <- paste(rootname, ".dis", sep = "")
    linin <- read_lines(infl)
    indx <- 2                       
    NLAY <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]] %>% as.integer()
    NROW <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()
    NCOL <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[3]] %>% as.integer()
    NPER <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[4]] %>% as.integer()
    ITMUNI <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[5]] %>% as.integer()
    LENUNI <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[6]] %>% as.integer()
    indx <- indx + 1
    BLOCKSIZE <- NROW * NCOL
    BLOCKEND <- ceiling(NLAY / 50)
    LAYCBD <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()

    indx <- indx + BLOCKEND
# COLUMN WIDTHS
#---------------------------------------------------------    
    dX <- vector(mode = "numeric", length = NCOL) 
    X <- vector(mode = "numeric", length = NCOL)      
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (ceiling(NCOL / FRMTREP))
    indx <- indx + 1
    if(UNI == 0){
    dX <- rep(MULT, NCOL)
    }else{  
        dX <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
        indx <- indx + BLOCKEND
        }
    dX %<>% as.numeric()  
    X <- c(dX[1] / 2., dX[1:(NCOL - 1)] / 2 + dX[2:NCOL] / 2) %>% cumsum()
# ROW WIDTHS
#----------------------------------------------------------    
    dY <- vector(mode = "numeric", length = NROW) 
    Y <- vector(mode = "numeric", length = NROW)  
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (ceiling(NROW / FRMTREP))  
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]    
    indx <- indx + 1
    if(UNI == 0){
    dY <- rep(MULT, NROW)
    }else{ 
        dY <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
        indx <- indx + BLOCKEND
        }
    dY %<>% as.numeric()    
    Y <- sum(dY) - (c(dY[1] / 2., dY[1:(NROW - 1)] / 2. + dY[2:NROW] / 2.) %>% cumsum())
# TOP ELEVATION
#-----------------------------------------------------------
    TOPin <- vector(mode = "numeric", length = BLOCKSIZE) 
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))  
    indx <- indx + 1
    if(UNI == 0){
        TOPin <- rep(MULT, BLOCKSIZE)
    }else{     
        TOPin <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
        indx <- indx + BLOCKEND
    } 
    TOPin %<>% as.numeric()    
        
    TOP <- tibble::data_frame(ROW = rep(1:NROW, each = NCOL), 
                              COL = rep(seq(1, NCOL, 1), NROW), 
                              TOP = TOPin) 
    rm(TOPin)
    
# BOTTOM ELEVATIONS
#-----------------------------------------------------------
    BOTin <- vector(mode = "numeric", length = NLAY * BLOCKSIZE)    
    BOTHDG <- linin[grep("BOT", linin)]
    HDGLOC <- grep("BOT", linin)
    UNI <- substr(BOTHDG, start = 1, stop = 10) %>% as.integer()
    MULT <- substr(BOTHDG, start = 11, stop = 20) %>% as.numeric()
    # THIS INDEX WILL BE USED TO FILTER OUT LAYERS WITH UNIFORM BOTTOM ELEVATIONS
    MULTFACT <- MULT
    MULTFACT[MULTFACT != 1] <- 0
    ##############################
    FRMT <- substr(BOTHDG, start = 21, stop = 30)
    FRMTREP <- regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT)) %>% lapply('[[', 1) %>% unlist() %>% as.integer()
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))
    LYR  <- substr(BOTHDG, start = 71, stop = nchar(BOTHDG)) %>% as.integer()
    MULTLOC <- grep(0, UNI)
    
    if(length(MULTLOC) > 0){
    # SPECIFIED CELLS: CELL BOTTOM ELEVATIONS SPECIFED USING MULT
    SPEC_CELLS <- rep(BLOCKSIZE * (MULTLOC - 1), each = BLOCKSIZE) + 1:(BLOCKSIZE)
    # ARRAY CELLS
    ARR_CELL   <- seq(1, NLAY * BLOCKSIZE) %>% setdiff(SPEC_CELLS)
    BOTin[SPEC_CELLS] <- rep(MULT[MULTLOC], each = BLOCKSIZE)
    BOT_END <- indx + (BLOCKEND * MULTFACT + 1) %>% cumsum() - 1
    BOT_ARR_ROW <- indx:max(BOT_END) %>% setdiff(HDGLOC)
    BOTin[ARR_CELL] <- linin[BOT_ARR_ROW] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
    }else{    
    BOT_END <- indx + (BLOCKEND * MULTFACT + 1) %>% cumsum() - 1
    BOT_ARR_ROW <- indx:max(BOT_END) %>% setdiff(HDGLOC)
    BOTin <- linin[BOT_ARR_ROW] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
    }
    
    BOTin %<>% as.numeric()    
    BOT <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW),
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY), 
                      BOT = BOTin) 
    rm(BOTin)  
    indx <- max(BOT_END) + 1
#
#----------------------------------------------------------------    
    PERLEN <- vector(mode = "numeric", length = NPER)
    NSTP <- vector(mode = "integer", length = NPER)
    TSMULT <- vector(mode = "numeric", length = NPER)
    SS <- vector(mode = "character", length = NPER)
    PERLEN <- linin[indx:(indx + NPER - 1)] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]] %>% as.numeric()
    NSTP   <- linin[indx:(indx + NPER - 1)] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()    
    TSMULT <- linin[indx:(indx + NPER - 1)] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[3]] %>% as.numeric()
    SS     <- linin[indx:(indx + NPER - 1)] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[4]] %>% as.character()                 
    DIS <- list(NLAY = NLAY,
                NCOL = NCOL, 
                NROW = NROW, 
                NPER = NPER, 
                ITMUNI = ITMUNI, 
                LENUNI = LENUNI, 
                LAYCBD = LAYCBD, 
                dX = dX, 
                X = X, 
                dY = dY, 
                Y = Y, 
                TOP = TOP, 
                BOT = BOT, 
                PERLEN = PERLEN, 
                NSTP = NSTP, 
                TSMULT = TSMULT, 
                SS = SS)    
    return(DIS)        
    }
    