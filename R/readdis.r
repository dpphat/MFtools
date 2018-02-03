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
#' \item{dY}{Cell Width Along Rows}
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


readdis <- function(rootname = NA){
    if(is.na(rootname)){
            rootname <- getroot()
    }
    infl <- paste0(rootname, ".dis")
    linin <- read_lines(infl) %>% .[-grep("#", .)]              # READ IN DIS FILE BUT REMOVE COMMENTED LINES
                                                                # THIS IS IN PREPARATION FOR MODFLOW 6
    indx <- 1
    NLAY <- linin[indx] %>% parse_MF_FW_ELMT(1) %>% as.integer()
    NROW <- linin[indx] %>% parse_MF_FW_ELMT(2) %>% as.integer()
    NCOL <- linin[indx] %>% parse_MF_FW_ELMT(3) %>% as.integer()
    NPER <- linin[indx] %>% parse_MF_FW_ELMT(4) %>% as.integer()
    ITMUNI <- linin[indx] %>% parse_MF_FW_ELMT(5) %>% as.integer()
    LENUNI <- linin[indx] %>% parse_MF_FW_ELMT(6) %>% as.integer()

    HDGLOC       <- grep("\\(", linin)
    HDG          <- linin[HDGLOC]
    UNI          <- substr(HDG, start = 1, stop = 10) %>% as.integer()
    MULT         <- substr(HDG, start = 11, stop = 20) %>% as.numeric()
    ARR_MULT     <- UNI / UNI
    ARR_MULT[is.na(ARR_MULT)] <- 0    
    MULTLOC      <- grep("^0$", UNI)
    FRMT         <- substr(HDG, start = 21, stop = 30)
    FRMTREP      <- regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT)) %>% lapply('[[', 1) %>% unlist() %>% as.integer()
    BLOCK_START  <- HDGLOC + 1
    BLOCK_END    <- lead(BLOCK_START) - 2
    BLOCK_LENGTH <- (NROW * ceiling(NCOL / FRMTREP))
    BLOCK_END[length(BLOCK_END)] <- BLOCK_END[length(BLOCK_END) - 1] + BLOCK_LENGTH[length(BLOCK_END)] + 1
    BLOCK_START  <- BLOCK_START * ARR_MULT
    BLOCK_END    <- BLOCK_END * ARR_MULT
    CELL_INDX    <- Map(seq, BLOCK_START, BLOCK_END)
    
    indx <- indx + 1
    BLOCKSIZE <- NROW * NCOL
    BLOCKEND <- ceiling(NLAY / 50)
    LAYCBD <- linin[indx + seq(1:BLOCKEND) - 1] %>% parse_MF_FW_LINE() %>% 
              as.integer()

# COLUMN WIDTHS
#---------------------------------------------------------    
    indx <- HDGLOC[1]
    dX <- vector(mode = "numeric", length = NCOL) 
    X <- vector(mode = "numeric", length = NCOL)
    BLOCKEND <- (ceiling(NCOL / FRMTREP[1]))
    indx <- indx + 1
    if(UNI[1] == 0){
    dX <- rep(MULT[1], NCOL)
    }else{  
        dX <- linin[indx + seq(1:BLOCKEND) - 1] %>% parse_MF_FW_LINE()
        }
    dX %<>% as.numeric()  
    X <- c(dX[1] / 2., dX[1:(NCOL - 1)] / 2 + dX[2:NCOL] / 2) %>% cumsum()
# ROW WIDTHS
#----------------------------------------------------------    
    indx <- HDGLOC[2]
    dY <- vector(mode = "numeric", length = NROW) 
    Y <- vector(mode = "numeric", length = NROW)  
    BLOCKEND <- (ceiling(NROW / FRMTREP[2]))  
    indx <- indx + 1
    if(UNI[2] == 0){
    dY <- rep(MULT[2], NROW)
    }else{ 
        dY <- linin[indx + seq(1:BLOCKEND) - 1] %>% parse_MF_FW_LINE()
        }
    dY %<>% as.numeric()    
    Y <- sum(dY) - (c(dY[1] / 2., dY[1:(NROW - 1)] / 2. + dY[2:NROW] / 2.) %>% cumsum())
# TOP ELEVATION
#-----------------------------------------------------------
    indx <- HDGLOC[3]
    TOPin <- vector(mode = "numeric", length = BLOCKSIZE) 
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP[3]))  
    indx <- indx + 1
    if(UNI[3] == 0){
        TOPin <- rep(MULT[3], BLOCKSIZE)
    }else{     
        TOPin <- linin[indx + seq(1:BLOCKEND) - 1] %>% parse_MF_FW_LINE()
    } 
    TOPin %<>% as.numeric()    
        
    TOP <- tibble::data_frame(ROW = rep(1:NROW, each = NCOL) %>% as.integer(), 
                              COL = rep(seq(1, NCOL, 1), NROW) %>% as.integer(),  
                              X   = rep(X, NROW) %>% as.numeric(), 
                              Y   = rep(Y, each = NCOL) %>% as.numeric(), 
                              TOP = TOPin %>% as.numeric()) 
    rm(TOPin)
    
# BOTTOM ELEVATIONS
#-----------------------------------------------------------
    indx <- HDGLOC[4]
    #BOTin <- vector(mode = "numeric", length = NLAY * BLOCKSIZE)    

    VAL <- vector(mode = "numeric", length = NLAY * BLOCKSIZE)
if(length(MULTLOC[MULTLOC > 3]) > 0){
    # SPECIFIED CELLS: CELL BOTTOM ELEVATIONS SPECIFED USING MULT
    # SPEC_CELLS ARE THE CELLS THAT ARE DEFINED WHEN UNIT == 0
    SPEC_CELLS <- rep(BLOCKSIZE * (MULTLOC[MULTLOC > 3] - 4), each = BLOCKSIZE) + 1:BLOCKSIZE
    # ARR_CELLS ARE SPEFICIED IN ARRAYS.
    ARR_LOC    <- grep("^1$", ARR_MULT)
    ARR_CELL   <- rep(BLOCKSIZE * (ARR_LOC[ARR_LOC > 3] - 4), each = BLOCKSIZE) + 1:BLOCKSIZE
    VAL[SPEC_CELLS] <- rep(MULT[MULTLOC[MULTLOC > 3]], each = BLOCKSIZE)
    VAL[ARR_CELL] <- CELL_INDX %>% parse_MF_CELL_INDX(LINE = linin)
    }else{
    VAL <- CELL_INDX %>% parse_MF_CELL_INDX(LINE = linin)
    }
    
    VAL %<>% as.numeric()    
    BOT <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW) %>% as.integer(),
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY) %>% as.integer(), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY) %>% as.integer(), 
                      X   = rep(rep(X, NROW), NLAY) %>% as.numeric(), 
                      Y   = rep(rep(Y, each = NCOL), NLAY) %>% as.numeric(), 
                      BOT = VAL %>% as.numeric()) 
    rm(VAL)  
    indx <- HDGLOC[length(HDGLOC)] + 
            ifelse(UNI[length(UNI)] >= 1, 1, 0) * 
            BLOCK_LENGTH[length(BLOCK_LENGTH)] + 1
#
#----------------------------------------------------------------    
    PERLEN <- vector(mode = "numeric", length = NPER)
    NSTP <- vector(mode = "integer", length = NPER)
    TSMULT <- vector(mode = "numeric", length = NPER)
    SS <- vector(mode = "character", length = NPER)
    PERLEN <- linin[indx:(indx + NPER - 1)] %>% parse_MF_FW_ELMT(1) %>% as.numeric()
    NSTP   <- linin[indx:(indx + NPER - 1)] %>% parse_MF_FW_ELMT(2) %>% as.integer()    
    TSMULT <- linin[indx:(indx + NPER - 1)] %>% parse_MF_FW_ELMT(3) %>% as.numeric()
    SS     <- linin[indx:(indx + NPER - 1)] %>% parse_MF_FW_ELMT(4) %>% as.character()                 
    DIS <- list(NLAY = NLAY,
                NCOL = NCOL, 
                NROW = NROW, 
                NPER = NPER, 
                ITMUNI = ITMUNI, 
                LENUNI = LENUNI, 
                LAYCBD = LAYCBD, 
                dX = dX, 
                dY = dY, 
                TOP = TOP, 
                BOT = BOT, 
                PERLEN = PERLEN, 
                NSTP = NSTP, 
                TSMULT = TSMULT, 
                SS = SS)    
    return(DIS)        
    }
    