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
    conn <- file(infl, open = "r")
    linin <- readLines(conn)
    close(conn)
    indx <- 2                       
    NLAY <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]] %>% as.integer()
    NROW <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()
    NCOL <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[3]] %>% as.integer()
    NPER <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[4]] %>% as.integer()
    ITMUNI <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[5]] %>% as.integer()
    LENUNI <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[6]] %>% as.integer()
    indx <- indx + 1
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
    X <- c()
    X[1] <- dX[1] / 2.      
    for(j in 2:NCOL){
        X[j] <- dX[j - 1] / 2. + dX[j] / 2. + X[j - 1]
    }  
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
    Y[NROW] <- dY[NROW] / 2.      
    for(i in seq(from = NROW - 1, to = 1, by = -1)){
        Y[i] <- dY[i + 1] / 2. + dY[i] / 2. + Y[i + 1]
    }
# TOP ELEVATION
#-----------------------------------------------------------
    TOPin <- vector(mode = "numeric", length = NROW * NCOL) 
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))  
    indx <- indx + 1
    if(UNI == 0){
        TOPin <- rep(MULT, NROW * NCOL)
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
    BOTin <- vector(mode = "numeric", length = NLAY * NCOL * NROW)    
    for(k in 1:NLAY){
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- NROW * ceiling(NCOL / FRMTREP)  
    indx <- indx + 1
    if(UNI == 0){
        FROM <- (k - 1) * NROW * NCOL + 1
        TO <- k * NROW * NCOL
        BOTin[FROM:TO] <- rep(MULT, NROW * NCOL)
    }else{ 
    FROM <- (k - 1) * NROW * NCOL + 1
    TO <- k * NROW * NCOL
    BOTin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
    indx <- indx + BLOCKEND
    }    
    }
    BOTin %<>% as.numeric()
    
    BOT <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW),
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY), 
                      BOT = BOTin) 
    rm(BOTin)  
    
    PERLEN <- vector(mode = "numeric", length = NPER)
    NSTP <- vector(mode = "integer", length = NPER)
    TSMULT <- vector(mode = "numeric", length = NPER)
    SS <- vector(mode = "character", length = NPER)
    for(Q in 1:NPER){
        PERLEN[Q] <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]] %>% as.numeric()
        NSTP[Q]   <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()    
        TSMULT[Q] <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[3]] %>% as.numeric()
        SS[Q]     <- linin[indx] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[4]] %>% as.character()                 
        indx <- indx + 1 
    }
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
    