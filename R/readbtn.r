#' Read MT3D .btn File
#'
#' This function reads in a btn file and creates a list
#' composed of the following vectors:
#' \describe{
#' \item{NLAY}{Atomic Vector of the Number of Layers in the Model Grid}
#' \item{NCOL}{Atomic Vector of the Number of Columns in the Model Grid} 
#' \item{NROW}{Atomic Vector of the Number of Rows in the Model Grid} 
#' \item{NPER}{Atomic Vector of the Number of Stress Periods in the Model}
#' \item{NCOMP}{Atomic Vector of the Number of Compounds in the Model} 
#' \item{MCOMP}{Atomic Vector of the Number of Mobile Compounds in the Model} 
#' \item{TUNIT}{Atomic Vector of the Number of Time Unit}
#' \item{LUNIT}{Atomic Vector of the Number of Length Unit}
#' \item{MUNIT}{Atomic Vector of the Number of Mass Unit} 
#' \item{TRNOP}{Logical Vector of Packages To Use}
#' \item{LAYCON}{Vector of Layer Types: LAYCON = 0 = Confined, LAYCON ≠ 0 = Unconfined or Convertible} 
#' \item{dX}{Cell Width Along Columns} 
#' \item{X}{Cell Center Coordinate in the Model Coordinate System} 
#' \item{dY}{Cell Width Along Rows}
#' \item{Y}{Cell Center Coordinate in the Model Coordinate System}
#' \item{TOP}{Top Elevation of Layer 1}
#' \item{dZ}{Layer Thicknesses}
#' \item{PRSITY}{Porosity} 
#' \item{ICBUND}{ICBUND array. See MT3DMS manual for a description of ICBUND}
#' \item{SCONC}{Starting Concentrations at the beginning of the simulation} 
#' \item{CINACT}{Value that indicates an inactive value} 
#' \item{THKMIN}{Minimum saturated thickness in a cell}
#' \item{IFMTCN}{Flag indicating whether the calculated concentration should be 
#' printed to the standard output text file and also serves as a printing-format
#' code if it is printed}
#' \item{IFMTNP}{Flag indicating if the number of particles in each cell should be printed and also
#' serves as a printing format code if it is printed}
#' \item{IFMTRF}{Flag indicating if the model-calculated retardation factor should be printed and also
#' serves as a printing format code if it is printed}
#' \item{IFMTDP}{Flag indicating if the model-calculated distance-weighted dispersion coefficient should
#' be printed and also serves as a printing format code if it is printed} 
#' \item{SAVUCN}{Logical flag indicating if the concentration solution should be saved to a .ucn file}
#' \item{NPRS}{Flag indicating the frequncy of output and also indicating whether the frequency output
#' is specified in terms of total elapsed simulation time or the transport step number.
#' See MT3D Manual for further details of NPRS}
#' \item{TIMPRS}{Total elapsed time at which the simulation results are printed}
#' \item{NOBS}{Number of Observations wells recording concentration data} 
#' \item{NPROBS}{Integer indicating how frequently the concentration is recorded at each
#'  Observation Well} 
#' \item{OBSLOC}{Cell indices (L, R, C) of Observation Well locations} 
#' \item{CHKMAS}{Logical Flag indicating whether a one-line summary of mass balance
#' information should be printed} 
#' \item{NPRMAS}{Integer indicating how frequently the mass budget information
#' should be recorded} 
#' \item{PERLEN}{Lengths of Stress Perids}
#' \item{NSTP}{Number of time-steps for the transient flow folution in each stress-period} 
#' \item{TSMULT}{Time-step multiplier. Only used if NSTP > 1. See MT3DMS Manual for 
#' further details} 
#' \item{TSLNGHdf}{If TSMULT < 0, TSLNGHdf frovides the length of time-steps for the
#' flow solution in each stress period}
#' \item{DT0}{The user-specified transport step size within each time-step of the
#' flow solution} 
#' \item{MXSTRN}{The maximum number of transport time steps allowed within one time
#' step of the flow solution}
#' \item{TTSMULT}{A multiplier for successive transport steps within a flow step} 
#' \item{TTSMAX}{Maximum transport step size allowed when transport step size multiplier
#' TTSMULT > 1.0}
#' }
#' @param rootname This is the root name of the btn file
#' @export
#' @examples
#' readbtn("T04")
#' 
#' # Find the average initial concentration assigned to the MT3DMS Model
#' btn <- readbtn("T04")
#' btn$SCONC %>% 
#' select(SCONC) %>%
#' summarise(AVG = mean(SCONC))
#'
#' > A tibble: 1 x 1
#' >        AVG
#' >      <dbl>
#' > 1 17.77554
#'
#' # Find the average initial concentration (> 5 µg/L) 
#' # assigned to Layer 1 of the MT3DMS Model
#' btn$SCONC %>% 
#' filter(LAY == 1) %>%
#' select(SCONC) %>%
#' filter(SCONC >= 5) %>%
#' summarise(AVG = mean(SCONC))
#'
#' > A tibble: 1 x 1
#' >        AVG
#' >      <dbl>
#' > 1 1096.754
#'
#' # Find the range of porosity values assigned to the model 
#' btn$PRSITY %>%                 # Select the RCL Value table that contains the porosity values
#' select(PRSITY) %>%             # Select the table column that contains porosity values
#' t() %>%                        # Convert to a vector of values
#' as.factor() %>%
#' levels()
#'
#' > [1] "0.15" "0.21" "0.25" "0.4" 

readbtn <- function(rootname){
    infl <- paste(rootname, ".btn", sep = "")
    conn <- file(infl, open = "r")
    linin <- readLines(conn)
    close(conn)
    indx <- 3                      
    NLAY <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    NROW <- substr(linin[indx], start = 11, stop = 20) %>% as.integer()
    NCOL <- substr(linin[indx], start = 21, stop = 30) %>% as.integer()
    NPER <- substr(linin[indx], start = 31, stop = 40) %>% as.integer()
    NCOMP <- substr(linin[indx], start = 41, stop = 50) %>% as.integer()
    MCOMP <- substr(linin[indx], start = 51, stop = 60) %>% as.integer()
    indx <- indx + 1
    TUNIT <- substr(linin[indx], start = 1, stop = 4) %>% gsub("[[:space:]]", "", .)
    LUNIT <- substr(linin[indx], start = 5, stop = 8) %>% gsub("[[:space:]]", "", .)
    MUNIT <- substr(linin[indx], start = 9, stop = 12) %>% gsub("[[:space:]]", "", .)    
    indx <- indx + 1
    TRNOP <- substring(linin[indx], 
                       seq(1,nchar(linin[indx]),2), 
                       seq(2,nchar(linin[indx]),2)) %>% 
                       gsub("[[:space:]]", "", .)
    indx <- indx + 1
    BLOCKEND <- ceiling(NLAY / 40)
    LAYCON <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
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
    TOPin <- vector(mode = "numeric", length = NROW) 
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))  
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]] 
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
# CELL THICKNESS
#--------------------------------------------------------------      
    dZin <- vector(mode = "numeric", length = NLAY * NCOL * NROW)    
    for(k in 1:NLAY){
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))  
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]] 
    indx <- indx + 1
    if(UNI == 0){
        FROM <- (k - 1) * NROW * NCOL + 1
        TO <- k * NROW * NCOL
        dZin[FROM:TO] <- rep(MULT, NROW * NCOL)
    }else{ 
    FROM <- (k - 1) * NROW * NCOL + 1
    TO <- k * NROW * NCOL
    dZin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
    indx <- indx + BLOCKEND
    }    
    }
    dZin %<>% as.numeric()
    
    dZ <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW),
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY), 
                      dZ = dZin) 
    rm(dZin)    
# POROSITY
#--------------------------------------------------------------    
    PRSITYin <- vector(mode = "numeric", length = NLAY * NCOL * NROW)
    for(k in 1:NLAY){
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- (NROW * ceiling(NCOL / FRMTREP))  
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]    
    indx <- indx + 1
    if(UNI == 0){
    FROM <- (k - 1) * NROW * NCOL + 1
    TO <- k * NROW * NCOL
    PRSITYin[FROM:TO] <- rep(MULT, NROW * NCOL)
    }else{ 
    for(i in 1:BLOCKEND){    
    FROM <- (k - 1) * NROW * NCOL + 1
    TO   <- k * NROW * NCOL
        PRSITYin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
    indx <- indx + BLOCKEND
        }
    }    
    }  
    PRSITYin %<>% as.numeric()    
    PRSITY <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW), 
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY), 
                      PRSITY = PRSITYin)  
    rm(PRSITYin)                      
# ICBUND
#---------------------------------------------------------------    
    ICBUNDin <- vector(mode = "integer", length = NLAY * NCOL * NROW)
    for(k in 1:NLAY){
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(NCOL / FRMTREP) * NROW 
    FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]    
    indx <- indx + 1
    if(UNI == 0){    
        FROM <- (k - 1) * NROW * NCOL + 1
        TO <- k * NROW * NCOL
        ICBUNDin[FROM:TO] <- rep(MULT, NROW * NCOL)
    }else{ 
    FROM <- (k - 1) * NROW * NCOL + 1
    TO   <- k * NROW * NCOL 
        ICBUNDin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "")
        indx <- indx + BLOCKEND
        }
        }
    ICBUNDin %<>% as.integer()    
    ICBUND <- tibble::data_frame(
                      LAY = rep(1:NLAY, each = NCOL * NROW) %>% as.integer(), 
                      ROW = rep(rep(1:NROW, each = NCOL), NLAY) %>% as.integer(), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NLAY) %>% as.integer(), 
                      ICBUND = ICBUNDin)
    rm(ICBUNDin)    
# SOURCE CONCENTRATION
#----------------------------------------------------------------                      
    SCONCin <- vector(mode = "numeric", length = MCOMP * NLAY * NCOL * NROW)
    for(M in 1:MCOMP){
        for(k in 1:NLAY){
            UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
            MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
            FRMT <- substr(linin[indx], start = 21, stop = 30)
            FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
            FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
            BLOCKEND <- NROW * ceiling(NCOL / FRMTREP)  
            FRMTREP <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
            FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]    
            indx <- indx + 1
        if(UNI == 0){
        SCONCin[FROM:TO] <- rep(MULT, NROW * NCOL)
        }else{     
            FROM <- (M - 1) * NLAY * NCOL * NROW + (k - 1) * NROW * NCOL + 1
            TO <- M * k * NROW * NCOL     
            SCONCin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
                                strsplit("\\s+") %>% 
                                unlist() %>% 
                                subset(. != "")
            indx <- indx + BLOCKEND
                }
            }    
        } 
    SCONCin %<>% as.numeric()
    SCONC <- tibble::data_frame(
                      MCOMP = rep(1:MCOMP, each = NLAY * NCOL * NROW) %>% as.integer(), 
                      LAY = rep(rep(1:NLAY, each = NCOL * NROW), MCOMP) %>% as.integer(), 
                      ROW = rep(rep(rep(1:NROW, each = NCOL), NLAY), MCOMP) %>% as.integer(), 
                      COL = rep(rep(rep(seq(1, NCOL, 1), NROW), NLAY), MCOMP) %>% as.integer(), 
                      SCONC = SCONCin %>% as.numeric())
    rm(SCONCin)        
# BACK ARRAYS
#--------------------------------------------------------------
    CINACT <- substring(linin[indx], 
                        seq(1, nchar(linin[indx]), 10), 
                        seq(2, nchar(linin[indx]), 10))[[1]] %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.numeric()
    THKMIN <- substring(linin[indx], 
                        seq(1, nchar(linin[indx]), 10), 
                        seq(10, nchar(linin[indx]), 10)) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.numeric()   
    indx <- indx + 1
    IFMTCN <- substring(linin[indx], 1, 10) %>% 
              gsub("[[:space:]]", "", .) %>% as.integer()
    IFMTNP <- substring(linin[indx], 11, 20) %>% 
              gsub("[[:space:]]", "", .) %>% as.integer()
    IFMTRF <- substring(linin[indx], 21, 30) %>% 
              gsub("[[:space:]]", "", .) %>% as.integer()  
    IFMTDP <- substring(linin[indx], 31, 40) %>% 
              gsub("[[:space:]]", "", .) %>% as.integer() 
    SAVUCN <- substring(linin[indx], 41, 50) %>% 
              gsub("[[:space:]]", "", .) 
    UNKN <- substring(linin[indx], 51, 60) %>% 
              gsub("[[:space:]]", "", .)
    indx <- indx + 1

# UCN PRINTING FREQUENCY
#-----------------------------------------------------------------    
    NPRS <- linin[indx] %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()
    indx <- indx + 1
    TIMPRS <- vector(mode = "numeric", length = NPRS)
    FRMTREP <- 8
    FRMTWIDTH <- 10
    BLOCKEND <- ceiling(NPRS / FRMTREP)
    if(NPRS > 0){
    for(i in 1:BLOCKEND){
        FROM <- (i - 1) * FRMTREP + 1
        TO <- ifelse(i == BLOCKEND, NPRS, i * FRMTREP)    
        TIMPRS[FROM:TO] <- substring(linin[indx], 
                           seq(1, nchar(linin[indx]), FRMTWIDTH), 
                           seq(FRMTWIDTH, nchar(linin[indx]), FRMTWIDTH)) %>% 
                           gsub("[[:space:]]", "", .) %>%
                        as.numeric()
        indx <- indx + 1
        }
    }
#
#-----------------------------------------------------------------
    NOBS <- substring(linin[indx], 1, 10) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()
    KOBS <- vector(mode = "integer", length = NOBS)
    IOBS <- vector(mode = "integer", length = NOBS)
    JOBS <- vector(mode = "integer", length = NOBS)    
    NPROBS <- substring(linin[indx], 11, 20) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()     
    indx <- indx + 1
    OBSLOC <- tibble::data_frame(
                        OBS_WELL = NA, 
                        LAY = NA, 
                        ROW = NA, 
                        COL = NA
                        )    
                        
    if(NOBS > 0){
    for(i in 1:NOBS){
    OBSLOC$OBS_WELL[i] <- i 
    OBSLOC$LAY[i] <- substring(linin[indx], 1, 10) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()
    OBSLOC$ROW[i] <- substring(linin[indx], 11, 20) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()    
    OBSLOC$COL[i] <- substring(linin[indx], 21, 30) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()
    indx <- indx + 1              
        }        
    }                      
  
    L13 <- read.fwf(infl, 
                    skip = indx, 
                    widths = rep(10, 2), 
                    n = 1)
    CHKMAS <- substring(linin[indx], 1, 10) %>% 
              gsub("[[:space:]]", "", .)
    NPRMAS <- substring(linin[indx], 11, 20) %>% 
              gsub("[[:space:]]", "", .) %>% 
              as.integer()   
    indx <- indx + 1 
    PERLEN <- vector(mode = "numeric", length = NPER)
    NSTP <- vector(mode = "integer", length = NPER)
    TSMULT <- vector(mode = "numeric", length = NPER)
    TSLNGH <- c()
    DT0 <- vector(mode = "numeric", length = NPER)
    MXSTRN <- vector(mode = "integer", length = NPER)
    TTSMULT <- vector(mode = "numeric", length = NPER)
    TTSMAX <- vector(mode = "numeric", length = NPER)
    TSLNGHdf <- tibble::data_frame(
              PER = NA, 
              TSLNGH = NA
              )
                  
    for(Q in 1:NPER){
        PERLEN[Q] <- substr(linin[indx + 1], start = 0, stop = 11) %>%
                     as.numeric()    
        NSTP[Q] <-   substr(linin[indx + 1], start = 11, stop = 20) %>%
                     as.integer()    
        TSMULT[Q] <- substr(linin[indx + 1], start = 21, stop = 30) %>%
                     as.numeric()    
        indx <- indx + 1 
        if(TSMULT[[Q]] < 0){
            FRMTREP <- 8
            FRMTWIDTH <- 10
            BLOCKEND <- ceiling(TSMULT[[Q]] / FRMTREP)
            for(i in 1:BLOCKEND){            
                FROM <- (i - 1) * FRMTREP + 1
                TO <- ifelse(i == BLOCKEND, NPRS, i * FRMTREP)
                TSLNGH[FROM:TO] <- substring(linin[indx], 
                                   seq(1, nchar(linin[indx]), FRMTWIDTH), 
                                   seq(FRMTWIDTH, nchar(linin[indx]), FRMTWIDTH)) %>% 
                                   gsub("[[:space:]]", "", .) %>%
                                as.numeric()
                indx <- indx + 1
                TSLNGHdf$PER[FROM:TO]     <- Q
                TSLNGHdf$TSLNGTH[FROM:TO] <- TSLNGTH  
            }                
        }
        DT0[Q] <- substr(linin[indx], start = 0, stop = 10) %>%
                  as.numeric()
        MXSTRN[Q] <- substr(linin[indx], start = 11, stop = 20) %>%
                     as.integer()
        TTSMULT[Q] <- substr(linin[indx], start = 21, stop = 30) %>%
                      as.numeric()
        TTSMAX[Q] <- substr(linin[indx], start = 31, stop = 40) %>%
                     as.numeric()
        indx <- indx + 1
    }
    rm(linin)
    BTN <- list(NLAY = NLAY,
                NCOL = NCOL, 
                NROW = NROW, 
                NPER = NPER,
                NCOMP = NCOMP, 
                MCOMP = MCOMP, 
                TUNIT = TUNIT, 
                LUNIT = LUNIT, 
                MUNIT = MUNIT, 
                TRNOP = TRNOP, 
                LAYCON = LAYCON, 
                dX = dX, 
                X = X, 
                dY = dY, 
                Y = Y, 
                TOP = TOP, 
                dZ = dZ, 
                PRSITY = PRSITY, 
                ICBUND = ICBUND, 
                SCONC = SCONC, 
                CINACT = CINACT, 
                THKMIN = THKMIN, 
                IFMTCN = IFMTCN, 
                IFMTNP = IFMTNP, 
                IFMTRF = IFMTRF, 
                IFMTDP = IFMTDP, 
                SAVUCN = SAVUCN, 
                NPRS = NPRS, 
                TIMPRS = TIMPRS, 
                NOBS = NOBS, 
                NPROBS = NPROBS, 
                OBSLOC = OBSLOC, 
                CHKMAS = CHKMAS, 
                NPRMAS = NPRMAS, 
                PERLEN = PERLEN, 
                NSTP = NSTP, 
                TSMULT = TSMULT, 
                TSLNGHdf = TSLNGHdf, 
                DT0 = DT0, 
                MXSTRN = MXSTRN, 
                TTSMULT = TTSMULT, 
                TTSMAX = TTSMAX)    
    return(BTN)  
    gc()    
    }
    