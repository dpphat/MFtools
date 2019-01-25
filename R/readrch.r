#' Read MODFLOW .rch File
#'
#' This function reads in a rch file and creates a list
#' composed of the following vectors:
#' \describe{
#' \item{NRCHOP}{Atomic Vector of the is the recharge option code. Recharge fluxes are defined in a layer variable, RECH, with one value for
#'               each vertical column. Accordingly, recharge is applied to one cell in each vertical column, and the option code
#'               determines which cell in the column is selected for recharge.
#'               1—Recharge is only to the top grid layer.
#'               2—Vertical distribution of recharge is specified in layer variable IRCH.
#'               3—Recharge is applied to the highest active cell in each vertical column. A constant-head node intercepts
#'               recharge and prevents deeper infiltration.}
#' \item{IRCHCB}{Atomic Vector and is flag and a unit number.
#'               If IRCHCB > 0, cell-by-cell flow terms will be written to this unit number when "SAVE BUDGET" or a nonzero
#'               value for ICBCFL is specified in Output Control.
#'               If IRCHCB ≤ 0, cell-by-cell flow terms will not be written.}
#' \item{RCH}{Data Frame Composed of Layer, Row, Column, Stress Period, and Recharge rate applied to the top of the model domain}
#' }
#' @param rootname This is the root name of the rch file
#' @export

readrch <- function(rootname = NA){
    if(is.na(rootname)){
            rootname <- MFtools::getroot()
    }
    infl <- paste0(rootname, ".rch")
    MOD_DIMS <- MFtools::get_dims(rootname)
    NPER <-  MOD_DIMS$NPER
    NCOL <-  MOD_DIMS$NCOL
    NROW <-  MOD_DIMS$NROW
    linin <- readr::read_lines(infl) %>% .[!grepl("#", .)]              # READ IN RCH FILE BUT REMOVE COMMENTED LINES
    indx <- 1
    NRCHOP <- linin[indx] %>% MFtools::parse_MF_FW_ELMT(1) %>% as.integer()
    IRCHCB <- linin[indx] %>% MFtools::parse_MF_FW_ELMT(2) %>% as.integer()
    
    HDGLOC       <- grep("\\(", linin)
    HDG          <- linin[HDGLOC]
    UNI          <- substr(HDG, start = 1, stop = 10) %>% as.integer()
    MULT         <- substr(HDG, start = 11, stop = 20) %>% as.numeric()
    ARR_MULT     <- UNI / UNI
    ARR_MULT[is.na(ARR_MULT)] <- 0    
    MULTLOC      <- grep("^0$", UNI)
    FRMT         <- substr(HDG, start = 21, stop = 30)
    FRMTREP      <- regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT)) %>% lapply('[[', 1) %>% unlist() %>% as.integer()
    BLOCK_LENGTH <- (NROW * ceiling(NCOL / FRMTREP))
    BLOCK_START  <- HDGLOC + 1
    BLOCK_END    <- dplyr::lead(BLOCK_START) - 2
    BLOCK_END    <- ifelse(is.na(BLOCK_END), BLOCK_START + BLOCK_LENGTH - 1, BLOCK_END)
    if(length(BLOCK_END) > 1){
        BLOCK_END[length(BLOCK_END)] <- BLOCK_END[length(BLOCK_END) - 1] + BLOCK_LENGTH[length(BLOCK_END)] + 1
        }
    BLOCK_START  <- BLOCK_START * ARR_MULT
    BLOCK_END    <- BLOCK_END * ARR_MULT
    CELL_INDX    <- Map(seq, BLOCK_START, BLOCK_END)
    
    indx <- indx + 1
    BLOCKSIZE <- NROW * NCOL
    BLOCKEND <- ceiling(NPER / 50)
    INRECH_INIRCH_LOC <- HDGLOC - 1
    INRECH <- linin[INRECH_INIRCH_LOC] %>% parse_MF_FW_ELMT(1) %>% as.integer()
    INIRCH <- linin[INRECH_INIRCH_LOC] %>% parse_MF_FW_ELMT(2) %>% as.integer()
    
    indx <- HDGLOC[1]
    VAL <- vector(mode = "numeric", length = NPER * BLOCKSIZE)
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
    VAL <- CELL_INDX %>% MFtools::parse_MF_CELL_INDX(LINE = linin)
    }

    RCHdf <- tibble::data_frame(
                      LAY = 1 %>% as.integer(),
                      ROW = rep(rep(1:NROW, each = NCOL), NPER) %>% as.integer(), 
                      COL = rep(rep(seq(1, NCOL, 1), NROW), NPER) %>% as.integer(), 
                      SPER = rep(1:NPER, each = NCOL * NROW) %>% as.integer(),
                      RCH = VAL %>% as.numeric())
    rm(VAL)
    RCH <- list(NRCHOP = NRCHOP, 
                IRCHCB = IRCHCB, 
                RCH    = RCHdf)
    return(RCH)
    cat("WARNING################################\n")
    cat("More work is needed on this function if there are more than one stress period\n")
    cat("Use the readlpf function as a template\n")
    cat("#######################################\n")
    }