#' Read MODFLOW .lpf File
#'
#' This function reads in a lpf file and creates a list
#' composed of the following vectors:
#' \describe{
#' \item{ILPFCB}{is a flag and a unit number. If ILPFCB > 0, cell-by-cell flow terms will be written to this unit
#' number when "SAVE BUDGET" or a non-zero value for ICBCFL is specified in Output Control. The terms that are saved
#' are storage, constant-head flow, and flow between adjacent cells.
#' If ILPFCB = 0, cell-by-cell flow terms will not be written.
#' If ILPFCB < 0, cell-by-cell flow for constant-head cells will be written in the listing file when "SAVE BUDGET"
#' or a non-zero value for ICBCFL is specified in Output Control. Cell-by-cell flow to storage and between
#' adjacent cells will not be written to any file.}
#' \item{HDRY}{is the head that is assigned to cells that are converted to dry during a simulation. Although this value plays
#' no role in the model calculations, HDRY values are useful as indicators when looking at the resulting heads that are
#' output from the model. HDRY is thus similar to HNOFLO in the Basic Package, which is the value assigned to cells
#' that are no-flow cells at the start of a model simulation.}
#' \item{NPLPF}{is the number of LPF parameters}
#' \item{LAYTYP}{contains a flag for each layer that specifies the layer type.
#' 0 – confined
#' >0 – convertible
#' <0 – convertible unless the THICKSTRT option is in effect. When THICKSTRT is in effect, a negative value of
#' LAYTYP indicates that the layer is confined, and its saturated thickness will be computed as STRT-BOT.}
#' \item{LAYAVG}{contains a flag for each layer that defines the method of calculating interblock transmissivity.
#' 0—harmonic mean
#' 1—logarithmic mean
#' 2—arithmetic mean of saturated thickness and logarithmic-mean hydraulic conductivity.}
#' \item{CHANI}{contains a value for each layer that is a flag or the horizontal anisotropy. If CHANI is less than or equal to
#' 0, then variable HANI defines horizontal anisotropy. If CHANI is greater than 0, then CHANI is the horizontal
#' anisotropy for the entire layer, and HANI is not read. If any HANI parameters are used, CHANI for all layers must
#' be less than or equal to 0.}
#' \item{LAYVKA}{contains a flag for each layer that indicates whether variable VKA is vertical hydraulic conductivity or
#' the ratio of horizontal to vertical hydraulic conductivity.
#' 0—indicates VKA is vertical hydraulic conductivity
#' not 0—indicates VKA is the ratio of horizontal to vertical hydraulic conductivity, where the horizontal hydraulic
#' conductivity is specified as HK in item 10.}
#' \item{LAYWET}{contains a flag for each layer that indicates whether wetting is active.
#' 0—indicates wetting is inactive
#' not 0—indicates wetting is active}
#' \item{WETFCT}{is a factor that is included in the calculation of the head that is initially established at a cell when the cell
#' is converted from dry to wet. (See IHDWET.)
#' IWETIT—is the iteration interval for attempting to wet cells. Wetting is attempted every IWETIT iteration. If using
#' the PCG solver (Hill, 1990), this applies to outer iterations, not inner iterations. If IWETIT ≤ 0, the value is changed
#' to 1.}
#' \item{IHDWET}{is a flag that determines which equation is used to define the initial head at cells that become wet:
#' If IHDWET = 0, equation 5-32A is used: h = BOT + WETFCT (hn - BOT) .
#' If IHDWET is not 0, equation 5-32B is used: h = BOT + WETFCT(THRESH)}
#' \item{PARNAM}{is the name of a parameter to be defined. This name can consist of 1 to 10 characters and is not case
#' sensitive. That is, any combination of the same characters with different case will be equivalent.}
#' \item{PARTYP}{is the type of parameter to be defined. For the LPF Package, the allowed parameter types are:
#' HK—defines variable HK, horizontal hydraulic conductivity
#' HANI—defines variable HANI, horizontal anisotropy
#' VK—defines variable VKA for layers for which VKA represents vertical hydraulic conductivity (LAYVKA=0)
#' VANI—defines variable VKA for layers for which VKA represents vertical anisotropy (LAYVKA≠0)
#' SS—defines variable Ss, the specific storage
#' SY—defines variable Sy, the specific yield
#' VKCB—defines variable VKCB, the vertical hydraulic conductivity of a Quasi-3D confining layer.}
#' \item{Parval}{is the parameter value. This parameter value may be overridden by a value in the Parameter Value File.}
#' \item{NCLU}{is the number of clusters required to define the parameter. Each repetition of Item 9 is a cluster (variables
#' Layer, Mltarr, Zonarr, and IZ). Each layer that is associated with a parameter usually has only one cluster. For
#' example, parameters which apply to cells in a single layer generally will be defined by just one cluster. However,
#' having more than one cluster for the same layer is acceptable.}
#' \item{Layer}{is the layer number to which a cluster definition applies.}
#' \item{Mltarr}{is the name of the multiplier array to be used to define variable values that are associated with a parameter.
#' The name “NONE” means that there is no multiplier array, and the variable values will be set equal to Parval.}
#' \item{Zonarr}{is the name of the zone array to be used to define the cells that are associated with a parameter. The name
#' “ALL” means that there is no zone array, and all cells in the specified layer are part of the parameter.}
#' \item{IZ}{is up to 10 zone numbers (separated by spaces) that define the cells that are associated with a parameter. These
#' values are not used if ZONARR is specified as “ALL”. Values can be positive or negative, but 0 is not allowed. The
#' end of the line, a zero value, or a non-numeric entry terminates the list of values.}
#' \item{PROPS}{Data frame of LAY, ROW, COL, HK, HANI, VKA, Ss, Sy, VKCB, WETDRY}
#' }
#' @param rootname This is the root name of the lpf file
#' @export
#' @examples
#' readlpf("F95")
#' $ILPFCB
#' [1] 50
#' 
#' $HDRY
#' [1] -1e+30
#' 
#' $NPLPF
#' [1] 0
#' 
#' $LAYTYP
#' [1] 1 3 3 3 0 0 0 0
#' 
#' $LAYAVG
#' [1] 0 0 0 0 0 0 0 0
#' 
#' $CHANI
#' [1] -1 -1 -1 -1 -1 -1 -1 -1
#' 
#' etc.
#' 
#' Use this to develop summary statistics of the hydraulic properties
#' p <- readlpf("F95")
#' p$PROPS %>% group_by(LAY) %>% summarise(MIN_K = min(HK), MEDIAN_K = median(HK), MAX_K = max(HK))
#' 
# A tibble: 8 x 4
#     LAY      MIN_K  MEDIAN_K    MAX_K
#   <int>      <dbl>     <dbl>    <dbl>
# 1     1  0.1000004  3.229593 17.91849
# 2     2  0.0080000  0.008000  0.00800
# 3     3  0.0080000  0.008000  0.00800
# 4     4  0.0080000  0.008000  0.00800
# 5     5  0.0080000  0.008000  0.00800
# 6     6  0.0080000  0.008000  0.00800
# 7     7  0.1000000  6.449597 17.46494
# 8     8 20.0000000 20.000000 20.00000


readlpf <- function(rootname = NA){
    if(is.na(rootname)){
            rootname <- getroot()
    }
    infl <- paste0(rootname, ".lpf")
    d <- readdis(rootname)           
    linin <- read_lines(infl)
    indx <- max(grep("#", linin)) + 1
    ILPFCB <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[1]] %>% 
              as.integer()    
    HDRY   <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[2]] %>% 
              as.numeric()    
    NPLPF  <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[3]] %>% 
              as.integer()    
    indx <- indx + 1
    BLOCKEND <- ceiling(d$NLAY / 50) 
    BLOCKEND_NUM <- ceiling(d$NLAY / 20)    
    LAYTYP <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()    
    indx <- indx + BLOCKEND              
    LAYAVG <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()     
    indx <- indx + BLOCKEND
    CHANI <- linin[indx + seq(1:BLOCKEND_NUM) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
    indx <- indx + BLOCKEND_NUM
    LAYVKA <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()    
    indx <- indx + BLOCKEND
    LAYWET <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.integer()
    indx <- indx + BLOCKEND
    WETFCT <- c(NA)    
    IWETIT <- c(NA)
    IHDWET <- c(NA)
    if(sum(LAYWET) > 0){
    WETFCT <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[1]] %>% 
              as.numeric()    
    IWETIT   <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[2]] %>% 
              as.integer()    
    IHDWET  <- linin[indx] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              .[[3]] %>% 
              as.integer() 

    indx <- indx + 1              
    }
    PARNAM <- vector(mode = "character", length = NPLPF) 
    PARTYP <- vector(mode = "character", length = NPLPF)
    Parval <- vector(mode = "numeric", length = NPLPF)     
    NCLU   <- vector(mode = "integer", length = NPLPF) 
    Layer  <- c(NULL)
    Mltarr <- c(NULL)
    Zonarr <- c(NULL)
    IZ     <- c(NULL)
    if(NPLPF > 0){
        for(Q in 1:NPLPF){
            PARNAM[Q] <- linin[indx] %>% 
                         strsplit("\\s+") %>% 
                         unlist() %>% 
                         subset(. != "") %>% 
                         .[[1]]     
            PARTYP[Q] <- linin[indx] %>% 
                         strsplit("\\s+") %>% 
                         unlist() %>% 
                         subset(. != "") %>% 
                         .[[2]]
            Parval[Q] <- linin[indx] %>% 
                         strsplit("\\s+") %>% 
                         unlist() %>% 
                         subset(. != "") %>% 
                         .[[3]]    %>%
                         as.numeric()
            NCLU[Q]   <- linin[indx] %>% 
                         strsplit("\\s+") %>% 
                         unlist() %>% 
                         subset(. != "") %>% 
                         .[[4]] %>%        
                         as.numeric()
            indx <- indx + 1             
            for(ii in 1:NCLU[Q]){
                indx <- indx + 1    
                Layer[ii] <- linin[indx] %>% 
                             strsplit("\\s+") %>% 
                             unlist() %>% 
                             subset(. != "") %>% 
                             .[[1]] %>%
                             as.integer()
                Mltarr[ii] <- linin[indx] %>% 
                             strsplit("\\s+") %>% 
                             unlist() %>% 
                             subset(. != "") %>% 
                             .[[2]]    
                Zonarr[ii] <- linin[indx] %>% 
                             strsplit("\\s+") %>% 
                             unlist() %>% 
                             subset(. != "") %>% 
                             .[[3]]    
                IZ[ii]    <- linin[indx] %>% 
                             strsplit("\\s+") %>% 
                             unlist() %>% 
                             subset(. != "") %>% 
                             .[[4:nchar(linin[indx])]] %>%
                             as.integer()                             
            }                
        }
    }    
    HKin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY) 
    if(min(CHANI) < 0){ 
           HANIin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY)
           }else{
           HANIin <- rep(NA, d$NCOL * d$NROW * d$NLAY)
           }
    VKAin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY) 
    if("TR" %in% d$SS){
        Ssin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY)
        }else{
        Ssin <- rep(NA, d$NCOL * d$NROW * d$NLAY)
        }
    if(("TR" %in% d$SS)&(min(abs(LAYTYP)) == 0)){
        Syin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY)
        }else{
        Syin <- rep(NA, d$NCOL * d$NROW * d$NLAY)
        }        
    if(any(d$LAYCBD) != 0){
        VKCBDin <- vector(mode = "numeric", length = d$NCOL * d$NROW * d$NLAY)
        }else{
        VKCBDin <- rep(NA, d$NCOL * d$NROW * d$NLAY)
        }
    if((sum(LAYWET) == d$NLAY) & (sum(LAYTYP) != 0)){
        WETDRYin <- vector(mode = "numeric", length = d$NCOL * d$NROW * sum(LAYWET))
        }else{
        WETDRYin <- rep(NA, d$NCOL * d$NROW * d$NLAY) %>% as.numeric()
        }    
            
    for(K in 1:d$NLAY){
# READ HORIZONTAL HYDRAULIC CONDUCTIVITY
#########################################################
    # print(paste("READING HK: LAYER", K))
    FROM <- (K - 1) * d$NROW * d$NCOL + 1
    TO   <- K * d$NROW * d$NCOL
    
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    # print(paste("FRMTREP[",K,"] = ", FRMTREP, sep = ""))
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    HKin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
    }else{  
        HKin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
# READ HORIZONTAL ANISOTROPY IF CHANI < 0
########################################################
    # print(paste("READING HANI: LAYER", K))
    if(CHANI[K] <= 0){                  
        UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
        MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
        FRMT <- substr(linin[indx], start = 21, stop = 30)
        FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
        FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
        BLOCKEND <- (ceiling(d$NCOL / FRMTREP)) * d$NROW
        indx <- indx + 1 
        if(UNI == 0){
        HANIin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
        }else{  
            HANIin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
                  strsplit("\\s+") %>% 
                  unlist() %>% 
                  subset(. != "") %>% 
                  as.numeric()
            indx <- indx + BLOCKEND
            }
            }
        
# READ VKA
##########################################################    
    # print(paste("READING VKA: LAYER", K))
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    VKAin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
    }else{  
        VKAin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
            
# READ Ss IF NUMBER OF TRANSIENT > 0
##########################################################
if("TR" %in% d$SS){    
    # print(paste("READING Ss: LAYER", K))
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    Ssin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
    }else{  
        Ssin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
        }
        
# READ Sy IF NUMBER OF TRANSIENT > 0 & LAYTYP == UNCONFINED (0)
##########################################################
if(("TR" %in% d$SS)&(LAYTYP[K] != 0)){    
    # print(paste("READING Sy: LAYER", K))
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    Syin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
    }else{  
        Syin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
        }

# READ VKCBD 
##########################################################
if(d$LAYCBD[K] != 0){    
    # print(paste("READING VKBD: LAYER", K))
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    rR    }else{  
        VKCBDin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
        }

# READ WETDRY 
##########################################################
if((LAYWET[K] != 0) & (LAYTYP[K] != 0)){    
    # print(paste("READING WETDRY: LAYER", K))
    UNI <- substr(linin[indx], start = 1, stop = 10) %>% as.integer()
    MULT <- substr(linin[indx], start = 11, stop = 20) %>% as.numeric()
    FRMT <- substr(linin[indx], start = 21, stop = 30)
    FRMTREP <- as.integer(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[1]]
    FRMTWIDTH <- as.numeric(regmatches(FRMT, gregexpr("[[:digit:]]+", FRMT))[[1]])[[2]]
    BLOCKEND <- ceiling(d$NCOL / FRMTREP) * d$NROW 
    
    indx <- indx + 1
    if(UNI == 0){
    WETDRYin[FROM:TO] <- rep(MULT, d$NROW * d$NCOL)
    }else{  
        WETDRYin[FROM:TO] <- linin[indx + seq(1:BLOCKEND) - 1] %>% 
              strsplit("\\s+") %>% 
              unlist() %>% 
              subset(. != "") %>% 
              as.numeric()
        indx <- indx + BLOCKEND
        }
        }
    }
PROPS <- tibble::data_frame(
                      LAY = rep(1:d$NLAY, each = d$NCOL * d$NROW) %>% as.integer(), 
                      ROW = rep(rep(1:d$NROW, each = d$NCOL), d$NLAY) %>% as.integer(), 
                      COL = rep(rep(seq(1, d$NCOL, 1), d$NROW), d$NLAY) %>% as.integer(), 
                      HK = HKin %>% as.numeric(), 
                      HANI = HANIin %>% as.numeric(), 
                      VKA = VKAin %>% as.numeric(), 
                      Ss = Ssin %>% as.numeric(), 
                      Sy = Syin %>% as.numeric(), 
                      VKCBD = VKCBDin %>% as.numeric(), 
                      WETDRY = WETDRYin %>% as.numeric()
                      )
rm(HKin)
rm(HANIin)
rm(VKAin)
rm(Ssin)
rm(Syin)
rm(VKCBDin)
rm(WETDRYin)    
rm(d)                  
gc()                      
    
PROPLIST <- list(ILPFCB = ILPFCB, 
                 HDRY = HDRY, 
                 NPLPF = NPLPF, 
                 LAYTYP = LAYTYP, 
                 LAYAVG = LAYAVG, 
                 CHANI = CHANI, 
                 LAYVKA = LAYVKA, 
                 LAYWET = LAYWET, 
                 WETFCT = WETFCT, 
                 IWETIT = IWETIT, 
                 IHDWET = IHDWET, 
                 PARNAM = PARNAM, 
                 PARTYP = PARTYP, 
                 Parval = Parval, 
                 NCLU = NCLU, 
                 Layer = Layer, 
                 Mltarr = Mltarr, 
                 Zonarr = Zonarr, 
                 IZ = IZ,                 
                 PROPS = PROPS)    
    return(PROPLIST)        
    }
    