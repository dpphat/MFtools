#' Read MODFLOW .wel File
#'
#' This function reads in a wel file and creates a list
#' composed of the following vectors:
#' \describe{
#' \item{TEXT}{is a character variable (199 characters) that starts in column 2 of the .wel file. Any characters can be included in Text. 
#' The “#” character is in column 1. Text is written to the Listing File.}
#' \item{PARAMETER}{The optional items NPWEL and MXL (below) must start with the word "PARAMETER"}
#' \item{NPWEL}{is the number of well parameters}
#' \item{MXL}{is the maximum number of wells that will be defined using parameters}
#' \item{MXACTW}{is the maximum number of wells in use during any stress period, including those that are defined using parameters}
#' \item{IWELCB}{is a flag and a unit number.
#' If IWELCB > 0, cell-by-cell flow terms will be written to this unit number when "SAVE BUDGET" or a nonzero
#' value for ICBCFL is specified in Output Control.
#' If IWELCB = 0, cell-by-cell flow terms will not be written.
#' If IWELCB < 0, well recharge for each well will be written to the listing file when "SAVE BUDGET" or a nonzero
#' value for ICBCFL is specified in Output Control}
#' \item{Option}{is an optional list of character values.
#' “AUXILIARY abc” or “AUX abc”—defines an auxiliary variable, named "abc", which will be read for each
#' well as part of Items 4 and 6. Up to 20 variables can be specified, each of which must be preceded by
#' "AUXILIARY" or "AUX." These variables will not be used by the Ground-Water Flow Process, but they
#' will be available for use by other processes. The auxiliary variable values will be read after the Q variable.
#' “NOPRINT”—specifies that lists of wells will not be written to the Listing File.}
#' \item{WELL}{A dataframe consisting of columns of Stress-Period (SP), Layer (LAY), Row (ROW), Column (COL), Pumping Rate (Q), and 
#' auxilliary variable (xyz) for each well cell and each stress-period}
#' }
#' @param rootname This is the root name of the dis file
#' @param NSP This is the number of stress periods in the model simulation that will be listed in the well file
#' @export
#' @examples
#' readwl("F10")
#' $TEXT
#' [1] "# MODFLOW2000 Well Package"
#' 
#' $PARAMETER
#' [1] "PARAMETER"
#' 
#' $NPWEL
#' [1] 0
#' 
#' $MXL
#' [1] 0
#' 
#' $Option
#' [1] "   AUX IFACE"
#' 
#' $MXACTW
#' [1] 105
#' 
#' $IWELCB
#' [1] 50
#' 
#' $WELL
#' # A tibble: 13,965 × 6
#'       SP   LAY   ROW   COL          Q       xyz
#'    <int> <int> <int> <int>      <dbl>     <chr>
#' 1      1    11   142   135 -13378.750         0
#' 2      1    12   142   135 -13378.750         0
#' 3      1     7   127   243 -10683.750         0
#' 4      1     8   127   243 -10683.750         0
#' 5      1    11    27    46  -9624.999         0
#' 6      1    12    27    46  -9624.999         0
#' 7      1    11    27    47  -7699.999         0
#' 8      1    12    27    47  -7699.999         0
#' 9      1     1   232    39  -2310.000         0
#' 10     1     7   230    94  -9239.999         0
#' # ... with 13,955 more rows
readwel <- function(rootname = NA, NSP = 1){
       if(is.na(rootname)){
               rootname <- MFtools::getroot()
       }
       infl                   <- paste(rootname, ".wel", sep = "")
       fl                     <- readr::read_file(infl) %>% gsub("\r", "", x = .) %>% strsplit("\n") %>% unlist()
       TEXT                   <- fl[grep("#", fl)]
       INDX                   <- ifelse(length(grep("#", fl)) == 0, 0, 
                                        max(grep("#", fl), na.rm = TRUE)) + 1
       PARAMETER              <- NULL
       NPWEL                  <- NULL
       MXL                    <- NULL
       Option                 <- NULL
       if(fl[INDX] %>% grepl("PARAMETER", .)){
           PARAMETER          <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]]
           NPWEL              <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()
           MXL                <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[3]] %>% as.integer()
           INDX               <- INDX + 1
       }
       LNGTH                  <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% length()
       MXACTW                 <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[1]] %>% as.integer()
       IWELCB                 <- fl[INDX] %>% strsplit("\\s+") %>% unlist() %>% subset(. != "") %>% .[[2]] %>% as.integer()
       if(LNGTH > 2){
           Option             <- fl[INDX] %>% substr(start = 21, stop = nchar(fl[INDX]))
       }
       INDX                   <- INDX + 1
       # WRITE A FUNCTION FOR READING PARNAM (ITEMS 3 AND 4)
       STARTS                 <- INDX + (seq(1, NSP) - 1) * (MXACTW + 1)
       BLOCKS                 <- setdiff(INDX + seq(1, NSP * (MXACTW + 1) - 1), STARTS)
       ITMP                   <- fl[STARTS] %>% substr(start = 1, stop = 10) %>% as.integer()
       NP                     <- fl[STARTS] %>% substr(start = 11, stop = 20) %>% as.integer()
       SP                     <- seq(1, NSP) %>% rep(each = MXACTW)
       INSTANCES              <- fl[STARTS] %>% substr(start = 21, stop = nchar(fl[INDX + STARTS]))
       # WELL                   <- fl[BLOCKS] %>% 
       #                           stringr::str_split_fixed("\\s+", n = 6) %>% 
       #                           tibble::as_tibble() %>% 
       #                           dplyr::mutate(V1 = SP, 
       #                                         V2 = as.integer(V2), 
       #                                         V3 = as.integer(V3), 
       #                                         V4 = as.integer(V4), 
       #                                         V5 = as.numeric(V5)) %>% 
       #                           rename(SP = V1, LAY = V2, ROW = V3, COL = V4, Q = V5, xyz = V6)
       Layer                  <- fl[BLOCKS] %>% substr(start = 1, stop = 10) %>% as.integer()
       Row                    <- fl[BLOCKS] %>% substr(start = 11, stop = 20) %>% as.integer()
       Col                    <- fl[BLOCKS] %>% substr(start = 21, stop = 30) %>% as.integer()
       Qfact                  <- fl[BLOCKS] %>% substr(start = 31, stop = 41) %>% as.numeric()
       xyz                    <- fl[BLOCKS] %>% substr(start = 42, nchar(fl[INDX + 1]))

       WELL      <- tibble::data_frame(SP = SP, 
                                       LAY = Layer, 
                                       ROW = Row, 
                                       COL = Col, 
                                       Q   = Qfact, 
                                       xyz = xyz)
       WELFL     <- list(TEXT = TEXT, 
                         PARAMETER = PARAMETER, 
                         NPWEL     = NPWEL, 
                         MXL       = MXL, 
                         Option    = Option, 
                         MXACTW    = MXACTW, 
                         IWELCB    = IWELCB, 
                         WELL      = WELL)
       return(WELFL)
       }