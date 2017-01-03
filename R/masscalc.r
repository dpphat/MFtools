#' Calculate mass based on MT3D Model Run
#'
#' This function reads in a btn file, hds file, and ucn file to calculate
#' the total mass (in units of lbs) in the model at each times step.
#'
#' @param BTN This is the name of the .btn file (without the file extension)
#' @param HDS This is the name of the .hds file (without the file extension)
#' @param UCN This is the name of the .ucn file (without the file extension)
#' @export
#' @examples
#' masscalc(BTN = "T04", HDS = "F95", UCN = "T041")
#' 
#' # A tibble: 32,325,120 x 7
#'    YEARS   LAY   ROW   COL THICK  CONC  MASS
#'    <dbl> <int> <int> <int> <dbl> <dbl> <dbl>
#' 1      1     1     1     1     0     0     0
#' 2      1     1     1     2     0     0     0
#' 3      1     1     1     3     0     0     0
#' 4      1     1     1     4     0     0     0
#' 5      1     1     1     5     0     0     0
#' 6      1     1     1     6     0     0     0
#' 7      1     1     1     7     0     0     0
#' 8      1     1     1     8     0     0     0
#' 9      1     1     1     9     0     0     0
#' 10     1     1     1    10     0     0     0
#' # ... with 32,325,110 more rows
#' 
#' # Determine the total mass in the top layer of model for each timestep:
#' m <- masscalc(BTN = "T04", HDS = "F95", UCN = "T041")
#' m %>% filter(LAY == 1) %>%
#'       group_by(YEARS) %>%
#'       summarise(TOT = sum(MASS))
#' # A tibble: 16 x 2
#'    YEARS      TOT
#'    <dbl>    <dbl>
#' 1      1 4866.663
#' 2      2 4896.831
#' 3      3 4926.615
#' 4      4 4954.909
#' 5      5 4981.363
#' 6      6 5005.803
#' 7      7 5028.133
#' 8      8 5048.347
#' 9      9 5066.414
#' 10    10 5082.366
#' 11    11 5096.217
#' 12    12 5107.996
#' 13    13 5117.772
#' 14    14 5125.564
#' 15    15 5101.810
#' 16   100 1442.831
#' 
#' # Note: the total mass is in units of lbs

masscalc <- function(BTN, HDS, UCN){
    b <- readbtn(BTN)
    NLAY  <- b$NLAY
    NCOL  <- b$NCOL
    NROW  <- b$NROW
    NSP   <- b$NPER
    NPRS  <- b$NPRS
    POR   <- b$TRANS$PRSITY
    THICK <- b$TRANS$dZ
    dX    <- b$dX
    dY    <- b$dY
    dZ    <- b$TRANS %>% select(LAY, ROW, COL, dZ) 
    TOP   <- b$TOP$TOP
    BOTL1 <- b$TOP$TOP - dZ[dZ$LAY == 1, ]$dZ
    rm(TOP)
    gc()
    if(toupper(b$LUNIT) == "FT"){
        from_L <- 1000. / 2.54^3 / 12^3
    } else if(toupper(b$LUNIT) == "FEET"){
        from_L <- 1000. / 2.54^3 / 12^3
    } else if(toupper(b$LUNIT) == "FOOT"){
        from_L <- 1000. / 2.54^3 / 12^3
    } else if(toupper(b$LUNIT) == "M"){
        from_L_ <- 1000. / 100^3
    } else if(toupper(b$LUNIT) == "METER"){
        from_L_ <- 1000. / 100^3
    } else if(toupper(b$LUNIT) == "METERS"){
        from_L_ <- 1000. / 100^3
    } else if(toupper(b$LUNIT) == "CM"){
        from_L_ <- 1000.
    } else{
        print("LENGTH UNITS CAN BE ft, m, cm")
        stop("LENGTH UNITS DON'T MATCH\n CHECK THE BTN FILE")
        }
    
    if(toupper(b$MUNIT) == "MG"){
        to_lbs <- 1. / 1000 * 2.2046226218
    } else if(toupper(b$MUNIT) == "LB"){
        to_lbs <- 1.
    } else if(toupper(b$MUNIT) == "LBS"){
        to_lbs <- 1.
    } else{
        to_lbs <- 1. / 10^6 / 1000. * 2.2046226218
        print("MASS UNITS ARE ASSUMED TO BE micrograms")
        print("IF THIS IS NOT THE CASE, MAKE THE CORRECTION")
        print("IN THE MASS DATA FRAME")
    }
    
    if(toupper(b$TUNIT) == "D"){
        to_yrs <- 1. / 365
    } else if(toupper(b$TUNIT) == "DAYS"){
        to_yrs <- 1. / 365
    } else if(toupper(b$TUNIT) == "DAY"){
        to_yrs <- 1. / 365
    } else if(toupper(b$TUNIT) == "H"){
        to_yrs <- 1. / 24 / 365
    } else if(toupper(b$TUNIT) == "HOUR"){
        to_yrs <- 1. / 24 / 365
    } else if(toupper(b$MUNIT) == "HOURS"){
        to_yrs <- 1. / 24 / 365
    } else if(toupper(b$TUNIT) == "S"){
        to_yrs <- 1. / 60 / 24 / 365
    } else if(toupper(b$TUNIT) == "SEC"){
        to_yrs <- 1. / 60 / 24 / 365
    } else if(toupper(b$MUNIT) == "SECS"){
        to_yrs <- 1. / 60 / 24 / 365
    } else if(toupper(b$TUNIT) == "Y"){
        to_yrs <- 1.
    } else if(toupper(b$TUNIT) == "YRS"){
        to_yrs <- 1.
    } else if(toupper(b$MUNIT) == "YEAR"){
        to_yrs <- 1.
    } else{
        stop("TIME UNITS DON'T MATCH\n CHECK THE BTN FILE")
    }
    
    TIMPRS_YEARS <- b$TIMPRS * to_yrs
    rm(b)     
    gc()
    h <- readhds(HDS, NLAY, NSP) %>%
         mutate(YEARS = TIME * to_yrs) 
    
    h$GWE[h$GWE == 999] <- -10^6                        # Index and filter this way to improve speed
    
    h %<>% mutate(THICK = ifelse(LAY == 1, GWE - BOTL1, dZ$dZ)) %>%    
           mutate(THICK = ifelse(THICK < 0, 0, THICK))
           
    FLWYRS <- h %>% group_by(YEARS) %>% summarise(TIME = mean(YEARS)) %>% select(YEARS)
    h %<>% select(STP, LAY, ROW, COL, THICK) %>% rename(PER = STP)
          
    TRANSYRS <- TIMPRS_YEARS %>% data_frame(YEARS = .)
    NTTS <- full_join(FLWYRS, TRANSYRS, by = "YEARS") %>% nrow() %>% as.integer()
    dX <- dX %>% rep(NROW) %>% rep(NLAY) %>% rep(NTTS)
    dY <- dY %>% rep(each = NCOL) %>% rep(NLAY) %>% rep(NTTS)
    POR <- POR %>% rep(NTTS)
    
    MASS <- readucn(UCN, NLAY, NTTS) %>%
            mutate(YEARS = TIME * to_yrs) %>% 
            left_join(h, by = c("PER", "LAY", "ROW", "COL")) %>%
            mutate(dX = dX) %>%
            mutate(dY = dY) %>%
            mutate(POR = POR) %>%
            mutate(CONC = ifelse(CONC < 0 , 0, CONC)) %>%
            mutate(MASS = dX * dY * POR * THICK * CONC / from_L * to_lbs) %>%
            select(YEARS, LAY, ROW, COL, THICK, CONC, MASS)         
            
    rm(h)
    rm(dX) 
    rm(dY)
    rm(POR)    
    rm(BOTL1)
    rm(THICK)
    gc()    
    return(MASS)
    }
    
    
