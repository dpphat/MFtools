#' Plot and Print Solver Maximum Head-Change Values
#' 
#' This function reads in the list files and plots the head-change values. It assumes that 
#' the solver is unit 19 in the .nam file. The slvrplt is currently working for GMG but as the
#' need arises other solvers will be added. Make sure that the solver is printing all information to the list file.
#' For the GMG solver this means setting ioutgmg to 2. This function will create a plot showing the head change over successive iterations.
#' It will also generate a .csv file indicating the cell location of the cell with the maximum head value.
#' 
#' @param PATH This is the path to the directory that contains model files. The directory needs to include the .in file, .nam file, and .lst file.
#' If PATH is not specified set the working directory to the directory containing the model files.
#' @param HCLOSE This is the head-change close criterion.
#' @param slvUNIT This is the solver unit number in the MODFLOW .nam file. This defaults to 19.
#' @export

slvrplt <- function(PATH = NA, HCLOSE = 0.001, slvUNIT = 19){
    if(is.na(PATH)){
        rtPATH <- getwd()
        }else{
        rtPATH <- PATH
        }
    MFIN_FILES <- c("mf2k.in", "mf2005.in", "mf6.in")
    infl <- list.files(rtPATH, full.names = TRUE)[list.files(rtPATH) %in% MFIN_FILES]
    nmfl <- readr::read_lines(infl) %>% paste(rtPATH, ., sep = "\\")
    rnm  <- nmfl %>% 
            stringr::str_split(".nam") %>% 
            unlist() %>% 
            .[[1]]
            
    pckg <- read.table(nmfl, stringsAsFactors = FALSE) %>% 
            tibble::as_tibble() %>%
            dplyr::rename(PACKAGE = V1, 
                          UNIT = V2, 
                          FILE = V3)
    
    slvr <- pckg %>%
            dplyr::filter(UNIT == slvUNIT) %>%
            .[[1]]
            
    lstfl <- pckg %>% 
             dplyr::filter(PACKAGE == "LIST") %>%
             .[[3]] %>%
             paste(rtPATH, ., sep = "\\")

    if(slvr == "GMG"){
        HDCHNG <- MFtools::read_gmg_lst(lstfl)
    }else if(slvr == "PCG"){
        HDCHNG <- MFtools::read_pcg_lst(lstfl)
    }else if(slvr == "NWT"){
        HDCHNG <- tryCatch(MFtools::read_nwt_lst(lstfl), error=function(e) print("Headchange has not been printed in .lst file yet"))
    }
    
    HDCHNG %>% readr::write_csv(paste(rtPATH, "HDCHNG.csv", sep = "\\"))
    print(HDCHNG)
    
    PLOT <- HDCHNG %>% ggplot2::ggplot(ggplot2::aes(x = ITER, abs(HDCHNG))) +
                       ggplot2::geom_point(shape = 21, 
                                           size = 2, 
                                           colour = "black", 
                                           fill = "blue", 
                                           alpha = 0.5) +
                       ggplot2::geom_hline(yintercept = HCLOSE, 
                                           size = 1, 
                                           colour = "red", 
                                           linetype = "dashed") +
                       ggplot2::scale_y_log10(name = "Maximum Head-Change Value",
                                           limits = c(HCLOSE / 10, NA), 
                                           breaks = 10^(-10:10), 
                                           labels = scales::trans_format("log10", scales::math_format(10^.x)), 
                                           minor_breaks = c(1:9 %o% 10^(-10:10)), 
                                           expand = c(0, 0)) +
                       ggplot2::xlab("Iteration Number") +
                       ggplot2::theme_bw()
    
    if(slvr == "PCG"){
    PLOT <- PLOT + ggplot2::geom_point(data = HDCHNG %>% dplyr::filter(INNER_OUTER == 1), 
                                       shape = 21, 
                                       size = 2, 
                                       colour = "black", 
                                       fill = "red", 
                                       alpha = 0.75)
    }
    print(PLOT)
}