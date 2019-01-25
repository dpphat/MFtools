#' Convert a MODPATH pathline file into a simple feature (sf) object composed of linestring features
#' 
#' This function takes as an argument a MODPATH pathline tibble (as generated using the readpathline
#' function) and converts the contents into an sf object. The pathline can be converted to real-world 
#' coordinates using coordinate offsets and rotations. The pathline can also be modified by applying a 
#' MAX_TIME limit that will only show pathlines that are less than or equal to the
#' threshold time limit (MAX_TIME).
#' @param df This is the data frame containing the pathline information. If this dataframe is not
#' present pth2sf will call readpathline using the supplied PATH. The default is NA.
#' @param X_off This is the global X coordinate offset.
#' @param Y_off This is the global Y coordinate offset.
#' @param ROT This is the rotation angle in units of degrees.
#' @param MAX_TIME is the maximum pathline travel time to be printed. This defaults to Inf.
#' @param PATH This is the full file path to the pathline file. 
#' When PATH is NA and df is not supplied this function will look in the current working 
#' directory for the pathline file.
#' @export

pth2sf <- function(df = NA, X_off = 0, Y_off = 0, ROT = 0, MAX_TIME = Inf, PATH = NA){
    if(is.na(df)){
        df <- MFtools::readpathline(PATH = PATH)
    }
    
    DF_SF <- df %>% MFtools::rot(X_off = X_off, 
                                 Y_off = Y_off, 
                                 ROT = ROT, 
                                 Xin = GLOBAL_X, 
                                 Yin = GLOBAL_Y) %>%
                    dplyr::select(-c(GLOBAL_X, GLOBAL_Y)) %>%
                    dplyr::rename(GLOBAL_X = Xout, GLOBAL_Y = Yout) %>%
                    dplyr::filter(CUMULATIVE_TIME <= MAX_TIME) %>% 
                    sf::st_as_sf(coords = c("GLOBAL_X", "GLOBAL_Y"))
                    
    PATH_SF <- DF_SF %>% 
               dplyr::group_by(PART) %>% 
               dplyr::summarise(do_union = FALSE) %>% 
               sf::st_cast() %>% 
               sf::st_cast("MULTILINESTRING") %>%
               dplyr::ungroup()
               
    return(PATH_SF)
}