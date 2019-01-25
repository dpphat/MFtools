#' Write a MODPATH pathline file into a simple feature (sf) vector file such as a shapefile
#' 
#' This function takes as an argument a MODPATH pathline tibble (as generated using the readpathline
#' function) and converts the contents into an sf vector file such as a shapefile. 
#' The pathline can be converted to real-world 
#' coordinates using coordinate offsets and rotations. The pathline can also be modified by applying a 
#' MAX_TIME limit that will only show pathlines that are less than or equal to the
#' threshold time limit (MAX_TIME).
#' @param df This is the data frame containing the pathline information. If this dataframe is not
#' present pth2sf will call readpathline using the supplied PATH. The default is NA.
#' @param ofl is the output vector file name (including the filepath) of the SF file.
#' @param X_off This is the global X coordinate offset.
#' @param Y_off This is the global Y coordinate offset.
#' @param ROT This is the rotation angle in units of degrees.
#' @param MAX_TIME is the maximum pathline travel time to be printed. This defaults to Inf.
#' @param PATH This is the full file path to the pathline file. 
#' When PATH is NA and df is not supplied this function will look in the current working 
#' directory for the pathline file.
#' @export

pth2shp <- function(df = NA, 
                    ofl = NA, 
                    PATH = NA, 
                    X_off = 0, 
                    Y_off = 0,
                    ROT = 0, 
                    MAX_TIME = Inf){
                    
                    PATH <- MFtools::pth2sf(df = df, 
                                            X_off = X_off, 
                                            Y_off = Y_off, 
                                            ROT = ROT, 
                                            MAX_TIME = MAX_TIME, 
                                            PATH = PATH)
                                   
                    PATH %>% write_sf(ofl)
    }