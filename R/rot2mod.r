#' Rotate global coordinates in a data frame to model coordinates.
#' 
#' This function reads in a data frame that contains
#' X and Y coordinates and converts those coordinates to a model coordinate system (i.e., zero rotation and X_off = Y_off = 0).
#' @param df This is the data frame containing the X and Y coordinates
#' @param X_off This is the global X coordinate offset
#' @param Y_off This is the global Y coordinate offset
#' @param ROT This is the rotation angle in units of degrees
#' @param Xin This is the dataframe column containing the X coordinate location
#' @param Yin This is the dataframe column containing the Y coordinate location
#' @export
#' @examples
#' obk_demo
#' # A tibble: 19 x 6
#'       ID     LITHO    K_cms           K       X       Y
#'    <chr>     <chr>    <dbl>       <dbl>   <dbl>   <dbl>
#'  1 MW-07      <NA> 5.88e-04  1.66677165 2980417 1927119
#'  2 MW-09        SW 2.58e-03  7.31338583 2980038 1927879
#'  3 MW-10      <NA> 2.51e-03  7.11496063 2980659 1927796
#'  4 MW-13      <NA> 2.22e-03  6.29291339 2980753 1927708
#'  5 MW-15        GM 2.00e-02 56.69291339 2980915 1927664
#'  6 MW-16        SW 6.07e-03 17.20629921 2981707 1928835
#'  7 MW-18        GM 3.91e-03 11.08346457 2981401 1929334
#'  8 MW-21        GM 9.10e-04  2.57952756 2981287 1928896
#'  9 MW-25        GM 1.25e-03  3.54330709 2981012 1928750
#' 10 MW-26     ML/GM 2.55e-03  7.22834646 2981059 1928830
#' 11 MW-27        GM 1.60e-03  4.53543307 2980973 1929291
#' 12 MW-29        GM 1.75e-04  0.49606299 2980824 1928950
#' 13 MW-30 sandstone 6.55e-04  1.85669291 2980486 1929689
#' 14 MW-31        ML 1.04e-05  0.02948031 2980409 1929362
#' 15 MW-32        ML 1.03e-02 29.19685039 2979395 1928257
#' 16 PZ-04     SP/ML 9.41e-06  0.02667402 2980207 1927028
#' 17 PZ-05     SP/GM 2.80e-04  0.79370079 2981367 1928068
#' 18 PZ-06        SW 3.06e-04  0.86740157 2981831 1927966
#' 
#' X_off <- 2970450
#' Y_off <- 1926010
#' rot   <- -45
#' obk_demo %>% rot2mod(X_off = X_off, Y_off = Y_off, ROT = -45)
#' # A tibble: 19 x 8
#'       ID     LITHO    K_cms           K       X       Y     Xout      Yout
#'    <chr>     <chr>    <dbl>       <dbl>   <dbl>   <dbl>    <dbl>     <dbl>
#'  1 MW-07      <NA> 5.88e-04  1.66677165 2980417 1927119 6263.672  7831.879
#'  2 MW-09        SW 2.58e-03  7.31338583 2980038 1927879 5458.256  8100.912
#'  3 MW-10      <NA> 2.51e-03  7.11496063 2980659 1927796 5955.897  8481.173
#'  4 MW-13      <NA> 2.22e-03  6.29291339 2980753 1927708 6085.184  8486.123
#'  5 MW-15        GM 2.00e-02 56.69291339 2980915 1927664 6230.735  8569.561
#'  6 MW-16        SW 6.07e-03 17.20629921 2981707 1928835 5962.466  9957.563
#'  7 MW-18        GM 3.91e-03 11.08346457 2981401 1929334 5393.075 10093.737
#'  8 MW-21        GM 9.10e-04  2.57952756 2981287 1928896 5621.676  9703.563
#'  9 MW-25        GM 1.25e-03  3.54330709 2981012 1928750 5530.678  9405.567
#' 10 MW-26     ML/GM 2.55e-03  7.22834646 2981059 1928830 5507.973  9495.461
#' 11 MW-27        GM 1.60e-03  4.53543307 2980973 1929291 5120.889  9760.895
#' 12 MW-29        GM 1.75e-04  0.49606299 2980824 1928950 5256.879  9413.974
#' 13 MW-30 sandstone 6.55e-04  1.85669291 2980486 1929689 4495.396  9698.061
#' 14 MW-31        ML 1.04e-05  0.02948031 2980409 1929362 4671.784  9412.086
#' 15 MW-32        ML 1.03e-02 29.19685039 2979395 1928257 4736.611  7913.883
#' 16 PZ-04     SP/ML 9.41e-06  0.02667402 2980207 1927028 6179.576  7619.259
#' 17 PZ-05     SP/GM 2.80e-04  0.79370079 2981367 1928068 6264.471  9174.908
#' 18 PZ-06        SW 3.06e-04  0.86740157 2981831 1927966 6664.969  9430.704

rot2mod <- function(df, 
                X_off = 0, 
                Y_off = 0, 
                ROT = 0, 
                Xin = X, 
                Yin = Y){
                
                Xin <- enquo(Xin)
                Yin <- enquo(Yin)
                RAD <- -ROT * pi / 180
                mutate(df, 
                       Xout = ((!!Xin) - X_off) * cos(RAD) - ((!!Yin) - Y_off) * sin(RAD), 
                       Yout = ((!!Xin) - X_off) * sin(RAD) + ((!!Yin) - Y_off) * cos(RAD)
                       )               
                }
