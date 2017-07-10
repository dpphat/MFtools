#' Rotate model data frame X and Y coordinates to global coordinates.
#' 
#' This function reads in a MFtools data frame (i.e., from readdis, readbtn) that contains
#' X and Y coordinates and converts those coordinates to a global coordinate system.
#' @param df This is the data frame containing the X and Y coordinates (i.e., dis$BOT)
#' @param X_off This is the global X coordinate offset
#' @param Y_off This is the global Y coordinate offset
#' @param ROT This is the rotation angle in units of degrees
#' @param Xin This is the dataframe column containing the X coordinate location
#' @param Yin This is the dataframe column containing the Y coordinate location
#' @export
#' @examples
#' d <- readdis(rnm)
#' d$BOT %>% rot(X_off = 2970450, Y_off = 1926010, ROT = -45)
#' # A tibble: 3,245,232 x 8
#'      LAY   ROW   COL      X     Y    BOT    Xout    Yout
#'    <int> <int> <dbl>  <dbl> <dbl>  <dbl>   <dbl>   <dbl>
#'  1     1     1     1  200.0 13200 505.74 2979925 1935202
#'  2     1     1     2  550.0 13200 504.18 2980173 1934955
#'  3     1     1     3  800.0 13200 504.26 2980349 1934778
#'  4     1     1     4  975.0 13200 503.99 2980473 1934654
#'  5     1     1     5 1117.5 13200 506.10 2980574 1934554
#'  6     1     1     6 1230.0 13200 503.66 2980654 1934474
#'  7     1     1     7 1305.0 13200 503.85 2980707 1934421
#'  8     1     1     8 1360.0 13200 504.83 2980745 1934382
#'  9     1     1     9 1410.0 13200 504.79 2980781 1934347
#' 10     1     1    10 1457.5 13200 504.11 2980814 1934313
#' # ... with 3,245,222 more rows

rot <- function(df, 
                X_off = 0, 
                Y_off = 0, 
                ROT = 0, 
                Xin = X, 
                Yin = Y){
                
                Xin <- enquo(Xin)
                Yin <- enquo(Yin)
                RAD <- ROT * pi / 180
                mutate(df, 
                       Xout = (!!Xin) * cos(RAD) - (!!Yin) * sin(RAD) + X_off, 
                       Yout = (!!Xin) * sin(RAD) + (!!Yin) * cos(RAD) + Y_off
                       )               
                }
