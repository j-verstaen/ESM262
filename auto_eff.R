#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at a given speed
#' @param cdrag coefficient due to drag default=0.3 
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

auto_power = function(v,
                    m, 
                    a, 
                    g = 9.8, 
                    p_air = 1.2, 
                    c_drag = 0.3,
                    c_roll= 0.015) {
  
  
  P = c_roll*m*g*v + 0.5*a*p_air*c_drag*v^3
    
    return(P)
  
}