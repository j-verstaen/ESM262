#' Abdunance and revenue information of fish caught in Moorea, French Polynesia
#'
#' calculate the most frequently caught fish in each location, total revenue for each location, total fisheries revenue sum, and if requested a graph of revenue by location and total revenue (as text)
#' @param fish_location_data  data frame with columns northside, westside, and eastside (sides of the island), and rows parrotfish, unicronfish, bonito. mahi, yellowfin, and swordfish (fish species caught)
#' @param price_data data frame with column price (Polynesian Franc/kg) and rows parrotfish, unicronfish, bonito. mahi, yellowfin, and swordfish (fish species caught)
#'	rain (precip in mm), year, month (integer), day
#' @param FILL THESE OUT MAYBE
#' @param 
#' @return returns a list containing  most frequently caught fish in each location, total revenue for each location, and total fisheries revenue sum


fish_summary = function(catch_location_data, fish_price_data) {
  
  freq_north <- max(fish_location_data$north)
  freq_west <- max(fish_location_data$west)
  freq_east <- max(fish_location_data$east)
  
  
  
  
}