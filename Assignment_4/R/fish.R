#' Abdunance and revenue information of fish caught in Moorea, French Polynesia
#'
#' calculate the most frequently caught fish in each location, total revenue for each location, total fisheries revenue sum, and if requested a graph of revenue by location and total revenue (as text)
#' @param catch_location_data  data frame with columns northside, westside, and eastside (sides of the island), and rows parrotfish, unicronfish, bonito. mahi, yellowfin, and swordfish (fish species caught)
#' @param price_data data frame with column price (Polynesian Franc/kg) and rows parrotfish, unicronfish, bonito. mahi, yellowfin, and swordfish (fish species caught)
#'	rain (precip in mm), year, month (integer), day
#' @param graph specify TRUE for output of a graph of revenue by location
#' @param total_revenue specifiy TRUE for total revenue sum
#' @return returns a list containing  most frequently caught fish in each location, total revenue for each location, and total fisheries revenue sum


fish_summary = function(catch_location_data, price_data, graph, total_revenue) {

  ### most frequently caught fish at each locaton(ie: side of the island)  
  north_catch <- rep(catch_location_data$fish, catch_location_data$north)
  west_catch <- rep(catch_location_data$fish, catch_location_data$west)
  east_catch <- rep(catch_location_data$fish, catch_location_data$east)
  
  north_catch <- as.factor(north_catch)
  west_catch <- as.factor(west_catch)
  east_catch <- as.factor(east_catch)
  
  freq_north <- names(which.max(summary(north_catch)))
  freq_west <- names(which.max(summary(west_catch)))
  freq_east <- names(which.max(summary(east_catch)))

  most_frequent_catch <- data_frame(freq_north, freq_west, freq_east) %>%
    magrittr::set_colnames(value = c("freq_north", "freq_west", "freq_east"))
  
  ### total revenue for each location
  
  revenues_locations <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(north_rev = north*price) %>%
    mutate(west_rev = west*price) %>%
    mutate(east_rev = east*price)
  
  north_rev = sum(revenues_locations$north_rev)
  west_rev = sum(revenues_locations$west_rev)
  east_rev = sum(revenues_locations$east_rev)
  
  total_revenues_locations <- data_frame(north_rev, west_rev, east_rev) %>%
    magrittr::set_colnames(value = c("rev_north", "rev_west", "rev_east"))
  
  ### total fisheries revenue sum 
  
  total_fisheries_revenue <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(totalfish = rowSums(.[2:4])) %>%
    mutate(fishrev = totalfish*price) %>%
    select("fish", "fishrev") %>%
    magrittr::set_colnames(value = c("Fishery", "Total Revenue"))
  
  ### graph if requested
  if (graph == TRUE) {
    
    graph <- revenues_locations %>%
      magrittr::set_colnames(value = c("fish", "north", "east", "west", "price", "North", "West", "East")) %>%
      gather("North", "West", "East", key = "location", value = "price") %>%
      group_by(location) %>%
      summarize(price=sum(price)) %>%
      ungroup() 
      
    graph_rev <- ggplot(graph) +
      geom_col(aes(x=location, y = price)) +
      ggtitle("Total Catch Revenues by Location") +
      ylab("Price (PF/kg)") +
      xlab("Location")
    
  }
  
  ### total revenue if requested
  
  if(total_revenue == TRUE) {
    
   island_revenue <- sum(north_rev, west_rev, east_rev)
   print(island_revenue)
  
}

  return(list(most_frequent_catch, total_revenues_locations, total_fisheries_revenue, graph_rev, island_revenue))
  
}