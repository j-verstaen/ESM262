#' Abundance and revenue information of fish caught in Moorea, French Polynesia
#'
#' Calculate the most frequently caught fish in each location, total revenue for each location, total fisheries revenue sum, and if requested a graph of revenue by location and total revenue (as text)
#' @param catch_location_data  data frame with columns: fish species, northside, westside, and eastside (sides of the island)
#' @param price_data data frame with fish species and price (Polynesian Franc/kg)
#' @param graph specify TRUE for output of a graph of revenue by location
#' @return returns a list containing  most frequently caught fish in each location, revenue by location, revenue by fisheries, total revene, and graph if requested

fish_summary = function(catch_location_data, price_data, graph=FALSE) {

  
  ### 1. most frequently caught fish at each location (ie: side of the island)  

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
  
  
  ### 2. total revenues by location
  
  if(any(price_data$price < 0)) stop('Potential error: fish prices can not be negative')
  
  revenues_locations <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(north_rev = north*price) %>%
    mutate(west_rev = west*price) %>%
    mutate(east_rev = east*price)
  
  north_rev = sum(revenues_locations$north_rev)
  west_rev = sum(revenues_locations$west_rev)
  east_rev = sum(revenues_locations$east_rev)
  
  total_revenues_locations <- data_frame(north_rev, west_rev, east_rev) %>%
    magrittr::set_colnames(value = c("rev_north", "rev_west", "rev_east"))
  
  
  ### 3. total revenues by fishery  
  
  total_revenues_by_fishery <- left_join(catch_location_data, price_data, by = "fish") %>%
    mutate(totalfish = rowSums(.[2:4])) %>%
    mutate(fishrev = totalfish*price) %>%
    select("fish", "fishrev") %>%
    magrittr::set_colnames(value = c("Fishery", "Total Revenue"))
  
  
  ### 4. total revenue of all fisheries
  
    total_revenue <- sum(north_rev, west_rev, east_rev)

    
  ### 5. graph of revenues by location if requested with total revenue printed bottom right
    
  if (graph == TRUE) {
    
    graph <- revenues_locations %>%
      magrittr::set_colnames(value = c("fish", "north", "east", "west", "price", "North", "West", "East")) %>%
      gather("North", "West", "East", key = "location", value = "price") %>%
      group_by(location) %>%
      summarize(price=sum(price)) %>%
      ungroup() 
      
caption <- c("Total Revenue: PF")   

    graph_revenue <- ggplot(graph) +
      geom_col(aes(x=location, y = price), fill= "deepskyblue4") +
      ylab("Price (PF/kg)") +
      xlab("Location") +
      theme_classic() +
      labs(title ="Total Catch Revenues by Location", caption = paste(caption,total_revenue))
    
    graph_revenue
    
  }

  return(list(most_frequent_catch, total_revenues_locations, total_revenues_by_fishery, total_revenue, graph_revenue))
  
}