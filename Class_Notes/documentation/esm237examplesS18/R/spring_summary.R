#' Summary information about spring climate
#'
#' computes summary information about spring temperature and precipitation
#' @param clim_data  data frame with columns tmax, tmin (C)
#'	rain (precip in mm), year, month (integer), day
#' @param months (as integer) to include in spring; default 4,5,6
#' @param springout (default FALSE) set to TRUE to output spring precip and temperature for all years
#' @return returns a list containing,
#' \describe{
#'  \item{mean_springT}{mean_springT mean spring temperature (C)}
#'  \item{coldest_spring_yr}{ year with lowest spring temperature  (year)}
#'  \item{coldest_springT}{  lowest spring temperature  (C)}
#'  \item{mean_springP}{ mean spring precipitation  (mm)}
#'  \item{wettest_spring_yr}{ spring (as year) with highest precip (year)}
#'  \item{wettest_springP}{  highest precip (mm)}
#'  }

spring_summary = function(clim_data, spring_months = c(4:6), springout=FALSE) {

  spring = subset(clim_data, clim_data$month %in% spring_months)
  springT = (spring$tmax+spring$tmin)/2.0
  all.springT = aggregate(springT, by =list(spring$year), mean)
  colnames(all.springT) = c("year","tavg")
  mean_springT = mean(c(spring$tmax, spring$tmin))
  lowyear = spring$year[which.min(spring$tmin)]
  lowT = min(spring$tmin)
  spring_precip = as.data.frame(matrix(nrow=unique(spring$year), ncol=2))
  colnames(spring_precip)=c("precip","year")

  spring_precip = aggregate(spring$rain, by=list(spring$year), sum)


  colnames(spring_precip) = c("year","precip")
  mean_spring_precip = mean(spring_precip$precip)
  wettest_spring_yr = spring_precip$year[which.max(spring_precip$precip)]
  wettest_spring = max(spring_precip$precip)

  if (springout)
  return(list(mean_springT = mean_springT, coldest_spring_yr=lowyear, coldest_springT=lowT,
              mean_springP=mean_spring_precip,wettest_springP=wettest_spring,
              wettest_spring_yr = wettest_spring_yr,
			all.springP = spring_precip, all.springT = all.springT ))
  else
  return(list(mean_springT = mean_springT, coldest_spring_yr=lowyear, coldest_springT=lowT,
              mean_springP=mean_spring_precip,wettest_springP=wettest_spring,
              wettest_spring_yr = wettest_spring_yr))

}
