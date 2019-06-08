
compute_NPV23 = function(value, time, discount=0.01) {

  result=0.0
  if (length(value) < length(time) )
    value = rep(value, times=length(time))
  for (i in 1:length(time) ) {
    result = result + value[i] / (1 + discount)**time[i]
  }

  return(result)
}
