thanos <- function(x){
  len_x <- length(x)
  len_del <- len_x / 2
  if (len_x %% 2 == 1){
    if (rnorm(1) < 0){
      len_del <- len_del - 0.5
    } else if (rnorm(1) > 0){
      len_del <- len_del + 0.5
    }
  } else {
    
  }
  x <- x[-c(sample(len_x, size = len_del))]
  return(x) 
}
