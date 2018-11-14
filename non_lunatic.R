combn.cal <- function(n, r){
  nCr <- factorial(n) / factorial(n - r) / factorial (r)
  return(nCr)
}

len_exp <- 6
non_lunatic <- 0

for (i in 1:len_exp){
  if ((i - 1) > 9){
    for (j in 1:9){
      non_lunatic <- non_lunatic + combn.cal(9, j) * combn.cal(i - 1, j - 1)
    }
  } else if ((i - 1) <= 9){
    for (j in 1:i){
      non_lunatic <- non_lunatic + combn.cal(9, j) * combn.cal(i - 1, j - 1)
    }
  }
  if (i == 1){
    next()
  } else {
    if ((i - 1) > 10){
      for (j in 2:10){
        non_lunatic <- non_lunatic + combn.cal(10, j) * combn.cal(i - 1, j - 1)
      }
    } else if ((i - 1) <= 10){
      for (j in 2:i){
        non_lunatic <- non_lunatic + combn.cal(10, j) * combn.cal(i - 1, j - 1)
      }
    }
  }
}


