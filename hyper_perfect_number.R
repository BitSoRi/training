len_p <- 1000

sum_div <- function(x){
  tar <- 0
  for (i in 1:floor(sqrt(x))){
    if (x %% i == 0){
      tar <- tar + i + (x / i)
    }
  }
  if (floor(sqrt(x)) == sqrt(x)){
    tar <- tar - sqrt(x)
  }
  return(tar)
}

answer <- data.frame(NULL)

for (i in 2:len_p){
  for (j in 1:i){
    if ((1 + j * (sum_div(i) - i - 1)) == i){
      answer <- rbind(answer, c(i, j))
    }
  }
}
names(answer) <- c('n', 'k')

answer
