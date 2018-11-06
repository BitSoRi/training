set <- c(10, 19, 20, 20, 30, 39, 40, 41, 50, 50,
         60, 60, 70, 71, 80, 89, 90, 91, 100, 100)

ap <- function(x){
  return(x %/% 10)
}
dui <- function(x){
  return(x %% 10)
}

jg <- c(12, 14, 19, 20, 50, 46)

tsf <- function(x){
  return(10 * ap(min(x)) + ap(max(x)))
}

n <- as.numeric(readline())

shuffle <- sample(1:20, size = 2*n)

for (i in 1:n){
  temp_n <- sprintf('p%d <- c(set[shuffle[i]], set[shuffle[i + n]])', i)
  eval(parse(text = temp_n))
}

jb <- function(x){
  if (dui(x[1]) == 9 & dui(x[2]) == 9){
    return(9999)
  } else if (ap(x[1]) == ap(x[2])){
    return(100 + ap(x[1]))
  } else if (is.element(tsf(x), jg)){
    mtc <- which(tsf(x) == jg)
    return((7 - mtc) * 10)
  } else if (ap(min(x)) == 3 & ap(max(x)) == 7){
    return(0.37)
  } else if (ap(min(x)) == 4 & ap(max(x)) == 9){
    return(NA)
  } else {
    cal <- ap(x[1]) + ap(x[2])
    return(cal %% 10)
  }
}
b <- rep(NA, n)

for (i in 1:n){
  b[i] <- as.numeric(readline())
}
 
for (i in 1:n){
  if (b[i] == 1){
    temp_b <- sprintf('b%d <- jb(p%d)', i, i)
    eval(parse(text = temp_b))
  } else {
    temp_d <- sprintf('b%d <- NULL', i)
    eval(parse(text = temp_d))
    sprintf('p%d die')
  }
  if (i == 1){
    dtf <- data.frame(b[1], b1)
  } else {
    temp_dt <- sprintf('dtf <- rbind(dtf, c(b[%d], b%d))', i, i)
    eval(parse(text = temp_dt))
  }
}
names(dtf) <- c('bet', 'index')

pb <- function(x){
  ct <- length(which(x$bet == 1))
  ss <- x[x$bet == 1, ]
  sur <- ss$index
  if (is.element(9999, sur)){
    temp_w <- as.numeric(which(x$index == 9999))
    sprintf('p%d is winner', temp_w)
  } else if (is.element(NA, sur)){
    if (is.element(110, sur)){
      temp_w <- as.numeric(which(x$index == 110))
      sprintf('p%d is winner', temp_w)
    } else {
      sprintf('replay')
    }
  } else if (is.element(0.37, sur)){
    if (sum(sur %/% 100 == 1) > 0){
      temp_w <- as.numeric(which(x$index == 0.37))
      sprintf('p%d is winner', temp_w)
    }
  } else {
    temp_w <- as.numeric(which(x$index == max(sur)))
    sprintf('p%d is winner', temp_w)
  }
}

pb(dtf)
