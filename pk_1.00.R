transf.deck <- function(x){
  cmb_deck <- matrix(ncol = 2, nrow = length(x))
  spt_deck <- strsplit(x, '')
  for (i in seq(length(x))){
    cmb_deck[i, 1] <- spt_deck[[i]][1]
    cmb_deck[i, 2] <- spt_deck[[i]][2]
  }
  cmb_deck <- gsub('S', 1, cmb_deck)
  cmb_deck <- gsub('D', 2, cmb_deck)
  cmb_deck <- gsub('H', 3, cmb_deck)
  cmb_deck <- gsub('C', 4, cmb_deck)
  cmb_deck <- gsub('J', 11, cmb_deck)
  cmb_deck <- gsub('Q', 12, cmb_deck)
  cmb_deck <- gsub('K', 13, cmb_deck)
  cmb_deck <- gsub('A', 14, cmb_deck)
  
  rem_deck <- rep(NA, length(x))
  for (i in seq(length(x))){
    rem_deck[i] <- as.numeric(paste(cmb_deck[i, 1], cmb_deck[i, 2], sep = ''))
  }
  return(rem_deck)
}

deck <- c(141, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131,
          142, 22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122, 132,
          143, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113, 123, 133,
          144, 24, 34, 44, 54, 64, 74, 84, 94, 104, 114, 124, 134)

shuffle <- sample(c(1:52), 10)
white <- sort(deck[head(shuffle, 5)])
black <- sort(deck[tail(shuffle, 5)])

card.pat <- function(x){
  return(x %% 10)
}
card.num <- function(x){
  return(x %/% 10)
}

pk <- function(x){
  
  if (all(card.pat(x) == card.pat(x[1])) & all(c(card.num(x[1]):(card.num(x[1]) + 4)) == card.num(x))){
    point_pk <- 800 + max(card.num(x))
  } else if (all(card.num(x[1:4]) == card.num(x[1])) | all(card.num(x[2:5]) == card.num(x[5]))){
    point_pk <- 700 + max(card.num(x))
  } else if (all(card.num(x[1:3]) == card.num(x[1])) & all(card.num(x[4:5]) == card.num(x[5]))){
    point_pk <- 600 + card.num(x[1])
  } else if (all(card.num(x[1:2]) == card.num(x[1])) & all(card.num(x[3:5]) == card.num(x[5]))){
    point_pk <- 600 + card.num(x[5])
  } else if (all(card.pat(x) == card.pat(x[1]))){
    point_pk <- 500 + max(card.num(x))
  } else if (all(c(card.num(x[1]):(card.num(x[1]) + 4)) == card.num(x))){
    point_pk <- 400 + max(card.num(x))
  } else if (all(card.num(x[1:3]) == card.num(x[1])) | all(card.num(x[3:5]) == card.num(x[5]))){
    point_pk <- 300 + card.num(x[3])
  } else if (length(union(card.num(x), NULL)) == 3){
    temp_pair <- which(table(card.num(x)) == 2)
    point_pk <- 200 + max(as.numeric(names(temp_pair)))
  } else if (length(union(card.num(x), NULL)) == 4){
    temp_pair <- which(table(card.num(x)) == 2)
    point_pk <- 100 + as.numeric(names(temp_pair))
  } else {
    point_pk <- card.num(x[5])
  }
  
  if (any(card.num(x) == 14)){
    du <- x
    du[card.num(du) == 14] <- du[card.num(du) == 14] - 130
    du <- sort(du)
    if (all(card.pat(du) == card.pat(du[1])) & c(card.num(du[1]):(card.num(du[1]) + 5)) == card.num(du)){
      point_pk <- 800 + max(card.num(du))
    } else if (c(card.num(du[1]):(card.num(du[1]) + 5)) == card.num(du) & point_pk < 400){
      point_pk <- 400 + max(card.num(du))
    }
  }
  return(point_pk)
}
pb <- function(x, y){
  if (x > y){
    print('P1 win')
  } else if (y > x){
    print('P2 win')
  } else if (x > 200 & x < 300){
    tp_x <- as.numeric(names(which(table(card.num(x)) == 1)))
    tp_y <- as.numeric(names(which(table(card.num(y)) == 1)))
    if (x > y){
      print('P1 win')
    } else if (y < x){
      print('P2 win')
    }
  } else if (x > 100 & x < 200){
    tp_x <- max(as.numeric(names(which(table(card.num(x)) == 1))))
    tp_y <- max(as.numeric(names(which(table(card.num(y)) == 1))))
    if (x > y){
      print('P1 win')
    } else if (y < x){
      print('P2 win')
    }
  }
}

point_white <- pk(white)
point_black <- pk(black)

pb(point_white, point_black)