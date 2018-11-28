ttt.win <- function(x, ply, emy){
  for (i in 1:3){
    for (j in 1:3){
      if (sum(x[i, ] == ply) == 2 & sum(x[i, ] == '_') == 1){
        x[i, which(x[i, ] == '_')] <- ply
        return(x)
      } else if (sum(x[, j] == ply) == 2 & sum(x[, j] == '_') == 1){
        x[which(x[, j] == '_'), j] <- ply
        return(x)
      } else if (sum(diag(x) == ply) == 2 & sum(diag(x) == '_') == 1){
        x[which(diag(x) == '_'), which(diag(x) == '_')] <- ply
        return(x)
      } else if (sum(diag(x[1:3, 3:1]) == ply) == 2 & sum(diag(x[1:3, 3:1]) == '_') == 1){
        x[which(diag(x[1:3, 3:1]) == '_'), which(diag(x[1:3, 3:1]) == '_')] <- ply
        return(x)
      } else {
        
      }
    }
  }
  return(x)
}
ttt.block <- function(x, ply, emy){
  for (i in 1:3){
    for (j in 1:3){
      if (sum(x[i, ] == emy) == 2 & sum(x[i, ] == '_') == 1){
        x[i, which(x[i, ] == '_')] <- ply
        return(x)
      } else if (sum(x[, j] == emy) == 2 & sum(x[, j] == '_') == 1){
        x[which(x[, j] == '_'), j] <- ply
        return(x)
      } else if (sum(diag(x) == emy) == 2 & sum(diag(x) == '_') == 1){
        x[which(diag(x) == '_'), which(diag(x) == '_')] <- ply
        return(x)
      } else if (sum(diag(x[1:3, 3:1]) == emy) == 2 & sum(diag(x[1:3, 3:1]) == '_') == 1){
        x[which(diag(x[1:3, 3:1]) == '_'), which(diag(x[1:3, 3:1]) == '_')] <- ply
        return(x)
      } else {
        
      }
    }
  }
  return(x)
}
ttt.fork <- function(x, ply, emy){
  du <- x
  ct <- 0
  for (i in 1:3){
    for (j in 1:3){
      if (du[i, j] == '_'){
        du[i, j] <- ply
        if (sum(du[i, ] == ply) == 2 & sum(du[i, ] == '_') == 1){
          ct <- ct + 1
        }
        if (sum(du[, j] == ply) == 2 & sum(du[, j] == '_') == 1){
          ct <- ct + 1
        }
        if (sum(diag(du) == ply) == 2 & sum(diag(du) == '_') == 1){
          ct <- ct + 1
        }
        if (sum(diag(du[1:3, 3:1]) == ply) == 2 & sum(diag(du[1:3, 3:1]) == '_') == 1){
          ct <- ct + 1
        }
        if (ct >= 2){
          return(du)
        }
        du <- x
        ct <- 0
      }
    }
  }
  return(x)
}
ttt.block.fork <- function(x, ply, emy){
  du <- x
  ct <- 0
  for (i in 1:3){
    for (j in 1:3){
      if (du[i, j] == '_'){
        du[i, j] <- emy
        if (sum(du[i, ] == emy) == 2 & sum(du[i, ] == '_') == 1){
          ct <- ct + 1
        }
        if (sum(du[, j] == emy) == 2 & sum(du[, j] == '_') == 1){
          ct <- ct + 1
        }
        if (sum(diag(du) == emy) == 2 & sum(diag(du) == '_') == 1){
          ct <- ct + 1
        }
        if (sum(diag(du[1:3, 3:1]) == emy) == 2 & sum(diag(du[1:3, 3:1]) == '_') == 1){
          ct <- ct + 1
        }
        if (ct >= 2){
          du[i, j] <- ply
          return(du)
        }
        du <- x
        ct <- 0
      }
    }
  }
  return(x)
}
ttt.center <- function(x, ply, emy){
  if (x[2, 2] == '_')
  x[2, 2] <- ply
  return(x)
}
ttt.opposite.corner <- function(x, ply, emy){
  if (x[1, 1] == emy & x[3, 3] == '_'){
    x[3, 3] <- ply
    return(x)
  } else if (x[1, 3] == emy & x[3, 1] == '_'){
    x[3, 1] <- ply
    return(x)
  } else if (x[3, 1] == emy & x[1, 3] == '_'){
    x[1, 3] <- ply
    return(x)
  } else if (x[3, 3] == emy & x[1, 1] == '_'){
    x[1, 1] <- ply
    return(x)
  } else {
    return(x)
  }
}
ttt.empty.corner <- function(x, ply, emy){
  if (x[1, 1] == '_'){
    x[1, 1] <- ply
    return(x)
  } else if (x[1, 3] == '_'){
    x[1, 3] <- ply
    return(x)
  } else if (x[3, 1] == '_'){
    x[3, 1] <- ply
    return(x)
  } else if (x[3, 3] == '_'){
    x[3, 3] <- ply
    return(x)
  } else {
    return(x)
  }
}
ttt.empty.side <- function(x, ply, emy){
  for (i in 1:3){
    for (j in 1:3){
      if (x[i, j] == '_'){
        x[i, j] <- ply
        return(x)
      }
    }
  }
}

ply_set <- c('O', 'X')

ttt <- function(x, ply, emy){
  du <- ttt.win(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.block(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.fork(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.block.fork(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.center(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.opposite.corner(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.empty.corner(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
  du <- ttt.empty.side(x, ply, emy)
  if (!all(du == x)){
    print(du)
    return(du)
  }
}

ply <- 'O'
emy <- 'X'
x <- matrix('_', nrow = 3, ncol = 3)


ttt_run <- function(x, ply, emy){
  while (sum(x == '_') != 0){
    x <- ttt(x, ply, emy)
    
    ply <- ply_set[which(ply_set == ply) %% 2 + 1]
    emy <- ply_set[which(ply_set == emy) %% 2 + 1]
  }
}

ttt_run(x, ply, emy)
