library(stringr)
prob <- matrix(NA, nrow = 9, ncol = 9)

prob[1, ] <- strsplit('103450000', split = '')[[1]]
prob[2, ] <- strsplit('400709123', split = '')[[1]]
prob[3, ] <- strsplit('789023056', split = '')[[1]]
prob[4, ] <- strsplit('201064089', split = '')[[1]]
prob[5, ] <- strsplit('564000231', split = '')[[1]]
prob[6, ] <- strsplit('890201504', split = '')[[1]]
prob[7, ] <- strsplit('312645000', split = '')[[1]]
prob[8, ] <- strsplit('645900012', split = '')[[1]]
prob[9, ] <- strsplit('000312045', split = '')[[1]]

duprob <- prob

find_zero <- function(x){
  for (i in 1:9){
    if (sum(str_count(x[i, ], '0')) == 1){
      goal <- setdiff(c(1:9), as.numeric(x[i, ]))
      for (j in 1:9){
        if (x[i, j] == '0'){
          x[i, j] <- as.character(goal)
        }
      }
    }
  }
  for (i in 1:9){
    if (sum(str_count(x[, i], '0')) == 1){
      goal <- setdiff(c(1:9), as.numeric(x[, i]))
      for (j in 1:9){
        if (x[j, i] == '0'){
          x[j, i] <- as.character(goal)
        }
      }
    }
  }
  return(x)
}
find_blank <- function(x){
  for (i in 1:9){
    if (sum(str_count(x, as.character(i))) == 8){
      
      goal <- i
      
      for (j in 1:9){
        if (sum(str_count(x[, j], as.character(goal))) == 0){
          goal_col <- j
        }
      }
      for (j in 1:9){
        if (sum(str_count(x[j, ], as.character(goal))) == 0){
          goal_row <- j
        }
      }
      
      if (x[goal_row, goal_col] == '0'){
        x[goal_row, goal_col] <- as.character(goal)
      } else {
        print(goal)
      }
    }
  }
  return(x)
}
find_cross <- function(x){
  for (goal in 1:9){
    for (i in 1:9){ #search in row
      
      can_set <- NULL
      del_set <- NULL
      
      if (sum(str_count(x[i, ], as.character(goal))) == 0){
        
        goal_row <- i
        
        for (j in 1:9){
          if (x[i, j] == '0'){
            can_set <- c(can_set, j)
          }
          if (sum(str_count(x[, j], as.character(goal))) == 1){
            del_set <- c(del_set, j)
          }
        }
        
        goal_col <- setdiff(can_set, del_set)
        
        if (length(goal_col) == 1 & x[goal_row, goal_col] == '0'){
          x[goal_row, goal_col] <- as.character(goal)
        } 
      }
    }
  }
  for (i in 1:9){ #search in row
    
    can_set <- NULL
    del_set <- NULL
    
    if (sum(str_count(x[, i], as.character(goal))) == 0){
      
      goal_col <- i
      
      for (j in 1:9){
        if (x[j, i] == '0'){
          can_set <- c(can_set, j)
        }
        if (sum(str_count(x[j, ], as.character(goal))) == 1){
          del_set <- c(del_set, j)
        }
      }
      
      goal_row <- setdiff(can_set, del_set)
      
      if (length(goal_row) == 1 & x[goal_row, goal_col] == '0'){
        x[goal_row, goal_col] <- as.character(goal)
      } 
    }
  }
  return(x)
}


while (sum(str_count(duprob, '0')) == 0){
  duprob <- find_zero(duprob)
  duprob <- find_blank(duprob)
  duprob <- find_cross(duprob)
}

answer <- matrix(as.numeric(duprob), nrow = 9, ncol = 9)