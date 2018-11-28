text_raw <- readline()
text_blank <- strsplit(text_raw, split = ' ')

text_sblank <- text_blank[[1]]
text_comma <- strsplit(text_sblank, split = ',')

text_scomma <- rep(NA)
for (i in seq(text_comma)){
  text_scomma[i] <- text_comma[[i]]
}

text_period <- strsplit(text_scomma, split = '.', fixed = T)
text_speriod <- rep(NA)
for (i in seq(text_period)){
  text_speriod[i] <- text_period[[i]]
}

text_set <- union(text_speriod, NULL)

text_df <- NULL

for (i in seq(text_set)){
  temp <- c(text_set[i], sum(text_speriod == text_set[i]))
  text_df <- rbind(text_df, temp)
}

head(text_df[order(text_df[, 2], decreasing = T), ], 10)
