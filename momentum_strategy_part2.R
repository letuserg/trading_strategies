#Part 2 is applying the optimal lookback and holding periods to the momentum strategy with each individual futures contract.

getSymbols("GC=F", from="2018-04-12", to="2023-04-12", auto.assign = FALSE) -> data
cl <- unclass(data$`GC=F.Adjusted`)
cl <- na.omit(cl)

lookback <- 250
holddays <- 5

longs <- cl > lag(cl, lookback)
shorts <- cl < lag(cl, lookback)

pos <- numeric(length(cl))

for (h in 0:(holddays - 1)) {
  
  long_lag <- lag(longs, h)
  long_lag[is.na(long_lag)] <- FALSE
  
  short_lag <- lag(shorts, h)
  short_lag[is.na(short_lag)] <- FALSE
  
  pos[long_lag] <- pos[long_lag] + 1
  pos[short_lag] <- pos[short_lag] - 1
}

ret <- lag(pos) * (cl - lag(cl)) / lag(cl) / holddays
ret <- ret[!is.na(ret)]
APR <- mean(ret, na.rm = TRUE) * 252 * 100
print(APR)
sharpe_ratio <- mean(ret, na.rm = TRUE) / sd(ret, na.rm = TRUE) * sqrt(252)
print(sharpe_ratio)