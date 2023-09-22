library(quantmod)
library(urca)
library(zoo)

# Include any set of symbols with historical data available on Yahoo Finance for the chosen period
etfs <- c("SHY","EZU","IYR","IAU", "IGSB", "EWY", "IVV", "MUB", "EMB",
          "MCHI", "EWJ", "IEF", "IEFA", "AGG", "GOVT", "IEMG", "EFA", 
          "EWZ", "SLV", "LQD", "EEM", "TLT", "FXI", "IWM", "HYG")

# The first period is for backtesting to identify symbol combinations which perform well with mean reversion strategy
data_list <- lapply(etfs, function(symbol) {
  getSymbols(symbol, from="2016-04-04", to="2022-04-09", auto.assign = FALSE)[,6]
})
names(data_list) <- etfs
combinations <- combn(etfs, 3)
results <- data.frame(combination = character(), TestStatistic = numeric(), halflife = numeric(), APR = numeric(), Sharpe = numeric(), stringsAsFactors = FALSE)

# My adaptation of mean reversion strategy programmed in MATLAB by Chan (2013)
for(i in 1:ncol(combinations)) {
  etf1 <- data_list[[combinations[1,i]]]
  etf2 <- data_list[[combinations[2,i]]]
  etf3 <- data_list[[combinations[3,i]]]
  merged_data <- na.omit(merge.xts(etf1, etf2, etf3))
  test <- ca.jo(data.frame(merged_data), type="trace", K=2, ecdet="none", spec="longrun")
  test_statistic <- test@teststat[1]
  eigenvector <- test@V[,1]
  prices <- as.matrix(merged_data)
  yport <- prices %*% eigenvector
  ylag <- lag(yport, 1)
  deltaY <- diff(yport)
  data <- data.frame(deltaY = deltaY, ylag = coredata(ylag[-length(ylag)]), intercept = rep(1, length(ylag)-1))
  regress_results <- lm(deltaY ~ ylag + intercept, data = data)
  halflife <- -log(2) / coef(regress_results)["ylag"]
  halflife <- pmax(pmin(halflife, (nrow(etf1) / 2)), 2)
  lookback <- round(halflife)
  numUnits <- -(yport - rollmean(yport, lookback, fill = NA)) / rollapply(yport, lookback, sd, fill = NA)
  numUnits_mat <- matrix(rep(numUnits, each = 3), nrow = length(numUnits), ncol = 3, byrow = TRUE)
  eigenvector_mat <- matrix(rep(eigenvector, each = nrow(prices)), nrow = nrow(prices), ncol = 3, byrow = TRUE)
  actual_weight <- numUnits_mat * eigenvector_mat
  actual_weight <- pmax(pmin(actual_weight, 10), -10)
  positions <- actual_weight * prices
  positions_lag <- rbind(NA, head(positions, -1))
  price_changes <- prices - rbind(NA, head(prices, -1))
  pnl <- rowSums(positions_lag * price_changes / rbind(NA, head(prices, -1)), na.rm = TRUE)
  ret <- pnl / rowSums(abs(positions_lag), na.rm = TRUE)
  APR <- mean(ret, na.rm = TRUE) * 252 * 100
  Sharpe <- mean(ret, na.rm = TRUE) / sd(ret, na.rm = TRUE) * sqrt(252)    
  results <- rbind(results, data.frame(combination = paste(combinations[,i], collapse = ", "), TestStatistic = test_statistic, halflife = as.numeric(halflife), APR = APR, Sharpe = Sharpe))
}

filtered_combinations <- results[results$Sharpe > 1,]$combination
new_data_list <- lapply(etfs, function(symbol) {
  getSymbols(symbol, from="2022-04-11", to="2023-04-12", auto.assign = FALSE)[,6]
})
names(new_data_list) <- etfs
new_results <- data.frame(combination = character(), TestStatistic = numeric(), halflife = numeric(), APR = numeric(), Sharpe = numeric(), stringsAsFactors = FALSE)
for(comb in filtered_combinations) {
  current_combination <- unlist(strsplit(comb, ", "))
  etf1 <- new_data_list[[current_combination[1]]]
  etf2 <- new_data_list[[current_combination[2]]]
  etf3 <- new_data_list[[current_combination[3]]]
  merged_data <- na.omit(merge.xts(etf1, etf2, etf3))
  test <- ca.jo(data.frame(merged_data), type="trace", K=2, ecdet="none", spec="longrun")
  test_statistic <- test@teststat[1]
  eigenvector <- test@V[,1]
  prices <- as.matrix(merged_data)
  yport <- prices %*% eigenvector
  ylag <- lag(yport, 1)
  deltaY <- diff(yport)
  data <- data.frame(deltaY = deltaY, ylag = coredata(ylag[-length(ylag)]), intercept = rep(1, length(ylag)-1))
  regress_results <- lm(deltaY ~ ylag + intercept, data = data)
  halflife <- -log(2) / coef(regress_results)["ylag"]
  halflife <- pmax(pmin(halflife, (nrow(etf1) / 2)), 2)
  lookback <- round(halflife)
  numUnits <- -(yport - rollmean(yport, lookback, fill = NA)) / rollapply(yport, lookback, sd, fill = NA)
  numUnits_mat <- matrix(rep(numUnits, each = 3), nrow = length(numUnits), ncol = 3, byrow = TRUE)
  eigenvector_mat <- matrix(rep(eigenvector, each = nrow(prices)), nrow = nrow(prices), ncol = 3, byrow = TRUE)
  actual_weight <- numUnits_mat * eigenvector_mat
  actual_weight <- pmax(pmin(actual_weight, 10), -10)
  positions <- actual_weight * prices
  positions_lag <- rbind(NA, head(positions, -1))
  price_changes <- prices - rbind(NA, head(prices, -1))
  pnl <- rowSums(positions_lag * price_changes / rbind(NA, head(prices, -1)), na.rm = TRUE)
  ret <- pnl / rowSums(abs(positions_lag), na.rm = TRUE)
  APR <- mean(ret, na.rm = TRUE) * 252 * 100
  Sharpe <- mean(ret, na.rm = TRUE) / sd(ret, na.rm = TRUE) * sqrt(252)    
  new_results <- rbind(new_results, data.frame(combination = comb, TestStatistic = test_statistic, halflife = as.numeric(halflife), APR = APR, Sharpe = Sharpe))
}
