# Initialize the data frame to store best results for each symbol
best_results <- data.frame(
  "Symbol" = character(),
  "Short_MA" = numeric(),
  "Long_MA" = numeric(),
  "APR" = numeric(),
  "Sharpe" = numeric()
)

# Moving average lengths to test
short_lengths <- c(5, 10, 15, 20, 30)
long_lengths <- c(50, 100, 150, 200, 250)

# List of futures symbols
futures_symbols <- c("ES=F", "NQ=F", "YM=F", "ZM=F", "NKD=F",
                     "CL=F", "GC=F", "NG=F", "HG=F", "SI=F",
                     "6E=F", "6J=F", "6B=F", "6A=F", "6C=F",
                     "ZT=F", "ZQ=F", "ZF=F", "ZN=F", "ZB=F",
                     "ZC=F", "ZW=F", "CT=F", "ZL=F", "ZO=F")

# Loop through each futures symbol
for (sym in futures_symbols) {
  
  # Get the data
  getSymbols(sym, from="2010-04-11", to="2018-04-11", auto.assign = FALSE) -> data
  cl <- unclass(data[,6])  # Adjusted Close column
  cl <- na.omit(cl)
  
  # Initialize the data frame to store results for this symbol
  results <- data.frame(
    "Short_MA" = numeric(),
    "Long_MA" = numeric(),
    "APR" = numeric(),
    "Sharpe" = numeric()
  )
  
  # Loop through all combinations of short and long moving averages
  for (s in short_lengths) {
    for (l in long_lengths) {
      
      # Calculate Moving Averages
      short_ma <- SMA(cl, n = s)
      long_ma <- SMA(cl, n = l)
      
      # Align data lengths by removing NA
      valid_idx <- complete.cases(short_ma, long_ma)
      short_ma <- short_ma[valid_idx]
      long_ma <- long_ma[valid_idx]
      cl_valid <- cl[valid_idx]
      
      # Generate Trading Signals
      signal <- ifelse(short_ma > long_ma, 1, 0)
      lag_signal <- lag(signal, 1)
      lag_signal <- na.omit(lag_signal)
      
      # Calculate Logarithmic Daily Returns
      log_ret <- diff(log(cl_valid))
      log_ret <- na.omit(log_ret)
      
      # Calculate Strategy Log Returns
      strategy_log_ret = lag_signal * log_ret
      
      # Performance Metrics
      APR = mean(strategy_log_ret, na.rm = TRUE) * 252 * 100
      Sharpe = (mean(strategy_log_ret, na.rm = TRUE) / sd(strategy_log_ret, na.rm = TRUE)) * sqrt(252)
      
      # Add results to data frame
      results <- rbind(results, data.frame("Short_MA" = s, "Long_MA" = l, "APR" = APR, "Sharpe" = Sharpe))
    }
  }
  
  # Find the highest Sharpe ratio for this symbol
  best_row <- results[which.max(results$Sharpe),]
  best_row <- cbind(Symbol = sym, best_row)
  
  # Add to the best_results data frame
  best_results <- rbind(best_results, best_row)
}

best_results_sharpeover0 <- best_results[best_results$Sharpe > 0,]

final_results <- data.frame(
  "Symbol" = character(),
  "Short_MA" = numeric(),
  "Long_MA" = numeric(),
  "APR" = numeric(),
  "Sharpe" = numeric()
)

# Loop through all futures in best_results_sharpeover0
for (i in 1:nrow(best_results_sharpeover0)) {
  
  # Get symbol and corresponding MA lengths from best_results_sharpeover0
  symbol <- best_results_sharpeover0$Symbol[i]
  short_ma_length <- best_results_sharpeover0$Short_MA[i]
  long_ma_length <- best_results_sharpeover0$Long_MA[i]
  
  # Get the data for the symbol
  getSymbols(symbol, from="2018-04-12", to="2023-04-12", auto.assign = FALSE) -> data
  cl <- unclass(data[, paste0(symbol, ".Adjusted")])
  cl <- na.omit(cl)
  
  # Calculate Moving Averages
  short_ma <- SMA(cl, n = short_ma_length)
  long_ma <- SMA(cl, n = long_ma_length)
  
  # Align data lengths by removing NA
  valid_idx <- complete.cases(short_ma, long_ma)
  short_ma <- short_ma[valid_idx]
  long_ma <- long_ma[valid_idx]
  cl_valid <- cl[valid_idx]
  
  # Generate Trading Signals
  signal <- ifelse(short_ma > long_ma, 1, 0)
  lag_signal <- lag(signal, 1)
  lag_signal <- na.omit(lag_signal)
  
  # Calculate Logarithmic Daily Returns
  log_ret <- diff(log(cl_valid))
  
  # Calculate Strategy Log Returns
  strategy_log_ret = lag_signal * log_ret
  
  # Performance Metrics
  APR = mean(strategy_log_ret, na.rm = TRUE) * 252 * 100
  Sharpe = (mean(strategy_log_ret, na.rm = TRUE) / sd(strategy_log_ret, na.rm = TRUE)) * sqrt(252)
  
  # Add results to data frame
  final_results <- rbind(final_results, data.frame("Symbol" = symbol, "Short_MA" = short_ma_length, "Long_MA" = long_ma_length, "APR" = APR, "Sharpe" = Sharpe))
}
