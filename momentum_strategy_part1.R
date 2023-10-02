# Part 1 is identifying optimal lookback and holding periods for every future, which are variables needed for the momentum strategy application. 

# List of futures to be tested
futures_symbols <- c("ZB=F", "NQ=F", "YM=F", "ZM=F", "NKD=F",
                     "CL=F", "GC=F", "NG=F", "HG=F", "SI=F",
                     "6E=F", "6J=F", "6B=F", "6A=F", "6C=F",
                     "ZT=F", "ZQ=F", "ZF=F", "ZN=F", "ZB=F",
                     "ZC=F", "ZW=F", "CT=F", "ZL=F", "ZO=F")

# Initialize an empty data frame to store the final results
final_results_df <- data.frame(
  symbol = character(0),
  lookback = integer(0),
  holddays = integer(0),
  cc = numeric(0),
  lowest_pval = numeric(0)
)

# Loop through each future
for (symbol in futures_symbols) {
  # Import historical data
  getSymbols(symbol, from="2010-04-11", to="2018-04-11", auto.assign = FALSE) -> data
  cl <- unclass(data[,6])
  cl <- na.omit(cl)
  
  # Initialize an empty data frame to store intermediate results
  results_df <- data.frame(
    lookback = integer(0),
    holddays = integer(0),
    cc = numeric(0),
    pval = numeric(0)
  )
  
  # Nested loop to iterate through different lookback and holddays
  for (lookback in c(60, 120, 250)) {
    for (holddays in c(1, 5, 10, 25)) {
      # Calculate lagged and future returns
      ret_lag <- (cl - lag(cl, lookback)) / lag(cl, lookback)
      ret_fut <- (lead(cl, holddays) - cl) / cl
      
      # Remove NaN values
      badDates <- is.na(ret_lag) | is.na(ret_fut)
      ret_lag <- ret_lag[!badDates]
      ret_fut <- ret_fut[!badDates]
      
      # Generate independent set of observations
      if (lookback >= holddays) {
        indepSet <- seq(1, length(ret_lag), by = lookback)
      } else {
        indepSet <- seq(1, length(ret_lag), by = holddays)
      }
      
      ret_lag <- ret_lag[indepSet]
      ret_fut <- ret_fut[indepSet]
      
      # Calculate correlation coefficient and p-value
      cor_test <- cor.test(ret_lag, ret_fut)
      cc <- cor_test$estimate
      pval <- cor_test$p.value
      
      # Append the results to the data frame
      results_df <- rbind(results_df, data.frame(lookback, holddays, cc, pval))
    }
  }
  
  # Find the index of the row with the lowest p-value
  min_pval_index <- which.min(results_df$pval)
  
  # Extract the lookback, holddays, and cc associated with the lowest p-value
  lowest_pval_lookback <- results_df$lookback[min_pval_index]
  lowest_pval_holddays <- results_df$holddays[min_pval_index]
  lowest_pval_cc <- results_df$cc[min_pval_index]
  lowest_pval <- results_df$pval[min_pval_index]
  
  # Append the final results to the data frame
  final_results_df <- rbind(final_results_df, data.frame(symbol, lowest_pval_lookback, lowest_pval_holddays, lowest_pval_cc, lowest_pval))
}