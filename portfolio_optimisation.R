etf_symbols <- c("SHY", "EZU", "IYR", "IAU", "IGSB", "EWY", "IVV", "MUB", "EMB", 
                 "MCHI", "EWJ", "IEF", "IEFA", "AGG", "GOVT", "IEMG", "EFA", 
                 "EWZ", "SLV", "LQD", "EEM", "TLT", "FXI", "IWM", "HYG")

data_list <- lapply(etf_symbols, function(symbol) {
  getSymbols(symbol, from="2016-04-04", to="2022-04-09", auto.assign = FALSE)[,6]
})

names(data_list) <- etf_symbols

combinations <- combn(etf_symbols, 3)

results_optim <- data.frame(combination = character(), 
                      optimal_return = numeric(), 
                      optimal_sd = numeric(), 
                      optimal_Sharpe = numeric(),
                      weight1 = numeric(),
                      weight2 = numeric(),
                      weight3 = numeric())

for(i in 1:ncol(combinations)) {
  etf1_ret <- diff(log(data_list[[combinations[1,i]]]))
  etf2_ret <- diff(log(data_list[[combinations[2,i]]]))
  etf3_ret <- diff(log(data_list[[combinations[3,i]]]))
  
  ret_df <- na.omit(data.frame(etf1_ret, etf2_ret, etf3_ret))
  cov_mat <- cov(ret_df)
  mean_returns <- colMeans(ret_df)
  weights <- c(0.3, 0.3, 0.4)
  
  optimize_sharpe <- function(weights) {
    weights <- c(weights, 1 - sum(weights))
    portfolio_mean_return <- sum(weights * mean_returns)
    portfolio_std_dev <- sqrt(t(weights) %*% (cov_mat %*% weights))
    Sharpe <- -portfolio_mean_return / portfolio_std_dev
    return(Sharpe)
  }
  
  # Limit the weights to 10 times the original investment in both long and short directions
  result <- optim(
    par = weights[1:2],
    fn = optimize_sharpe, 
    method = "L-BFGS-B", 
    lower = rep(-10, 2),
    upper = rep(10, 2)
  )
  
  optimal_weights <- c(result$par, 1 - sum(result$par))
  optimal_Sharpe <- -result$value * sqrt(252)
  optimal_return <- t(optimal_weights) %*% mean_returns
  optimal_sd <- optimal_return / -result$value
  
  results_optim <- rbind(results_optim, data.frame(combination = paste(combinations[,i], collapse = ", "), 
                                       optimal_return = optimal_return, 
                                       optimal_sd = optimal_sd, 
                                       optimal_Sharpe = optimal_Sharpe,
                                       weight1 = optimal_weights[1],
                                       weight2 = optimal_weights[2],
                                       weight3 = optimal_weights[3]))
}

# Fetch new data for the assets from April 11, 2022, to April 12, 2023
new_data_list <- lapply(etf_symbols, function(symbol) {
  getSymbols(symbol, from="2022-04-11", to="2023-04-12", auto.assign = FALSE)[,6]
})

names(new_data_list) <- etf_symbols

# Create a new data frame to store the results
new_results_optim <- data.frame(combination = character(), 
                          optimal_return = numeric(), 
                          optimal_sd = numeric(), 
                          optimal_Sharpe = numeric(),
                          weight1 = numeric(),
                          weight2 = numeric(),
                          weight3 = numeric())

# Filter the results with Sharpe over 1
filtered_results <- subset(results_optim, optimal_Sharpe > 1)

# Iterate through the filtered combinations
for(i in 1:nrow(filtered_results)) {
  combination <- strsplit(as.character(filtered_results$combination[i]), ", ")[[1]]
  
  # Extract the optimal weights from the previous results
  optimal_weights <- as.numeric(filtered_results[i, c("weight1", "weight2", "weight3")])
  
  # Get the returns for the selected combination over the new data range
  etf1_ret <- diff(log(new_data_list[[combination[1]]]))
  etf2_ret <- diff(log(new_data_list[[combination[2]]]))
  etf3_ret <- diff(log(new_data_list[[combination[3]]]))
  
  ret_df <- na.omit(data.frame(etf1_ret, etf2_ret, etf3_ret))
  mean_returns <- colMeans(ret_df)
  cov_mat <- cov(ret_df)
  
  # Compute the performance metrics using the optimal weights
  portfolio_mean_return <- sum(optimal_weights * mean_returns)
  portfolio_std_dev <- sqrt(t(optimal_weights) %*% (cov_mat %*% optimal_weights))
  optimal_Sharpe <- portfolio_mean_return / portfolio_std_dev * sqrt(252)
  
  # Add the result to the new_results data frame
  new_results_optim <- rbind(new_results_optim, data.frame(combination = paste(combination, collapse = ", "), 
                                               optimal_return = portfolio_mean_return, 
                                               optimal_sd = portfolio_std_dev, 
                                               optimal_Sharpe = optimal_Sharpe,
                                               weight1 = optimal_weights[1],
                                               weight2 = optimal_weights[2],
                                               weight3 = optimal_weights[3]))
}

# The new_results data frame now contains the outcomes for the new date range, using the optimal weights from the previous analysis.