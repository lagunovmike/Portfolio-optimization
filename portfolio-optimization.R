# Load required libraries
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyverse)

# Define ticker symbols for stocks to include in portfolio
ticker_symbols <- c('SBER.ME', 'AAPL', 'PLZL.ME', 'ALRS.ME', 'MTSS.ME',
                    "GAZP.ME", "LKOH.ME", "YNDX.ME", "MOEX.ME", "QIWI.ME")

# Download stock price data from Yahoo Finance
price_data <- tq_get(ticker_symbols,
                     from = '2018-11-03',
                     to = '2020-11-03',
                     get = 'stock.prices')

# Calculate daily log returns for each stock
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

# Convert log returns to xts object and remove any missing values
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()
log_ret_xts_compl <- na.omit(log_ret_xts) 

# Calculate mean returns for each stock
mean_returns <- colMeans(log_ret_xts_compl)

# Calculate annualized covariance matrix for stocks
covariance_matrix <- cov(log_ret_xts_compl) * 252

# Define number of portfolios to simulate
num_portfolios <- 5000

# Initialize empty matrix to store asset weightsfor each portfolio
all_wts <- matrix(nrow = num_portfolios,
                  ncol = length(ticker_symbols))

# Initialize empty vectors to store portfolio returns, risk, and Sharpe ratios
portfolio_returns <- vector('numeric', length = num_portfolios)
portfolio_risk <- vector('numeric', length = num_portfolios)
sharpe_ratio <- vector('numeric', length = num_portfolios)

# Set risk-free rate
risk_free_rate <- 0.06

# Perform Monte Carlo simulation
for (i in seq_along(portfolio_returns)) {
  # Generate random weights for assets
  weights <- runif(length(ticker_symbols))
  weights <- weights /sum(weights)
  
  # Store asset weights for current portfolio
  all_wts[i,] <- weights
  
  # Calculate portfolio returns
  port_ret <- sum(weights * mean_returns)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Store portfolio return values
  portfolio_returns[i] <- port_ret
  
  # Create and store portfolio risk
  port_sd <- sqrt(t(weights) %*% (covariance_matrix   %*% weights ))
  portfolio_risk[i] <- port_sd
  
  # Create and store portfolio Sharpe Ratios
  
  sr <- (port_ret-risk_free_rate)/port_sd
  sharpe_ratio[i] <- sr
  
}


# Storing the values in the table
portfolio_values <- tibble(Return = portfolio_returns,
                           Risk = portfolio_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

#head(portfolio_values)

# The minimum variance portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk),]

p <- min_var %>%
  gather(names(min_var)[1]:names(min_var)[length(names(min_var))-3], 
         key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

# The tangency portfolio (the portfolio with highest sharpe ratio)
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

p <- max_sr %>%
  gather(names(min_var)[1]:names(min_var)[length(names(min_var))-3], 
         key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

ggplotly(p)

# Visualizing all the portfolios

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'green') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')

ggplotly(p)