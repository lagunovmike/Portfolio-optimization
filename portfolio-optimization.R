library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyverse)

tick <- c('SBER.ME', 'AAPL', 'PLZL.ME', 'ALRS.ME', 'MTSS.ME',
          "GAZP.ME", "LKOH.ME", "YNDX.ME", "MOEX.ME", "QIWI.ME")

price_data <- tq_get(tick,
                     from = '2018-11-03',
                     to = '2020-11-03',
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')

log_ret_xts <- log_ret_tidy %>%
    spread(symbol, value = ret) %>%
    tk_xts()

log_ret_xts_compl <- na.omit(log_ret_xts) 

mean_ret <- colMeans(log_ret_xts_compl)
# print(round(mean_ret, 5))

cov_mat <- cov(log_ret_xts_compl) * 252

# print(round(cov_mat,4))



# Optimization

num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

risk_free <- 0.06

# Simulation
for (i in seq_along(port_returns)) {
    
    wts <- runif(length(tick))
    wts <- wts/sum(wts)
    
    # Storing weight in the matrix
    all_wts[i,] <- wts
    
    # Portfolio returns
    
    port_ret <- sum(wts * mean_ret)
    port_ret <- ((port_ret + 1)^252) - 1
    
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    
    
    # Creating and storing portfolio risk
    port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
    port_risk[i] <- port_sd
    
    # Creating and storing Portfolio Sharpe Ratios
    # Assuming 0% Risk free rate
    
    sr <- (port_ret-risk_free)/port_sd
    sharpe_ratio[i] <- sr
    
}


# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
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
                   y = Return), data = min_var, color = 'red') +
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red')


ggplotly(p)