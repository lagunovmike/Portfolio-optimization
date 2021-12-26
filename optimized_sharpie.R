library(PortfolioAnalytics)
suppressPackageStartupMessages(library(tidyquant)) # To download the data
suppressPackageStartupMessages(library(plotly)) # To create interactive charts
suppressPackageStartupMessages(library(tidyverse))
# Examples of solving optimization problems to maximize mean return per unit StdDev

tick <- c("AAPL", "AMD", "PLZL.ME", "SBER.ME", "QCOM", "AXSM",
          "NVTA", "ACMR", "POLY.ME", "MOEX.ME", "FATE", "FRHC", "SONO")


price_from <- "2018-01-07"
price_to <- "2020-01-07"

price_data <- tq_get(tick,
                     from = price_from,
                     to = price_to,
                     get = 'stock.prices')

# convert russian assets to dollars

usdrub <- tq_get("RUB=X", from = price_from,
                 to = price_to,
                 get = 'stock.prices')
usdrub <- select(usdrub, date, adjusted)

price_data <- price_data %>%
    left_join(usdrub, by = "date", suffix = c(".stock", ".dollar")) %>%
    mutate(adjusted = if_else(grepl(".ME", price_data$symbol),
                              adjusted.stock / adjusted.dollar, 
                              adjusted.stock)) %>%
    select(-c(adjusted.dollar, adjusted.stock))

# end converting

log_ret_tidy <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')

log_ret_xts <- log_ret_tidy %>%
    pivot_wider(names_from = symbol, values_from = ret) %>%
    timetk::tk_xts(date_var = date, silent = T)

log_ret_xts_compl <- na.omit(log_ret_xts) 

############################ ---- ######################

R <- log_ret_xts_compl

funds <- colnames(R)


# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.constraint(portfolio=init.portf, type="position_limit", max_pos=4)
# init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type='risk', name='ETL',
                            arguments=list(p=0.95))

#init.portf

# Maximizing Sharpe Ratio can be formulated as a quadratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.

# The default action if "mean" and "StdDev" are specified as objectives with
# optimize_method="ROI" is to maximize quadratic utility. If we want to maximize
# Sharpe Ratio, we need to pass in maxSR=TRUE to optimize.portfolio.

maxSR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="ROI", 
                                   maxSR=TRUE, trace=TRUE)
# maxSR.lo.ROI
maxSRReturns <- Return.portfolio(R,weight = extractWeights(maxSR.lo.ROI), 
                                   rebalance_on = "days")
#round(extractWeights(maxSR.lo.ROI),2)

table.AnnualizedReturns(R = maxSRReturns, Rf = 0.05/252)

my_port.SRReturns <- Return.portfolio(log_ret_xts_compl, 
                                 weight = wts_have, 
                                 rebalance_on = "days")
table.AnnualizedReturns(R = my_port.SRReturns, Rf = 0.05/252)
##
wts <- as_tibble(t(extractWeights(maxSR.lo.ROI)))
p <- wts %>%
    gather(names(wts), 
           key = Asset,
           value = Weights) %>%
    filter(Weights > 0.001) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(Weights,2)), nudge_y = 0.02, size = 3.5,
              color = "darkgray") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = 'Assets', y = 'Weights', title = "Optimized Portfolio Weights",
         subtitle = paste("From:", min(index(R)), "to", max(index(R)),
                          "| SR:", round(SharpeRatio.annualized(maxSRReturns, 
                                                                Rf = 0.05/252)[1,1],2),
                          "| Return:", round(Return.annualized(maxSRReturns)[1,1],2),
                          "| Risk: ", round(StdDev.annualized(maxSRReturns)[1,1],2)))
p <- p + theme(legend.position = "none")
p

