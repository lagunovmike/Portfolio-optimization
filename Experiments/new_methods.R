library(PortfolioAnalytics)

port <- portfolio.spec(assets = colnames((log_ret_xts_compl)))
port

port <- add.constraint(portfolio = port,
                       type = "full_investment")
port <- add.constraint(portfolio = port,
                       type = "long_only")
port <- add.constraint(portfolio = port,
                       type = "box",
                       min = 0.0,
                       max = 0.7)
port

portMinVar <- port

portMinVar <- add.objective(portfolio = portMinVar,
                            type = "risk",
                            name = "StdDev")
portMinVar

portMeanVar <- port
portMeanVar <- add.objective(portfolio = portMeanVar,
                             type = "risk",
                             name = "StdDev")
portMeanVar <- add.objective(portfolio = portMeanVar,
                             type = "return",
                             name = "mean")
portMeanVar

set.seed(10260)


minVarOpt <- optimize.portfolio(R = log_ret_xts_compl,
                                portfolio = portMinVar,
                                optimize_method = "random")

extractWeights(minVarOpt)

minVarReturns <- Return.portfolio(log_ret_xts_compl, 
                                  weight = extractWeights(minVarOpt), 
                                  rebalance_on = "months")

table.AnnualizedReturns(R = minVarReturns, Rf = 0.1/250)

set.seed(10260)
meanVarOpt <- optimize.portfolio(R = log_ret_xts_compl,
                                 portfolio = portMeanVar,
                                 optimize_method = "random",
                                 trace = TRUE, search_size = 500000)

extractWeights(meanVarOpt)


meanVarReturns <- Return.portfolio(log_ret_xts_compl, 
                                   weight = extractWeights(meanVarOpt), 
                                   rebalance_on = "months")
optimisedPortfolioReturns <- cbind(minVarReturns, meanVarReturns)
colnames(optimisedPortfolioReturns) <- c("Minimum Variance", 
                                         "Mean Variance")
table.AnnualizedReturns(R = optimisedPortfolioReturns, Rf = 0.1/252)

chart.RiskReward(meanVarOpt, risk.col = "StdDev", return.col = "mean",
                chart.assets = TRUE)

###########
eq_meanETL <- port
eq_meanETL <- add.objective(portfolio=eq_meanETL, type="return", name="mean")
eq_meanETL <- add.objective(portfolio=eq_meanETL, type="risk", name="ETL",
                            arguments=list(p=0.95))

eq_meanETL <- add.objective(portfolio=eq_meanETL, type="risk_budget",
                            name="ETL", max_prisk=0.4, 
                            arguments=list(p=0.95))

eq_meanETL <- add.objective(portfolio=eq_meanETL, type="risk_budget",
                            name="ETL", min_concentration=TRUE,
                            arguments=list(p=0.95))

opt_eq_meanETL <- optimize.portfolio(R=log_ret_xts_compl, 
                                     portfolio=eq_meanETL,
                                     optimize_method="DEoptim",
                                     search_size = 5000,
                                     trace = TRUE, traceDE=20)

opt_eq_meanETL <- optimize.portfolio(R=log_ret_xts_compl, 
                                     portfolio=eq_meanETL,
                                     optimize_method="random",
                                     trace=TRUE, search_size=2000)

wts <- as.tibble(t(extractWeights(opt_eq_meanETL)))
meanVarReturns <- Return.portfolio(log_ret_xts_compl, 
                                   weight = extractWeights(opt_eq_meanETL), 
                                   rebalance_on = "weeks")
table.AnnualizedReturns(R = meanVarReturns, Rf = 0.1/252)

chart.RiskReward(opt_eq_meanETL, risk.col = "ETL", return.col = "mean",
                 chart.assets = TRUE)


wts
p <- wts %>%
    gather(names(wts), 
           key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
    scale_y_continuous(labels = scales::percent) 
ggplotly(p)

plot(opt_eq_meanETL, risk.col="ETL", return.col="mean",
     main="mean-ETL Optimization", neighbors=25)
pct_contrib <- ES(R=log_ret_xts_compl, p=0.95, 
                  portfolio_method="component",
                  weights=extractWeights(opt_eq_meanETL))
barplot(pct_contrib$pct_contrib_MES, cex.names=0.8, las=3, 
        col="lightblue")
plot.new()
chart.RiskBudget(opt_eq_meanETL, risk.type="percentage", neighbors=25)
plot(opt_eq_meanETL, risk.col="ETL", return.col="mean",
     main="Risk Budget mean-ETL Optimization")



qu <- add.objective(portfolio=port, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25)
opt_qu <- optimize.portfolio(R=log_ret_xts_compl, portfolio=qu,
                             optimize_method="DEoptim",
                             trace=TRUE, traceDE=20)
plot(opt_qu, risk.col="StdDev", return.col="mean",
     main="Quadratic Utility Optimization", chart.assets=TRUE,
     xlim=c(0, 0.05), ylim=c(0, 0.0085))

opt_qu
