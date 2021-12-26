sp500_prices <- tq_get("^GSPC",
                     from = price_from,
                     to = price_to,
                     get = 'stock.prices')
sp500_logret <- sp500_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

sp500_logret_xts <- sp500_logret %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  tk_xts()

sp500_annual_ret <- Return.annualized(sp500_logret_xts)

Return.calculate(sp500_logret_xts)

table.AnnualizedReturns(sp500_logret_xts, Rf = (0.05/252))[1,]


plot(maxSR.lo.ROI, chart.assets=TRUE, xlim=c(0.02, 0.15), ylim = c(0.002, 0.003))
par(mfrow=c(1,1)) 
chart.RiskReward(maxSR.lo.ROI,return.col="mean", xlim=c(0, 0.2), 
                 risk.col="ETL",
                 chart.assets=TRUE, main="Maximum Return")

bt_maxret <- optimize.portfolio.rebalancing(R=R_best, portfolio=init.portf,
                                            optimize_method="ROI",
                                            maxSR=TRUE,
                                            rebalance_on="quarters",
                                            training_period=12)
extractWeights(bt_maxret$opt_rebalancing[[1]])

period <- 12
wts_backtrack <- as_tibble(t(extractWeights(bt_maxret$opt_rebalancing[[period]])))
wts_backtrack <- wts_backtrack %>%
  gather(names(wts_backtrack), 
         key = Asset,
         value = Weights) %>%
  filter(Weights > 0.001)

p <- wts_backtrack %>%
  filter(Weights >= 0.01) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(Weights,2)), nudge_y = 0.02, size = 3.5,
            color = "darkgray") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Assets', y = 'Weights', title = "Optimized Portfolio Weights",
       subtitle = paste("Date:", names(bt_maxret$opt_rebalancing[period]),
                        "| SR:", round(SharpeRatio.annualized(maxSRReturns, 
                                                              Rf = sp500_annual_ret/252)[1,1],2),
                        "| Return:", round(Return.annualized(maxSRReturns)[1,1],2),
                        "| Risk: ", round(StdDev.annualized(maxSRReturns)[1,1],2),
                        "| Rf (SP500):", round(sp500_annual_ret,2),
                        "| Price filter:", price_filter))
p <- p + theme(legend.position = "none", plot.subtitle=element_text(size=8))
p

start_date_filter <- today() - years(3)
log_ret_xts_date <- log_ret_xts[paste0(start_date_filter, "/")]


asset_favorite <- wts_best$Asset
optimal_portfolio_riskreward <- tibble("asset" = "Optimal portfolio",
                                       ret = maxSR.lo.ROI$objective_measures[[1]],
                                       es = maxSR.lo.ROI$objective_measures[[2]])
  
asset_riskreward <- tibble("asset" = names(R_best), 
       "ret" = scatterFUN(R_best, "mean"), 
       "es" = scatterFUN(R_best, "es")) %>%
  mutate(favorite = if_else(asset %in% asset_favorite, "favorite", "regular"))

p2 <- asset_riskreward %>%
  ggplot(aes(x = es, y  = ret)) +
  geom_point(aes(color = favorite)) +
  geom_text_repel(aes(label = asset, color = favorite), 
                  nudge_y = 0.00015, size = 3.5) +
  geom_point(data = optimal_portfolio_riskreward, color = "violetred3") +
  geom_text_repel(data = optimal_portfolio_riskreward, aes(label = asset), 
                  color ="violetred3", nudge_y =  0.00015) +
  labs(x = 'Expected shortfall', y = 'Mean return', 
       title = "Assets risk rewars scatter") +
  scale_color_manual(values=c("royalblue", "gray25")) +
  theme_ipsum_rc() + theme(legend.position = "none")
p2

library(gridExtra)
library(cowplot)
p <- p + theme(plot.margin = margin(t = 10, r = 10, b = 0, l = 10))
p2 <- p2 + theme(plot.margin = margin(t = 0, r = 10, b = 10, l = 10))
grid.arrange(p, p2, nrow=2, rel_)
plot_grid(p,p2, nrow = 2, rel_heights = c(0.4,0.6))
