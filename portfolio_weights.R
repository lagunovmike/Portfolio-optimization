wts_have <- c("BMCH" = 0.13,
              "MOEX.ME" = 0.05,
              "ZYXI" = 0.04,
              "SNAP" = 0.12,
              "CPB" = 0.13,
              "POLY.ME" = 0.17,
              "FRHC" = 0.37)
tick <- names(wts_have)

price_from <- "2016-11-25"
price_to <- "2020-11-25"


wts_have <- c("MOEX.ME" = 0.0489,
              "ACMR" = 0.1940,
              "AMD" = 0.2225,
              "QCOM" = 0.3817,
              "NVTA" = 0.1300)
sum(wts_have)
tick <- names(wts_have)
min(index(log_ret_xts_compl))
price_from <- min(index(log_ret_xts_compl))

wts_have <- c("BMCH" = 0.13,
              "NVTA" = 0.1350,
              "CPB" = 0.13,
              "POLY.ME" = 0.11,
              "FRHC" = 0.28,
              "AMD" = 0.2225)
tick <- names(wts_have)


wts_have <- c("FRHC" = 0.4624,
              "CPB" = 0.1317,
              "BMCH" = 0.1297,
              "SNAP" = 0.1181,
              "BTAI" = 0.1182,
              "POLY.ME")
tick <- names(wts_have)
sum(wts_have)
price_from <- "2015-11-25"
price_to <- "2020-11-25"
min(index(log_ret_xts_compl))


## Interesting

wts_have <- c("FRHC" = 0.3699,
              "POLY.ME" = 0.2221,
              "NEO" = 0.2406,
              "REGI" = 0.155)

tick <- names(wts_have)



wts_have_plot <- as_tibble(t(wts_have))
p <- wts_have_plot %>%
    gather(names(wts_have_plot), 
           key = Asset,
           value = Weights) %>%
    filter(Weights > 0.001) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(Weights,2)), nudge_y = 0.02, size = 4,
              color = "gray30") +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))+
    scale_y_continuous(labels = scales::percent) +
    labs(x = 'Assets', y = 'Weights', title = "Optimized Portfolio Weights",
         subtitle = paste("From:", min(index(R)), "to", max(index(R)),
                          "| SR:", round(SharpeRatio.annualized(maxSRReturns, 
                                                                Rf = 0.05/252)[1,1],2),
                          "| Return:", round(Return.annualized(maxSRReturns)[1,1],2),
                          "| Risk: ", round(StdDev.annualized(maxSRReturns)[1,1],2)))
p
