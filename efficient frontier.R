frontier <- extractEfficientFrontier(maxSR.lo.ROI)
frontier_create <- create.EfficientFrontier(R_best,
                                            init.portf,
                                            type = "mean-var",
                                            match.col = "",
                                            n = 25)

chart.EfficientFrontier(frontier_create)
chart.EF.Weights(frontier_create, match.col = "ES")
print.efficient.frontier(frontier_create)

summary(frontier_create)

frontier_create$frontier


frontier_summary <- summary(frontier)
frontier_metrics <- as.data.frame(frontier_summary$metrics)


frontier_summary <- summary(frontier_create)
frontier_metrics <- as.data.frame(frontier_summary$metrics)

ggplot(aes(x = ETL, y = mean), data = frontier_metrics) +
    geom_point() +
    geom_line()


asset_means <- colMeans(R_best)
asset_sd <- apply(R_best, MARGIN = 2, FUN = "sd")
asset_riskreward$sd <- asset_sd
optimal_portfolio_riskreward <- tibble("asset" = "Optimal portfolio",
                                       sd = maxSR.lo.ROI$objective_measures[[1]],
                                       ret = maxSR.lo.ROI$objective_measures[[2]])

ggplot(aes(x = sd, y = ret), data = asset_riskreward) +
    geom_point() +
    geom_point(aes(x = StdDev, y = mean), data = frontier_metrics)
    



p2 <- asset_riskreward %>%
    na.omit() %>%
    ggplot(aes(x = sd, y  = ret)) +
    geom_point(aes(color = favorite)) +
    geom_text_repel(
        aes(label = asset, color = favorite),
        nudge_y = 0.00015,
        size = 2.8,
        max.overlaps = 25
    ) +
    geom_point(data = optimal_portfolio_riskreward, color = "violetred3") +
    geom_text(
        data = optimal_portfolio_riskreward,
        aes(label = asset),
        color = "violetred3",
        nudge_y =  0.00015,
        size = 2.8
    ) +
    geom_point(aes(x = StdDev, y = mean), data = frontier_metrics, color = "green") +
    geom_line(aes(x = StdDev, y = mean), data = frontier_metrics, color = "green") +
    labs(x = 'Expected shortfall', y = 'Mean return',
         title = "Assets risk rewars scatter",
         caption = strftime(now(), format = "Date: %F | Time: %R")) +
    scale_color_manual(values = c("royalblue", "gray25")) +
    theme_ipsum_rc() + theme(legend.position = "none")
p2
