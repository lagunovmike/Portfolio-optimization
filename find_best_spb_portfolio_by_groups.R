## Setting workspace
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggrepel))
# font_import(pattern = "Arial")
# font_import(path = "C:/Windows/Fonts", pattern = "roboto")
# extrafont::loadfonts(device="win")
suppressPackageStartupMessages(library(tidyquant))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(timetk))
suppressPackageStartupMessages(library(PortfolioAnalytics))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(hrbrthemes))

Sys.setlocale("LC_ALL", "russian")
setwd("D:/Project/DataScience/Portfolio-optimization")

## Setting parameters

# spb_tickers <- read.csv("Data/ListingSecurityList.csv", sep = ";", quote = "")
spb_tickers <- read_csv2("Data/ListingSecurityList.csv", quote = "", lazy = FALSE)
spb_tickers <- spb_tickers %>%
    filter(s_sec_type_name_dop == "Акции",
           !s_face_value_currency %in% c("RUB", "KZT")) %>%
    select(s_RTS_code) %>%
    arrange(s_RTS_code) %>%
    pull()
spb_tickers <- gsub("@", ".", spb_tickers)
spb_tickers <- gsub(" ", "-", spb_tickers)

price_from <- "2011-11-01"
price_to <- "2021-12-28"

years_depth <- 3
price_filter <- 200
max_positions <- 10

## Data loading
price_data <- read_csv("Data/price_data.csv", lazy = FALSE)

# price_data <- tibble()
# for(i in 1:length(spb_tickers)){
#     price_data_temp <- tq_get(spb_tickers[i],
#                          from = price_from,
#                          to = price_to,
#                          get = 'stock.prices')
#     price_data <- bind_rows(price_data, price_data_temp)
#     cat("\r", "Total: ", length(spb_tickers), " | Done: " ,i, sep="")
# }


## Check the latest day of the base and update if necessary

if(ymd(price_to)-1 > max(price_data$date)) {
    start_timer <- now()
    cat("Updating the database \n" )
    
    price_data_new <- tibble()
    for(i in 1:length(spb_tickers)){
        price_data_new_temp <- tq_get(spb_tickers[i],
                                  from = price_from,
                                  to = price_to,
                                  get = 'stock.prices')
        price_data_new <- bind_rows(price_data_new, price_data_new_temp)
        cat("\r", "Total: ", length(spb_tickers), " | Done: " ,i, sep="")
    }
    
    finish_timer <- now() - start_timer
    finish_timer
    
    write_csv(price_data_new , "Data/price_data.csv", append = TRUE)
    cat("Added days:", as.numeric(today() - 1 -  max(price_data$date)))
    price_data <- price_data %>%
        bind_rows(price_data_new) %>%
        arrange(symbol, date)
} else {
    cat("Base is up to date")
}


# Decide if extra ticks are needed
tick <- c("AAPL", "AMD", "PLZL.ME", "SBER.ME", "QCOM",
          "NVTA", "POLY.ME", "MOEX.ME", "FRHC", "SONO")
include_ticks <- 1
if(include_ticks == 0){
  tick <- c()
}

# write_csv(price_data ,"Data/price_data.csv")

##############################################################################
### data processing ###
###############################################################################


# my brocker doesn't have
start_date_filter <- ymd(price_to) - years(years_depth)

not_available <- c("CCC", "RLMD","VAR1.DE", "CYRX",
                   "BJ", "NKLA", "ATSG", "RCII","VIRT", "UN01.DE")

price_data_selected_from <- price_data %>%
  filter(date >= start_date_filter)

price_data_avilable <- price_data_selected_from %>%
    filter(!symbol %in% not_available)

price_data_cheap_idx <- price_data_avilable %>%
    group_by(symbol) %>%
    mutate(last = last(close)) %>%
    summarize(lastdate = max(date), 
              lastprice = max(last), .groups = "drop") %>%
    filter(lastprice <= price_filter)

price_data_cheap <- price_data_selected_from %>%
    filter(symbol %in% price_data_cheap_idx$symbol)


log_ret_tidy <- price_data_cheap %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')

log_ret_xts <- log_ret_tidy %>%
    pivot_wider(names_from = symbol, values_from = ret) %>%
    tk_xts(silent = T)



many_na <- sapply(log_ret_xts, function(x){mean(is.na(x))})
many_na_idx <- which(many_na > 0.7)
log_ret_xts <- log_ret_xts[,-many_na_idx]

trade_begin <- as.Date(sapply(log_ret_xts, 
                        function(x){index(log_ret_xts)[min(which(!is.na(x)))]}))


log_ret_xts <- log_ret_xts[,which(trade_begin <= min(index(log_ret_xts)))]

ncol(log_ret_xts)


################################################################################
### calculate risk free based on S&P500 returns
################################################################################

sp500_prices <- tq_get("^GSPC",
                       from = start_date_filter,
                       to = price_to,
                       get = 'stock.prices')
sp500_logret <- sp500_prices %>%
    group_by(symbol) %>%
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period = 'daily',
        col_rename = 'ret',
        type = 'log'
    )

sp500_logret_xts <- sp500_logret %>%
    pivot_wider(names_from = symbol, values_from = ret) %>%
    tk_xts()
mean(sp500_logret_xts)
sp500_annual_ret <- as.numeric(Return.annualized(sp500_logret_xts))


################################################################################
### analysis ###
################################################################################


######
### Filter out bad stocks ###
######

wts_all <- tibble()
items_in_group <-70
groups_num <- ceiling(ncol(log_ret_xts)/items_in_group)
for(i in 1:groups_num){
    ind <- 1:items_in_group + (i-1)*items_in_group
    if(max(ind) > ncol(log_ret_xts)){
        ind <- seq((i-1)*items_in_group, ncol(log_ret_xts))
    }
    R <- log_ret_xts[,ind]
    R <- na.omit(R)
    funds <- colnames(R)
    
    # Construct initial portfolio
    init.portf <- portfolio.spec(assets=funds)
    init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
    init.portf <- add.constraint(portfolio=init.portf, type="long_only")
    init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
    init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
    
    maxSR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                       optimize_method="ROI", 
                                       maxSR=TRUE)
    
    wts_tmp <- as_tibble(t(extractWeights(maxSR.lo.ROI)))
    wts_tmp <- wts_tmp %>%
        gather(names(wts_tmp), 
               key = Asset,
               value = Weights) %>%
        filter(Weights > 0.1)
    wts_all <- bind_rows(wts_all, wts_tmp)
   
}

cat("first selection:", nrow(wts_all), "\n")
filter_rows_threshold <- 50
cat("cut to:", nrow(wts_all), "\n")

if(nrow(wts_all > filter_rows_threshold)){
  wts_all <- wts_all %>%
    top_n(filter_rows_threshold, wt = Weights)
}

p <- wts_all %>%
    top_n(10, wt = Weights) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(Weights,2)), nudge_y = 0.02, size = 3,
              color = "darkgray") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
p <- p + theme(legend.position = "none")
p



allticks <- unique(c(wts_all$Asset, tick))
cat("selection with ticks:", length(allticks), "\n")

price_data_best <- tq_get(allticks,
                     from = min(index(log_ret_xts)),
                     to = max(index(log_ret_xts)+1),
                     get = 'stock.prices')

# convert Russian assets to dollars

usdrub <- tq_get("RUB=X", from = price_from,
                 to = price_to,
                 get = 'stock.prices')
usdrub <- select(usdrub, date, adjusted)

price_data_best <- price_data_best %>%
    left_join(usdrub, by = "date", suffix = c(".stock", ".dollar")) %>%
    mutate(adjusted = if_else(grepl(".ME", price_data_best$symbol),
                              adjusted.stock / adjusted.dollar, 
                              adjusted.stock)) %>%
    select(-c(adjusted.dollar, adjusted.stock))

trade_begin_best <- price_data_best %>%
    group_by(symbol) %>%
    filter(date == min(date)) %>%
    summarize(symbol, date)

price_data_best <- price_data_best %>%
    filter(!symbol %in% pull(trade_begin_best[which(trade_begin_best$date > today() - years(years_depth)),1]))


# end converting

log_ret_tidy_best <- price_data_best %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')

log_ret_xts_best <- log_ret_tidy_best %>%
    pivot_wider(names_from = symbol, values_from = ret) %>%
    tk_xts()

log_ret_xts_best_compl <- na.omit(log_ret_xts_best) 


##########
### Portfolio optimization ###
##########

R_best <- log_ret_xts_best_compl
funds_best <- names(R_best)


init.portf <- portfolio.spec(assets=funds_best)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.constraint(portfolio=init.portf, type="position_limit", 
                             max_pos=max_positions)
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
# init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type='risk', name='ETL',
                            arguments=list(p=0.95))


maxSR.lo.ROI <- optimize.portfolio(R=R_best, portfolio=init.portf, 
                                   optimize_method="ROI", 
                                   maxSR=TRUE, trace = TRUE)
# maxSR.lo.ROI <- optimize.portfolio(R=R_best, portfolio=init.portf, 
#                                    optimize_method="glpk", maxSR=TRUE)


maxSRReturns <- Return.portfolio(R_best,weight = extractWeights(maxSR.lo.ROI), 
                                 rebalance_on = "days")

table.AnnualizedReturns(R = maxSRReturns, Rf = sp500_annual_ret/252)

#maxSR.lo.ROI


##########
### Plot optimized portfolio ###
##########

wts_best <- as_tibble(t(extractWeights(maxSR.lo.ROI)))
wts_best <- wts_best %>%
    gather(names(wts_best), 
           key = Asset,
           value = Weights) %>%
    filter(Weights > 0.001)

p <- wts_best %>%
    filter(Weights >= 0.01) %>%
    ggplot(aes(
        x = fct_reorder(Asset, Weights),
        y = Weights,
        fill = Asset
    )) +
    geom_bar(stat = 'identity', fill = "royalblue") +
    geom_text(
        aes(label = round(Weights, 2)),
        nudge_y = 0.03,
        size = 3.5,
        color = "gray30"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    labs(
        x = 'Assets',
        y = 'Weights',
        title = "Optimized Portfolio Weights",
        subtitle = paste(
            "From:",
            min(index(R_best)),
            "to",
            max(index(R_best)),
            "|| SR:",
            round(
                SharpeRatio.annualized(maxSRReturns,
                                       Rf = sp500_annual_ret /
                                           252)[1, 1], 2),
            "| Return:",
            round(Return.annualized(maxSRReturns)[1, 1], 2),
            "| Risk:",
            round(StdDev.annualized(maxSRReturns)[1, 1], 2)
        ),
        caption = paste(
            "Rf (SP500):",
            round(sp500_annual_ret, 2),
            "| Price filter ($):",
            price_filter
        )
    )
p <- p + theme_ipsum_rc(grid = "Y") + theme(legend.position = "none")
# p <- p + theme_minimal() + theme(legend.position = "none")
p


asset_favorite <- wts_best$Asset
optimal_portfolio_riskreward <- tibble("asset" = "Optimal portfolio",
                                       ret = maxSR.lo.ROI$objective_measures[[1]],
                                       es = maxSR.lo.ROI$objective_measures[[2]])

sp500_riskreward <- tibble(
    "asset" = "SP500",
    "ret" = mean(sp500_logret_xts),
    "es" = as.numeric(ES(sp500_logret_xts, invert = FALSE))
)

asset_riskreward <- tibble(
    "asset" = names(R_best),
    "ret" = scatterFUN(R_best, "mean"),
    "es" = scatterFUN(R_best, "es")
) %>%
    mutate(favorite = if_else(asset %in% asset_favorite, "favorite", "regular"))

p2 <- asset_riskreward %>%
    na.omit() %>%
    ggplot(aes(x = es, y  = ret)) +
    geom_point(aes(color = favorite)) +
    geom_point(data = sp500_riskreward,
               color = 'violetred3') +
    geom_text_repel(
        aes(label = asset),
        data = sp500_riskreward,
        color = "violetred3",
        size = 2.8
    ) +
    geom_text_repel(
        aes(label = asset, color = favorite),
        nudge_y = 0.00015,
        size = 2.8,
        max.overlaps = 25
    ) +
    geom_point(data = optimal_portfolio_riskreward, color = "violetred3") +
    geom_text_repel(
        data = optimal_portfolio_riskreward,
        aes(label = asset),
        color = "violetred3",
        nudge_y =  0.00015,
        size = 2.8
    ) +
    xlim(c(0.015, 0.125)) +
    ylim(c(0, 0.0045)) +
    labs(x = 'Expected shortfall', y = 'Mean return',
         title = "Assets risk rewars scatter",
         caption = strftime(now(), format = "Date: %F | Time: %R")) +
    scale_color_manual(values = c("royalblue", "gray25")) +
    theme_ipsum_rc() + theme(legend.position = "none")
p2

p <- p + theme(plot.margin = margin(t = 10, r = 10, b = 0, l = 10))
p2 <- p2 + theme(plot.margin = margin(t = 0, r = 10, b = 10, l = 10))
p3 <- plot_grid(p,p2, nrow = 2, rel_heights = c(0.4,0.6))
p3

ggsave(paste0("Reports/OPW&ARR_", min(index(R_best)),"_to_", 
              max(index(R_best)),"_SR",
              gsub("\\.", "", 
                   round(SharpeRatio.annualized(maxSRReturns,
                                  Rf = sp500_annual_ret/252)[1,1],2)), "_", 
              max_positions,"_pos" ,".png"), 
       bg = "white", scale = 1.6, dpi = 150, plot = p3)

