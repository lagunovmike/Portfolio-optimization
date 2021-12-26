lastprices <- price_data_best %>%
    group_by(symbol) %>%
    filter(symbol %in% wts_best$Asset,
           date == max(date)) %>%
    select(symbol, close) %>%
    arrange(symbol)
lastprices

wts_best
