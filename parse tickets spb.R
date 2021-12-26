library("RSelenium")
library("rvest")

rD <- rsDriver(port = netstat::free_port(), 
               browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$navigate("https://spbexchange.ru/ru/listing/securities/list/")
option <- remDr$findElement(using = 'xpath', '//*[@id="ctl00_BXContent_list_ddlCBView"]/option[2]')
option$clickElement()

tickets <- c()

for (i in 1:165){
    Sys.sleep(2)
    cat("recording", "\n")
    getUrl <- read_html(remDr$getPageSource()[[1]])
    
    temp_table <- getUrl %>%
        html_node(xpath = '//*[@id="ctl00_BXContent_list_up"]/div[2]/table') %>%
        html_table()
    tmp_ticker <- temp_table$`Идентификационный код ценной бумаги`
    tickets <- c(tickets, tmp_ticker)
    cat("next", "\n", "\n")
    
}
tickets
length(tickets)
tail(tickets)
tickets
length(unique(tickets))
tickets <- unique(tickets)
unique(tickets)

write.csv(tickets, "tickets-spb.csv")
