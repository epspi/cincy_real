require(rvest)


dat <- html("http://www.wcsooh.org/SheriffSales/slsgrid.aspx") %>%
    html_nodes("table") %>%
    html_table() %>%
    data.frame
