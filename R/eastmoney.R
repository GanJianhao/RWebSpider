library(magrittr)
library(rvest)

web_spider.eastmoney <- function(website, num = 80) {

    page <- read_html(website)

    css.selector <-
        ".articleh .l1, .articleh .l2, .articleh .l3, .articleh .l5, .articleh .l6"
    html.contents <- html_nodes(page, css.selector)
    html.text <- html_text(html.contents, trim = TRUE)

    ## http://guba.eastmoney.com/list,000002,99.html
    ID <- substr(website, 32, 37) %>% rep(num)
    Main <- rep(website, num)

    ## Index to select useful information
    index <- seq(1, num * 5, 5)
    Sub <- html.contents[index + 2] %>%
        html_nodes(xpath = "./a") %>%
        html_attr("href")
    Title <- html.contents[index + 2] %>%
        html_nodes(xpath = "./a") %>%
        html_attr("title")
    Readings <- as.numeric(html.text[index])
    Reviews <- as.numeric(html.text[index + 1])
    PubDate <- paste0("2016-", html.text[index + 3])
    LastTime <- paste0("2016-", html.text[index + 4])

    df <- data.frame(ID, Main, Sub, PubDate, LastTime, Title, Readings,
                     Reviews)

    return(df)
}
