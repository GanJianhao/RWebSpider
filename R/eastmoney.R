library(magrittr)
library(rvest)

web_spider.eastmoney <- function(website, num = 80) {

    page <- read_html(website)

    css.selector <-
        ".articleh .l1, .articleh .l2, .articleh .l3, .articleh .l5, .articleh .l6"
    html.contents <- html_nodes(page, css.selector)
    html.text <- html_text(html.contents, trim = TRUE)

    ## http://guba.eastmoney.com/list,000002,99.html
    StockID <- substr(website, 32, 37) %>% rep(num)

    ## Index to select useful information
    index <- seq(1, num * 5, 5)
    PostID <- html.contents[index + 2] %>%
        html_nodes(xpath = "./a") %>%
        html_attr("href") %>%
        substr(13, 21) %>%
        as.numeric()
    Title <- html.contents[index + 2] %>%
        html_nodes(xpath = "./a") %>%
        html_attr("title")
    ReadingNum <- as.numeric(html.text[index])
    CommentNum <- as.numeric(html.text[index + 1])
    PostDate <- paste0("2016-", html.text[index + 3])
    LastCommentTime <- paste0("2016-", html.text[index + 4])

    df <- data.frame(PostID, LastCommentTime, PostDate, StockID, ReadingNum, CommentNum)# Title)

    return(df)
}
