library(magrittr)
library(rvest)
library(RMySQL)

web_spider.eastmoney <- function(file_path, max_page_num = 100) {
    
    webs <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE) %>% unlist() %>% 
        as.character()
    
    mydb <- dbConnect(MySQL(), user = "root", password = "ben2025530", dbname = "eastmoney", 
        host = "127.0.0.1")
    
    for (website in webs) {
        for (i in 1:max_page_num) {
            if (i > 1) {
                page.url <- substr(website, 1, 42) %>% paste0("_", i, ".html")
            } else {
                page.url <- website
            }
            tryCatch(html.page <- read_html(page.url),
                     error = function(mes) {
                         id <- substr(website, 32, 37)
                         if (mes$message == "HTTP error 404.") {
                             if (i > 1) {
                                 message("Downloads of stock ", id, " have been finished.")
                             }
                             else {
                                 message("Cann't find the page of stock ", id)
                             }
                             break
                         }
                         else {
                             mes$message <- paste0(mes$message, " (Stock ", id, " Page ", i, " )")
                             stop(mes)   
                         }
                     })
            
            css.selector <- ".articleh .l1, .articleh .l2, .articleh .l3, .articleh .l5, .articleh .l6"
            html.contents <- html_nodes(html.page, css.selector)
            items <- length(html.contents) / 5
            html.text <- html_text(html.contents, trim = TRUE)
            
            ## http://guba.eastmoney.com/list,000002,99.html
            StockID <- substr(website, 32, 37) %>% rep(items)
            
            ## Index to select useful information
            index <- seq(1, items * 5, 5)
            PostID <- html.contents[index + 2] %>% html_nodes(xpath = "./a") %>%
                html_attr("href") %>% substr(14, 22)
            PostID <- substr(website, 32, 37) %>% paste0(PostID)
            ReadingNum <- as.numeric(html.text[index])
            CommentNum <- as.numeric(html.text[index + 1])
            PostDate <- html.text[index + 3]
            PostDate[PostDate > "07-20"] <- paste0("2015-", PostDate[PostDate > "07-20"])
            PostDate[PostDate <= "07-20"] <- paste0("2016-", PostDate[PostDate <= "07-20"])
            LastCommentTime <- paste0("2016-", html.text[index + 4])
            
            df <- data.frame(PostID, LastCommentTime, PostDate, StockID, ReadingNum, 
                CommentNum)
            
            ## Import df to MySQL server
            dbtypes <- list(PostID = "bigint(15)", LastCommentTime = "datetime", PostDate = "date",
                            StockID= "int(6)", ReadingNum = "int(10)", CommentNum = "int(10)")
            dbWriteTable(mydb, name = "sz_a_stocks", df, field.types = dbtypes, 
                         row.names = FALSE, append = TRUE)
        }
    }
    
    dbDisconnect(mydb) %>% on.exit()
}
