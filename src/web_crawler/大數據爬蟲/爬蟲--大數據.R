#R專題爬蟲
library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(crrri)

#大數據分析師
## page1
resp_1 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%A4%A7%E6%95%B8%E6%93%9A%E5%88%86%E6%9E%90%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=1&jobexp=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_1$status_code
html_1 <- content(resp_1)

#page1網址
website_1 <- html_1 %>% 
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
    html_attr("href") %>%
    as.vector()

for (i in c(1:20)){
    website_1[i] <- paste0("https:",website_1[i])
}

#page1地點
place_1 <- html_1%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()

#page1薪水
wage_1 <- html_1%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()



## page2
resp_2 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%A4%A7%E6%95%B8%E6%93%9A%E5%88%86%E6%9E%90%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=2&jobexp=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_2$status_code
html_2 <- content(resp_2)

#page2網址
website_2 <- html_2 %>% 
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
    html_attr("href") %>%
    as.vector()

for (i in seq_along(website_2)){
    website_2[i] <- paste0("https:",website_2[i])
}

#page2地點
place_2 <- html_2%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()

#page2薪水
wage_2 <- html_2%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()


## page3
resp_3 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%A4%A7%E6%95%B8%E6%93%9A%E5%88%86%E6%9E%90%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=3&jobexp=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_3$status_code
html_3 <- content(resp_3)

#page3網址
website_3 <- html_3 %>% 
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
    html_attr("href") %>%
    as.vector()

for (i in c(1:20)){
    website_3[i] <- paste0("https:",website_3[i])
}

#page3地點
place_3 <- html_1%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()

#page3薪水
wage_3 <- html_3%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()

## page4
resp_4 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%A4%A7%E6%95%B8%E6%93%9A%E5%88%86%E6%9E%90%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=4&jobexp=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_4$status_code
html_4 <- content(resp_4)

#page4網址
website_4 <- html_4 %>% 
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
    html_attr("href") %>%
    as.vector()

for (i in c(1:20)){
    website_4[i] <- paste0("https:",website_4[i])
}

#page4地點
place_4 <- html_4%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()

#page4薪水
wage_4 <- html_4%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()

## page5
resp_5 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%A4%A7%E6%95%B8%E6%93%9A%E5%88%86%E6%9E%90%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=5&jobexp=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_5$status_code
html_5 <- content(resp_5)

#page5網址
website_5 <- html_5 %>% 
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
    html_attr("href") %>%
    as.vector()

for (i in c(1:9)){
    website_5[i] <- paste0("https:",website_5[i])
}

#page5地點
place_5 <- html_5%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()

#page5薪水
wage_5 <- html_5%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()



df_big <- data.frame(
    local = c(place_1,place_2,place_3,place_4,place_5),
    salary = c(wage_1,wage_2,wage_3,wage_4,wage_5),
    url = c(website_1,website_2,website_3,website_4,website_5)
)



###function

dump_DOM <- function(url, file = "") {
    perform_with_chrome(function(client) {
        Network <- client$Network
        Page <- client$Page
        Runtime <- client$Runtime
        Network$enable() %...>% { 
            Page$enable()
        } %...>% {
            Network$setCacheDisabled(cacheDisabled = TRUE)
        } %...>% {
            Page$navigate(url = url)
        } %...>% {
            Page$loadEventFired()
        } %...>% {
            Runtime$evaluate(
                expression = 'document.documentElement.outerHTML'
            )
        } %...>% (function(result) {
            html <- result$result$value
            return(html)
        }) 
    })
}
###function

##後端個別網頁
#擅長工具
html <- vector("list",length = 89)
df_big$tools <- vector("character",length = 89)
for(i in c(1:89)){
    html[[i]] <- dump_DOM(df_big$url[i]) %>% read_html()
    df_big$tools[i] <- html[[i]] %>% 
        html_nodes("#app") %>%
        html_nodes("div:nth-child(6) > div.col.p-0.job-requirement-table__data > p.mb-0.r3") %>%
        html_text() 
}



## 所有條件
select <- vector("list",89)
for(i in c(1:89)){
    select[[i]] <- html[[i]] %>%
        html_nodes("#app") %>%
        html_nodes("div > div.col.p-0.job-requirement-table__data") %>%
        html_text()
}
#科系要求 #104的資料有變動，原本有89筆
df_big$department <- vector(length = 89)
for(i in 1:89){
    df_big$department[i] <- select[[i]][4]
}
#擅長工具
df_big$tools <- vector(length = 89)
for(i in 1:89){
    df_big$tools[i] <- select[[i]][6]
}
#其他條件
df_big$else_need <- vector(length = 89)
for(i in 1:89){
    df_big$else_need[i] <- select[[i]][8]
}

df_big <- data.frame(
    local = df_big$local,
    salary = df_big$salary,
    department = df_big$department,
    tools = df_big$tools,
    else_need = df_big$else_need,
    url = df_big$url
)
#資料匯出
write.csv(df_big,file="../../data/bigdata.csv",row.names = F)
