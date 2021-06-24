#R專題爬蟲
library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(crrri)

#後端工程師
## page1
resp_1 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=1&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
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
website_1 <- website_1[1:20]
#page1地點
place_1 <- html_1%>%
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
place_1 <- place_1[1:20]
#page1薪水
wage_1 <- html_1%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
wage_1 <- wage_1[1:20]


## page2
resp_2 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=2&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
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
resp_3 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=3&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
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
place_3 <- place_3[1:20]
#page3薪水
wage_3 <- html_3%>%
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()

## page4
resp_4 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=4&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
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
resp_5 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=5&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_5$status_code
html_5 <- content(resp_5)

#page5網址
website_5 <- html_5 %>% 
  html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
  html_attr("href") %>%
  as.vector()

for (i in c(1:20)){
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

## page6
resp_6 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=6&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_6$status_code
html_6 <- content(resp_6)

#page6網址
website_6 <- html_6 %>% 
  html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
  html_attr("href") %>%
  as.vector()

for (i in c(1:20)){
  website_6[i] <- paste0("https:",website_6[i])
}

#page6地點
place_6 <- html_6%>%
  html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
  html_text()

#page6薪水
wage_6 <- html_6%>%
  html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
  html_text()


## page7
resp_7 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=7&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_7$status_code
html_7 <- content(resp_7)

#page7網址
website_7 <- html_7 %>% 
  html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
  html_attr("href") %>%
  as.vector()

for (i in c(1:20)){
  website_7[i] <- paste0("https:",website_7[i])
}

#page7地點
place_7 <- html_7%>%
  html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
  html_text()

#page7薪水
wage_7 <- html_7%>%
  html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
  html_text()

## page8
resp_8 <- GET("https://www.104.com.tw/jobs/search/?ro=1&keyword=%E5%BE%8C%E7%AB%AF%E5%B7%A5%E7%A8%8B%E5%B8%AB&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000%2C6001006000&order=14&asc=0&sr=99&page=8&mode=s&jobsource=2018indexpoc", encoding = "UTF-8")
resp_8$status_code
html_8 <- content(resp_8)

#page8網址
website_8 <- html_8 %>% 
  html_nodes("#js-job-content > article > div.b-block__left > h2 > a") %>%
  html_attr("href") %>%
  as.vector()

for (i in c(1:20)){
  website_8[i] <- paste0("https:",website_8[i])
}

#page8地點
place_8 <- html_8%>%
  html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
  html_text()

#page8薪水
wage_8 <- html_8%>%
  html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
  html_text()




df_back <- data.frame(
    local = c(place_1,place_2,place_3,place_4,place_5,place_6,place_7,place_8),
    salary = c(wage_1,wage_2,wage_3,wage_4,wage_5,wage_6,wage_7,wage_8),
    url = c(website_1,website_2,website_3,website_4,website_5,website_6,website_7,website_8)
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
html <- vector("list",length = 160)
df_back$tools <- vector("character",length = 160)
for(i in c(1:160)){
  html[[i]] <- dump_DOM(df_back$url[i]) %>% read_html()
  df_back$tools[i] <- html[[i]] %>% 
    html_nodes("#app") %>%
    html_nodes("div:nth-child(6) > div.col.p-0.job-requirement-table__data > p.mb-0.r3") %>%
    html_text()
}




## 所有條件
select <- vector("list",160)
for(i in c(1:160)){
  select[[i]] <- html[[i]] %>%
    html_nodes("#app") %>%
    html_nodes("div > div.col.p-0.job-requirement-table__data") %>%
    html_text()
}
#科系要求
df_back$department <- vector(length = 160)
for(i in 1:160){
  df_back$department[i] <- select[[i]][4]
}
#擅長工具
df_back$tools <- vector(length = 160)
for(i in 1:160){
  df_back$tools[i] <- select[[i]][6]
}
#其他條件
df_back$else_need <- vector(length = 160)
for(i in 1:160){
  df_back$else_need[i] <- select[[i]][8]
}


df_back <- data.frame(
  local = df_back$local,
  salary = df_back$salary,
  department = df_back$department,
  tools = df_back$tools,
  else_need = df_back$else_need,
  url = df_back$url
)
#資料匯出
write.csv(df_back,file="../../data/back.csv",row.names = F)
