library(dplyr)
library(rvest)
library(httr)
#104前端工程師-2。篩選條件：六都+新竹、無經驗（畢業生）
#地點是D，薪水是E，網址是F

#page1
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=1&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_one <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_one <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_one <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page2
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=2&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_two <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_two <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_two <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page3
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=3&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_thr <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_thr <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_thr <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")


#page4
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=4&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_fou <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_fou <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_fou <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page5
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=5&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_fiv <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_fiv <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_fiv <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page6
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=6&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_six <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_six <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_six <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")


#page7
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=7&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_sev <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_sev <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_sev <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")



#page8
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001002000%2C6001005000%2C6001008000%2C6001016000%2C6001014000%2C6001006000%2C6001001000&order=14&asc=0&sr=99&page=8&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
D_eig <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
E_eig <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
F_eig <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#合併各頁資料，共160筆
local <- c(D_one,D_two,D_thr,D_fou,D_fiv,D_six,D_sev,D_eig)
salary <- c(E_one,E_two,E_thr,E_fou,E_fiv,E_six,E_sev,E_eig)
#把網址c前面加上“https:”
url <- c(F_one,F_two,F_thr,F_fou,F_fiv,F_six,F_sev,F_eig)
url <- paste0("https:",url)

1
#用助教碼把160個網址的html變成repo_list
remotes::install_github('rlesur/crrri', force = T)
library(crrri)
library(rvest)
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

#從個網頁中抓出所有條件，再從中抓出科系要求、其他條件和擅長工具
html <- vector("list",length = 160)
tools <- vector("character",length(local))
for(i in c(1:160)){
    html[[i]] <- dump_DOM(url[i]) %>% read_html()
    tools[i] <- html[[i]] %>% 
        html_nodes("#app") %>%
        html_nodes("div:nth-child(6) > div.col.p-0.job-requirement-table__data > p.mb-0.r3") %>%
        html_text()
}
## 所有條件
try <- vector("list",length = 160)
for(i in c(1:160)){
    try[[i]] <- html[[i]] %>%
        html_nodes("#app") %>%
        html_nodes("div > div.col.p-0.job-requirement-table__data") %>%
        html_text()
}

department <- vector(length = 160)
for(i in 1:160){
    department[i] <- try[[i]][4]
}

else_need <- vector(length = 160)
for(i in 1:160){
    else_need[i] <- try[[i]][8]
}

#變成dataframe，成果就是product
product <- data.frame(local=local, salary=salary, department=department, tools=tools, else_need=else_need, url=url)
#輸出成csv檔
write.csv( product, file="../../data/front.csv", row.names = F)



