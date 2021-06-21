library(dplyr)
library(rvest)
library(httr)

#104前端工程師。篩選條件：六都、無經驗（畢業生）
#地點是a，薪水是b，網址是c

#page1
resp <- GET("https://www.104.com.tw/jobs/search/?ro=1&kwop=7&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000&order=12&asc=0&page=1&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
a <- html%>% 
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
b <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
c <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page2
resp <- GET("https://www.104.com.tw/jobs/search/?ro=1&kwop=7&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000&order=12&asc=0&page=2&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
ai <- html %>%  
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
bi <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
ci <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page3
resp <- GET("https://www.104.com.tw/jobs/search/?ro=1&kwop=7&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000&order=12&asc=0&page=3&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
aii <- html %>%  
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
bii <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
cii <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#page4
resp <- GET("https://www.104.com.tw/jobs/search/?ro=1&kwop=7&keyword=前端工程師&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&area=6001001000%2C6001002000%2C6001005000%2C6001008000%2C6001014000%2C6001016000&order=12&asc=0&page=4&jobexp=1&mode=s&jobsource=2018indexpoc")
resp$status_code
html <- content(resp)
aiii <- html %>%  #page4
    html_nodes("#js-job-content > article > div.b-block__left > ul.b-list-inline.b-clearfix.job-list-intro.b-content > li:nth-child(1)")%>%
    html_text()
biii <- html%>%   #page4
    html_nodes("#js-job-content > article > div.b-block__left > div > span:nth-child(1)")%>%
    html_text()
ciii <- html%>%   
    html_nodes("#js-job-content > article > div.b-block__left > h2 > a")%>%
    html_attr("href")

#合併各頁資料，共80筆
local <- c(a, ai, aii, aiii)
salary <- c(b, bi, bii, biii)
  #把網址c前面加上“https:”
url <- c(c,ci,cii,ciii)
url <- paste0("https:",url)

#用助教碼80個網址的html變成resp_list
remotes::install_github('rlesur/crrri')
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

resp_list <- vector("list", length(url))  #空向量
for(i in seq_along(url)){
    resp_list[[i]] <- dump_DOM(url[i]) %>% read_html()
}

#取出每個小網頁的科系要求與其他條件做成向量，科系要求是department，擅長工具是tool，其他條件是else_need
department <- vector("character",length(local))   #空向量
for(i in 1:80){
    if(any(i%in%c(6,11,14,21,23,34,36,38,41,47,50,55,57,61,64,66,70,71))){
        next
    }
    department[[i]] <- resp_list[[i]] %>%
        html_nodes("#app > div.container.jb-container.pt-4.position-relative > div > div.col.main > div.dialog.container-fluid.bg-white.rounded.job-requirement.mb-4.pt-6.pb-6 > div.job-requirement-table.row > div:nth-child(4) > div.col.p-0.job-requirement-table__data > p")%>%
        html_text() 
}
for(i in c(6,21,23,34,36,38,47,50,55,61,64,70,71)){
    department[[i]] <- "不拘"
}
for(i in c(11,14,41,57,66)){
    department[[i]] <- "電機電子工程相關、數學相關、資訊管理相關、資訊工程相關"
}

elsw_need <- vector("character",length(local))   #空向量
for(i in 1:80){
    if(any(i%in%c(6,11,14,21,23,34,36,38,41,47,50,55,57,61,64,66,70,71))){
        elsw_need[[i]] <- "未填寫"
        next
    }
    elsw_need[[i]] <- resp_list[[i]] %>%
        html_nodes("#app > div.container.jb-container.pt-4.position-relative > div > div.col.main > div.dialog.container-fluid.bg-white.rounded.job-requirement.mb-4.pt-6.pb-6 > div.job-requirement.col.opened > div > div.col.p-0.job-requirement-table__data > p")%>%
        html_text() 
}

try <- vector("list",length = 80)  #空list
for(i in c(1:80)){
    try[[i]] <- resp_list[[i]] %>%
        html_nodes("#app") %>%
        html_nodes("div > div.col.p-0.job-requirement-table__data") %>%
        html_text()
}

tool <- vector("character",length(local))  #空向量
for(i in 1:80){
    tool[i] <- try[[i]][6]
}

#NULL:c(6,11,14,21,23,34,36,38,41,47,50,55,57,61,64,66,70,71)
#有東西:c(16,19,20,25,27,35,45,49,51,67,77,79,80)
#其他條件路徑：#app > div.container.jb-container.pt-4.position-relative > div > div.col.main > div.dialog.container-fluid.bg-white.rounded.job-requirement.mb-4.pt-6.pb-6 > div.job-requirement.col.opened > div > div.col.p-0.job-requirement-table__data > p

#變成dataframe，成果就是product
product <- data.frame(local=local, salary=salary, department=department, tool=tool, elsw_need=elsw_need, url=url)
View(product)


library(DT)
datatable(iris)
test <- readr::read_csv("product.csv")
View(test)