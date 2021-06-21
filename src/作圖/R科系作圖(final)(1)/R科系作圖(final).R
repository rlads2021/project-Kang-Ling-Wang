library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

#前端工程師

##資料讀取與清理
product <- read_csv("前端.csv") %>%
    filter(salary != "待遇面議")
list_dep_product <- strsplit(product$department, "、")
vec_dep_product <- unlist(list_dep_product)
vec_dep_product <- gsub("\\s?資訊工程相關\\s?", "資訊工程相關", vec_dep_product)
vec_dep_product <- gsub("\\s?資訊管理相關\\s?", "資訊管理相關", vec_dep_product)
vec_dep_product <- gsub("\\s?電機電子工程相關\\s?", "電機電子工程相關", vec_dep_product) 
vec_dep_product[vec_dep_product == "應用數學相關" |
                    vec_dep_product == "數學及電算機科學學科類"] <- "其他數學及電算機科學相關"
vec_dep_product[vec_dep_product == "自然科學學科類" |
                    vec_dep_product == "藝術學科類" |
                    vec_dep_product == "數理統計相關" |
                    vec_dep_product == "商業及管理學科類"|
                    vec_dep_product == "其他工程相關" |
                    vec_dep_product == "工業管理相關" ] <- "其他相關科系"
vec_dep_product[vec_dep_product == " 不拘 "] <- "不拘"

productdt <- data.table(product)
product_m <- productdt[grepl('月薪',salary)] #將年薪資料去除

##建立提及次數dataframe
df_dep_product <- data.frame(table(vec_dep_product))
df_dep_product <- df_dep_product %>%
    arrange(desc(Freq)) %>%
    mutate(perc = round(Freq/sum(Freq)*100)) %>%
    rename(科系 = vec_dep_product)

##作長條圖
product_bar <- ggplot(df_dep_product) +
    geom_bar(aes(x = perc , y = reorder(科系, -perc)), stat = "identity") +
    labs(x = "百分比(%)", y = "科系", title = "前端工程師科系要求") +
    theme(text=element_text(family="STHeiti", size=14))

ggsave(product_bar, file="product_bar.png", width=30, height=20, units = "cm")

#後端工程師

##資料讀取與清理
back <- read_csv("back.csv") %>%
    filter(salary != "待遇面議")
list_dep_back <- strsplit(back$department, "、")
vec_dep_back <- unlist(list_dep_back)
vec_dep_back <- gsub("\\s?資訊工程相關\\s?", "資訊工程相關", vec_dep_back)
vec_dep_back <- gsub("\\s?資訊管理相關\\s?", "資訊管理相關", vec_dep_back)
vec_dep_back[vec_dep_back == "其他數學及電算機科學相關 " |
            vec_dep_back == "應用數學相關" |
            vec_dep_back == "數學及電算機科學學科類" |
            vec_dep_back == "數學及電算機科學相關"] <- "其他數學及電算機科學相關"
vec_dep_back[vec_dep_back == "運輸通信學科類"] <- "通信學類"
vec_dep_back[vec_dep_back == "工業工程相關"] <- "工程學科類" 
vec_dep_back[vec_dep_back == " 不拘 "] <- "不拘"


backdt <- data.table(back)
back_m <- backdt[grepl('月薪',salary)] #將年薪資料去除

##建立提及次數dataframe
df_dep_back <- data.frame(table(vec_dep_back))
df_dep_back <- df_dep_back %>%
    arrange(desc(Freq)) %>%
    mutate(perc = round(Freq/sum(Freq)*100)) %>%
    rename(科系 = vec_dep_back)

##作長條圖
back_bar <- ggplot(df_dep_back) +
    geom_bar(aes(x = perc , y = reorder(科系, -perc)), stat = "identity") +
    labs(x = "百分比(%)", y = "科系", title = "後端工程師科系要求") +
    theme(text=element_text(family="STHeiti", size=14))

back_bar

ggsave(back_bar, file="back_bar.png", width=30, height=20, units = "cm")
    
#大數據

##資料讀取與清理
bigdata <- read.csv("bigdata.csv") %>%
    filter(salary != "待遇面議")
list_dep_bigdata <- strsplit(bigdata$department, "、")
vec_dep_bigdata <- unlist(list_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?資訊工程相關\\s?", "資訊工程相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?資訊管理相關\\s?", "資訊管理相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?電機電子工程相關\\s?", "工程相關", vec_dep_bigdata) 
vec_dep_bigdata <- gsub("\\s?其他數學及電算機科學相關\\s?", "其他數學及電算機科學相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?數學及電算機科學學科類\\s?", "其他數學及電算機科學相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?應用數學相關\\s?", "其他數學及電算機科學相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?一般數學相關\\s?", "其他數學及電算機科學相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?數理統計相關\\s?", "統計學相關", vec_dep_bigdata)
vec_dep_bigdata <- gsub("\\s?統計學相關\\s?", "統計學相關", vec_dep_bigdata)
vec_dep_bigdata[vec_dep_bigdata == " 市場行銷相關"] <- "市場行銷相關" 
vec_dep_bigdata[vec_dep_bigdata == " 商業及管理學科類" |
                    vec_dep_bigdata == "商業及管理學科類" |
                vec_dep_bigdata == " 其他商業及管理相關" |
                vec_dep_bigdata == "商業及管理學科類 " |
                vec_dep_bigdata == "企業管理相關 " |
                vec_dep_bigdata == " 一般商業學類" |
                vec_dep_bigdata == "會計學相關 " |
                vec_dep_bigdata == "企業管理相關" |
                vec_dep_bigdata == "市場行銷相關"] <- "其他商業及管理相關"
vec_dep_bigdata[vec_dep_bigdata == " 經濟學相關" |
                vec_dep_bigdata == "經濟學相關" |
                vec_dep_bigdata == "經濟社會及心理學科類 "] <- "經濟社會及心理學科類"
vec_dep_bigdata[vec_dep_bigdata == "工業工程相關 "] <- "工程相關"
vec_dep_bigdata[vec_dep_bigdata == "生物學相關 " |
                vec_dep_bigdata == "食品科學相關" |
                vec_dep_bigdata == "公共衛生相關 " |
                vec_dep_bigdata == "大眾傳播學科類" |
                vec_dep_bigdata == " 地理學相關" |
                vec_dep_bigdata == " 建築及都市規劃學科類"] <- "其他專業學科"
vec_dep_bigdata[vec_dep_bigdata == " 不拘 "] <- "不拘"


bigdatadt <- data.table(bigdata)
bigdata_m <- bigdatadt[grepl('月薪',salary)] #將年薪資料去除

##建立提及次數dataframe
df_dep_bigdata <- data.frame(table(vec_dep_bigdata))
df_dep_bigdata <- df_dep_bigdata %>%
    arrange(desc(Freq)) %>%
    mutate(perc = round(Freq/sum(Freq)*100)) %>%
    rename(科系 = vec_dep_bigdata)

##作長條圖
bigdata_bar <- ggplot(df_dep_bigdata) +
    geom_bar(aes(x = perc , y = reorder(科系, -perc)), stat = "identity") +
    labs(x = "百分比(%)", y = "科系", title = "大數據工程師科系要求") +
    theme(text=element_text(family="STHeiti", size=14))

bigdata_bar

ggsave(bigdata_bar, file="bigdata_bar.png", width=30, height=20, units = "cm")

#三種工程師薪水比較圖

##將月薪轉換成數字

vec_sal_product <- as.numeric(substr(product_m$salary,3, 4))

vec_sal_back <- as.numeric(substr(back_m$salary,3, 4))
vec_sal_back[vec_sal_back == 10] <- 100
 
vec_sal_bigdata <- as.numeric(substr(bigdata_m$salary,3, 4))

##製作有無科系要求之向量

vec_dep_product1 <- product_m$department
vec_dep_product1[vec_dep_product1 == "不拘"] <- "無"
vec_dep_product1[vec_dep_product1 != "無"] <- "有"

vec_dep_back1 <- back_m$department
vec_dep_back1[vec_dep_back1 == "不拘"] <- "無"
vec_dep_back1[vec_dep_back1 != "無"] <- "有"

vec_dep_bigdata1 <- bigdata_m$department
vec_dep_bigdata1[vec_dep_bigdata1 == " 不拘 "] <- "無"
vec_dep_bigdata1[vec_dep_bigdata1 != "無"] <- "有"

job <- vector("character", 354) 
for (i in(1:354)) {
    if(i<= 132) {
        job[i] <- "前端工程師"
    }
    if(i>132 & i<= 267) {
        job[i] <- "後端工程師"
    }
    if(i>267) {
        job[i] <- "大數據分析工程師"
    }
}

df_sal <- data.frame(Job = job,
                     Department = c(vec_dep_product1,
                                      vec_dep_back1,
                                      vec_dep_bigdata1),
                     Salary = c(vec_sal_product,
                                vec_sal_back,
                                vec_sal_bigdata))

df_sal1 <- df_sal %>%
    group_by(Job, Salary) %>%
    summarise(n = n()) %>%
    mutate(cumulative_ratio = cumsum(n/sum(n)*100))

salary_line <- ggplot(df_sal1, aes(Salary, cumulative_ratio, group = Job, colour = Job)) +
    geom_line(size = 1.2) +scale_fill_brewer(palette = "Pastel1") +
    labs(x = "月薪(單位:千元)", y = "累積百分比(%)", title = "薪水比較") +
    theme(text=element_text(family="STHeiti", size=14))

salary_line

ggsave(salary_line, file="salary_line.png", width=32, height=18, units = "cm")


dep_boxplot <- ggplot(df_sal, aes(x = Job, y = Salary, fill = Department)) +
    geom_boxplot() +
    facet_wrap(~Job, scale="free") +
    labs(y = "月薪(單位:千元)", fill = "科系要求", title = "科系要求薪水比較") +
    theme(text=element_text(family="STHeiti", size=14))

dep_boxplot
    
ggsave(dep_boxplot, file="dep_boxplot.png", width=32, height=18, units = "cm")
