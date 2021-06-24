library(readr)
library(dplyr)
library(base)
library(data.table)
library(ggplot2)
library(stringr)

#資料讀取與清理
back_t <- read_csv("../../data/back.csv")
bigdata_t <- read_csv("../../data/bigdata.csv")
front_t <- read_csv("../../data/front.csv")

#將tools欄位中不相關的字詞去除
back_t$tools <- gsub("贊助提升專業能力", "", back_t$tools)
bigdata_t$tools <- gsub("贊助提升專業能力", "", bigdata_t$tools)
front_t$tools <- gsub("贊助提升專業能力", "", front_t$tools)

back_t$tools <- gsub(" 不拘 ", "不拘", back_t$tools)
bigdata_t$tools <- gsub(" 不拘 ", "不拘", bigdata_t$tools)
front_t$tools <- gsub(" 不拘 ", "不拘", front_t$tools)

#整理出tools欄位中提及的種類和次數
list_tools_back <- strsplit(back_t$tools, "、")
list_tools_bigdata <- strsplit(bigdata_t$tools, "、")
list_tools_front <- strsplit(front_t$tools, "、")

vec_tools_back <- unlist(list_tools_back)
vec_tools_bigdata <- unlist(list_tools_bigdata)
vec_tools_front <- unlist(list_tools_front)

df_tools_back <- data.frame(table(vec_tools_back))
df_tools_bigdata <- data.frame(table(vec_tools_bigdata))
df_tools_front <- data.frame(table(vec_tools_front))

df_tools_back <- df_tools_back %>%
  arrange(desc(Freq)) %>%
  mutate(perc = Freq/160*100) %>%
  rename(tools = vec_tools_back)
df_tools_bigdata <- df_tools_bigdata %>%
  arrange(desc(Freq)) %>%
  mutate(perc = Freq/160*100) %>%
  rename(tools = vec_tools_bigdata)
df_tools_front <- df_tools_front %>%
  arrange(desc(Freq)) %>%
  mutate(perc = Freq/160*100) %>%
  rename(tools = vec_tools_front)

#去除年薪，只留月薪
back_tm <- back_t %>% filter(str_detect(salary, "月薪"))
bigdata_tm <- bigdata_t %>% filter(str_detect(salary, "月薪"))
front_tm <- front_t %>% filter(str_detect(salary, "月薪"))

#將salary欄位整理成數字並以千元為單位
vec_sal_back <- as.numeric(substr(back_tm$salary, 3, 4))
vec_sal_back[vec_sal_back == 10] <- 100
vec_sal_bigdata <- as.numeric(substr(bigdata_tm$salary, 3, 4))
vec_sal_front <- as.numeric(substr(front_tm$salary, 3, 4))

#列出較不常用的工具
df_tools_backu <- df_tools_back %>% filter(df_tools_back$Freq < 4)

vec_tools_backu <- as.character(df_tools_backu$tools)

df_tools_bigdatau <- df_tools_bigdata %>% filter(df_tools_bigdata$Freq < 2)

vec_tools_bigdatau <- as.character(df_tools_bigdatau$tools)

df_tools_frontu <- df_tools_front %>% filter(df_tools_front$Freq < 4)

vec_tools_frontu <- as.character(df_tools_frontu$tools)

#製作有無工具要求要求之向量
vec_tools_back1 <- back_tm$tools
vec_tools_bigdata1 <- bigdata_tm$tools
vec_tools_front1 <- front_tm$tools
##不拘
vec_tools_back1[vec_tools_back1 == "不拘"] <- "不拘"
vec_tools_bigdata1[vec_tools_bigdata1 == "不拘"] <- "不拘"
vec_tools_front1[vec_tools_front1 == "不拘"] <- "不拘"
##有要求
vec_tools_back1[vec_tools_back1 != "不拘"] <- "有"
vec_tools_bigdata1[vec_tools_bigdata1 != "不拘"] <- "有"
vec_tools_front1[vec_tools_front1 != "不拘"] <- "有"
##冷門要求
for(i in seq_along(back_tm$tools)){
  for(j in seq_along(vec_tools_backu)){
    if(str_detect(back_tm$tools[i], vec_tools_backu[j]) == TRUE){
      vec_tools_back1[i] <- "冷門要求"
    }
  }
}
for(i in seq_along(bigdata_tm$tools)){
  for(j in seq_along(vec_tools_bigdatau)){
    if(str_detect(bigdata_tm$tools[i], vec_tools_bigdatau[j]) == TRUE){
      vec_tools_bigdata1[i] <- "冷門要求"
    }
  }
}
for(i in seq_along(front_tm$tools)){
  for(j in seq_along(vec_tools_frontu)){
    if(str_detect(front_tm$tools[i], vec_tools_frontu[j]) == TRUE){
      vec_tools_front1[i] <- "冷門要求"
    }
  }
}

#將三種資料彙整
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

df_sal_t <- data.frame(Job = job,
                       Tools = c(vec_tools_front1,
                                 vec_tools_back1,
                                 vec_tools_bigdata1),
                       Salary = c(vec_sal_front,
                                  vec_sal_back,
                                  vec_sal_bigdata))
df_sal1_t <- df_sal_t %>%
  group_by(Job, Salary) %>%
  summarise(n = n()) %>%
  mutate(cumulative_ratio = cumsum(n/sum(n)*100))

#畫三種薪水累計比較圖
salary_line_t <- ggplot(df_sal1_t, aes(Salary, cumulative_ratio, group = Job, colour = Job)) +
  geom_line(size = 1.2) +scale_fill_brewer(palette = "Pastel1") +
  labs(x = "月薪(單位:千元)", y = "累積百分比(%)", title = "薪水比較") +
  theme(text=element_text(family="STHeiti", size=14))


#畫箱型圖
dep_boxplot_t <- ggplot(df_sal_t, aes(x = Job, y = Salary, fill = Tools)) +
  geom_boxplot() +
  facet_wrap(~Job, scale="free") +
  labs(y = "月薪(單位:千元)", fill = "工具要求", title = "工具要求薪水比較") +
  theme(text=element_text(family="STHeiti", size=14))


#取提及次數最多的工具前十名
df_tools_back10 <- head(df_tools_back, n = 10)
df_tools_bigdata10 <- head(df_tools_bigdata, n = 10)
df_tools_front10 <- head(df_tools_front, n = 10)

#畫長條圖
back_bar_t <- ggplot(df_tools_back10) +
  geom_bar(aes(x = perc , y = reorder(tools, -perc)), stat = "identity") +
  labs(x = "提及該工具的公司百分比", y = "工具", title = "後端工程師工具要求前十名") +
  theme(text=element_text(family="STHeiti", size=14))

bigdata_bar_t <- ggplot(df_tools_bigdata10) +
  geom_bar(aes(x = perc , y = reorder(tools, -perc)), stat = "identity") +
  labs(x = "提及該工具的公司百分比", y = "工具", title = "大數據工程師工具要求前十名") +
  theme(text=element_text(family="STHeiti", size=14))

front_bar_t <- ggplot(df_tools_front10) +
  geom_bar(aes(x = perc , y = reorder(tools, -perc)), stat = "identity") +
  labs(x = "提及該工具的公司百分比", y = "工具", title = "前端工程師工具要求前十名") +
  theme(text=element_text(family="STHeiti", size=14))

ggsave(back_bar_t, file="../../report/graph_output/back_bar_t.png", width=32, height=18, units = "cm")

ggsave(front_bar_t, file="../../report/graph_output/front_bar_t.png", width=32, height=18, units = "cm")

ggsave(bigdata_bar_t, file="../../report/graph_output/bigdata_t.png", width=32, height=18, units = "cm")

ggsave(dep_boxplot_t, file="../../report/graph_output/dep_boxplot_t.png", width=32, height=18, units = "cm")