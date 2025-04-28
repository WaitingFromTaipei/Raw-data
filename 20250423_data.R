library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)

Info <- read_csv("20250331高田前測進度.csv")

Trial1_Exp <- read_tsv("trial-1/20241010_trial-1_yellow_R.tsv")
Trial1_Ctr <- read_tsv("trial-1/20241010_trial-1_yellow_L.tsv")

Trial2_Exp <- read_tsv("trial-2/20241011_trial-2_59187_L.tsv")
Trial2_Ctr <- read_tsv("trial-2/20241011_trial-2_59187_R.tsv")

Trial3_Exp <- read_tsv("trial-3/20241013_trial-3_59215_R.tsv")
Trial3_Ctr <- read_tsv("trial-3/20241013_trial-3_59215_L.tsv")

Trial4_Exp <- read_tsv("trial-4/20241018_trial-4_59181_L.tsv")
Trial4_Ctr <- read_tsv("trial-4/20241018_trial-4_59181_R.tsv")

Trial5_Exp <- read_tsv("trial-5/20241017_trial-5_12693_L.tsv")
Trial5_Ctr <- read_tsv("trial-5/20241017_trial-5_12693_R.tsv")

Trial6_Exp <- read_tsv("trial-6/20241017_trial-6_59200_R.tsv")
Trial6_Ctr <- read_tsv("trial-6/20241017_trial-6_59200_L.tsv")

Trial7_Exp <- read_tsv("trial-7/20241024_trial-7_yellow_L.tsv")
Trial7_Ctr <- read_tsv("trial-7/20241024_trial-7_yellow_R.tsv")

Trial9_Exp <- read_tsv("trial-9/20241026_trial-9_59187_L.tsv")
Trial9_Ctr <- read_tsv("trial-9/20241026_trial-9_59187_R.tsv")

# 高田進食重量比較

Food_weight <- Info %>% select(Trial, `Exp剩餘食物量(g)`,`Control剩餘食物量(g)`,
                               `Exp被叼出食物量(g)`,`Control被叼出食物量(g)`) 

# 刪去部分數據（未在實驗當下準備食物／將叼出食物放回盤內秤重／因錄影失誤而重複進行之實驗）
Food_weight <- Food_weight %>% slice(-1,-2,-3,-9) # slice:使用橫列索引選取特定橫列

Food_weight$`Exp被叼出食物量(g)` <- as.numeric(Food_weight$`Exp被叼出食物量(g)`)
Food_weight$`Control被叼出食物量(g)` <- as.numeric(Food_weight$`Control被叼出食物量(g)`)
str(Food_weight)

# 後續可使用slice來選取特定次序之實驗數據
Food_weight <- Food_weight %>% mutate(`Exp food intake` = 30 - `Exp剩餘食物量(g)`,
                                      `Control food intake` = 30 - `Control剩餘食物量(g)`)
Food_weight <- Food_weight %>% mutate(`Exp food intake` = `Exp food intake` - `Exp被叼出食物量(g)`, 
                                      `Control food intake` = `Control food intake` - `Control被叼出食物量(g)`)


# summary
summary(Food_weight$`Exp food intake`)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.280   0.780   1.910   2.001   2.475   6.400 
summary(Food_weight$`Control food intake`)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.250   0.695   1.130   1.531   2.030   4.720 

# boxplot
# base
boxplot(Food_weight$`Exp food intake`, Food_weight$`Control food intake`)

# ggplot
Food_weight_gg <- Food_weight %>%
  select(`Exp food intake`,`Control food intake`)%>%
  rename(`Experimental group`=`Exp food intake`, `Control group`=`Control food intake`)%>%
  pivot_longer(cols = everything(), 
               names_to = "Group", 
               values_to = "Intake")
# rename(new_name = old_name)

Food_weight_box <- ggplot(data=Food_weight_gg,
                          mapping=aes(x = Group, y = Intake, fill = Group))+
  geom_boxplot() +
  labs(title = "Food Intake by Group", y = "Intake (g)", x = "")
Food_weight_box
ggsave(Food_weight_box,filename = "Outputs/Food_weight_box.png",width = 15,
       height = 15,units = "cm")

# histogram
# base
hist(Food_weight$`Exp food intake`)
hist(Food_weight$`Control food intake`)

# ggplot
Food_weight_histogram <- ggplot(Food_weight_gg, aes(x = Intake , fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Food Intake Histogram")
Food_weight_histogram
ggsave(Food_weight_histogram,filename = "Outputs/Food_weight_histogram.png",width = 15,
       height = 15,units = "cm")

# 常態性檢定:Shapiro-Wilk test
shapiro.test(Food_weight$`Exp food intake`) # W = 0.87456, p-value = 0.03938
shapiro.test(Food_weight$`Control food intake`) # W = 0.87289, p-value = 0.03721

# QQ Plot
qqnorm(Food_weight$`Exp food intake`)
qqline(Food_weight$`Exp food intake`, col = "red")

qqnorm(Food_weight$`Control food intake`)
qqline(Food_weight$`Control food intake`, col = "blue")

# Density plot
Food_weight_density <- ggplot(Food_weight_gg, aes(x = Intake, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Food Intake by Group")
Food_weight_density
ggsave(Food_weight_density,filename = "Outputs/Food_weight_density.png",width = 15,
       height = 15,units = "cm")



# Mann–Whitney U test
wilcox.test(Food_weight$`Exp food intake`, Food_weight$`Control food intake`, paired = TRUE)
# V = 76, p-value = 0.3894
# alternative hypothesis: true location shift is not equal to 0



# 高田碰觸時間比較
Duration <- function(df, behavior_name) {
  df_filtered <- df %>% 
    filter(Behavior == behavior_name) %>%
    select(`Behavior type`, Time)
  
  df_start <- df_filtered %>%
    filter(`Behavior type` == "START") %>%
    rename(Start = Time)
  
  df_stop <- df_filtered %>%
    filter(`Behavior type` == "STOP") %>%
    rename(Stop = Time)
  
  result <- df_start %>%
    select(Start) %>%
    mutate(Stop = df_stop$Stop) %>%
    mutate(Duration = Stop - Start)
  
  return(result)
}

Dura1_exp <- Duration(Trial1_Exp, "Right cage")
Dura1_ctr <- Duration(Trial1_Ctr, "Left cage")

Dura2_exp <- Duration(Trial2_Exp, "Left cage")
Dura2_ctr <- Duration(Trial2_Ctr, "Right cage")

Dura3_exp <- Duration(Trial3_Exp, "Right cage")
Dura3_ctr <- Duration(Trial3_Ctr, "Left cage")

Dura4_exp <- Duration(Trial4_Exp, "Left cage")
Dura4_ctr <- Duration(Trial4_Ctr, "Right cage")

Dura5_exp <- Duration(Trial5_Exp, "Left cage")
Dura5_ctr <- Duration(Trial5_Ctr, "Right cage")

Dura6_exp <- Duration(Trial6_Exp, "Right cage")
Dura6_ctr <- Duration(Trial6_Ctr, "Left cage")

Dura7_exp <- Duration(Trial7_Exp, "Left cage")
Dura7_ctr <- Duration(Trial7_Ctr, "Right cage")

Dura9_exp <- Duration(Trial9_Exp, "Left cage")
Dura9_ctr <- Duration(Trial9_Ctr, "Right cage")

# 用3,4,5,6,7,9次實驗資料
# 總接觸時長
sum_exp<- c(sum(Dura3_exp$Duration), sum(Dura4_exp$Duration),
            sum(Dura5_exp$Duration), sum(Dura6_exp$Duration),
            sum(Dura7_exp$Duration), sum(Dura9_exp$Duration))

sum_ctr<- c(sum(Dura3_ctr$Duration), sum(Dura4_ctr$Duration),
            sum(Dura5_ctr$Duration), sum(Dura6_ctr$Duration),
            sum(Dura7_ctr$Duration), sum(Dura9_ctr$Duration))

Dura_sum <- data.frame(sum_exp, sum_ctr)

# summary
summary(sum_exp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 484.7   497.8   658.8   678.4   857.1   900.6 
summary(sum_ctr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 433.9   493.7   662.8   646.5   798.1   839.1 



# boxplot 
# base
boxplot(Dura_sum$sum_exp,Dura_sum$sum_ctr)

# ggplot
Dura_sum_gg <- Dura_sum %>%
  rename(`Experimental group`=sum_exp, `Control group`=sum_ctr)%>%
  pivot_longer(cols = everything(), 
               names_to = "Group", 
               values_to = "Duration")

Dura_sum_box <- ggplot(data=Dura_sum_gg,
                       mapping=aes(x = Group, y = Duration, fill = Group))+
  geom_boxplot() +
  labs(title = "Total Contact Time (First Six Individuals)", y = "Duration (s)", x = "")
Dura_sum_box
ggsave(Dura_sum_box,filename = "Outputs/Dura_sum_box.png",width = 15,
       height = 15,units = "cm")

# histogram
# base
hist(Dura_sum$sum_exp)
hist(Dura_sum$sum_ctr)

# density plot
# ggplot
Dura_sum_density <- ggplot(Dura_sum_gg, aes(x = Duration, color = Group, fill = Group)) +
  geom_density(alpha = 0.4) +
  labs(title = "Total contact time (First Six Individuals)")
Dura_sum_density
ggsave(Dura_sum_density,filename = "Outputs/Dura_sum_density.png",width = 15,
       height = 15,units = "cm")

# Mann–Whitney U test
wilcox.test(Dura_sum$sum_exp, Dura_sum$sum_ctr, paired = TRUE)
# V = 12, p-value = 0.8438
# alternative hypothesis: true location shift is not equal to 0



# 每次接觸時長
# 觀察兩組觸碰時長的分布是否不同
List_Exp <- list(Dura3_exp$Duration, Dura4_exp$Duration,
                 Dura5_exp$Duration, Dura6_exp$Duration,
                 Dura7_exp$Duration, Dura9_exp$Duration) 
List_Ctr <- list(Dura3_ctr$Duration, Dura4_ctr$Duration,
                 Dura5_ctr$Duration, Dura6_ctr$Duration,
                 Dura7_ctr$Duration, Dura9_ctr$Duration)

exp_df <- data.frame(Duration = unlist(List_Exp),
                              Group = "Experimental")
ctr_df <- data.frame(Duration = unlist(List_Ctr),
                         Group = "Control")

Dura_each <- rbind(exp_df, ctr_df)

# ggplot
# boxplot
Dura_each_box <- ggplot(data=Dura_each,
                       mapping=aes(x = Group, y = Duration, fill = Group))+
  geom_boxplot() +
  labs(title = "Contact Time per Contact (First Six Individuals)", y = "Duration (s)", x = "")
Dura_each_box
ggsave(Dura_each_box,filename = "Outputs/Dura_each_box.png",width = 15,
       height = 15,units = "cm")

# density plot
Dura_density <- ggplot(Dura_each, aes(x = Duration, color = Group, fill = Group)) +
  geom_density(alpha = 0.4) +
  labs(x = "Contact Duration (s)", y = "Density", title = "Contact Duration Density Plot")
Dura_density 
ggsave(Dura_density ,filename = "Outputs/Dura_density.png",width = 15,
       height = 15,units = "cm")

# histogram
Dura_histogram <- ggplot(Dura_each, aes(x = Duration, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Contact duration (s)", y = "Count", fill = "Object", title = "Contact Duration Histogram")
Dura_histogram
ggsave(Dura_histogram,filename = "Outputs/Dura_histogram.png",width = 15,
       height = 15,units = "cm")


# 常態性檢定:Shapiro-Wilk test
shapiro.test(exp_df$Duration) # W = 0.60328, p-value < 2.2e-16
shapiro.test(ctr_df$Duration) # W = 0.56238, p-value < 2.2e-16

# QQ Plot
qqnorm(exp_df$Duration)
qqline(exp_df$Duration, col = "red")

qqnorm(ctr_df$Duration)
qqline(ctr_df$Duration, col = "blue")


# Mann–Whitney U test
wilcox.test(exp_df$Duration, ctr_df$Duration, paired = FALSE)
# data:  exp_df$Duration and ctr_df$Duration
# W = 34618, p-value = 0.4193
# alternative hypothesis: true location shift is not equal to 0
