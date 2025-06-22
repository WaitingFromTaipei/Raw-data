library(dplyr)
library(readr)
library(openxlsx)
library(tidyr)
library(BSDA) # SIGN.test
library(ggplot2)
library(ggthemes)

# 紙本記錄
Info <- read.xlsx("20250621高田前測進度.xlsx")

# 影片紀錄
# 實驗組(E)/對照組(C)

# Trial 3: 排遺放置於右側
Tr3_E <- read_tsv("trial-3_N/20241013_trial-3_59215_R_Time_budget.tsv")
Tr3_C <- read_tsv("trial-3_N/20241013_trial-3_59215_L_Time_budget.tsv")

# Trial 4: 排遺放置於左側
Tr4_E <- read_tsv("trial-4_N/20241018_trial-4_59181_L_Time_budget.tsv")
Tr4_C <- read_tsv("trial-4_N/20241018_trial-4_59181_R_Time_budget.tsv")

# Trial 5: 排遺放置於左側
Tr5_E <- read_tsv("trial-5_N/20241017_trial-5_12693_L_Time_budget.tsv")
Tr5_C <- read_tsv("trial-5_N/20241017_trial-5_12693_R_Time_budget.tsv")

# Trial 6: 排遺放置於右側
Tr6_E <- read_tsv("trial-6_N/20241017_trial-6_59200_R_Time_budget.tsv")
Tr6_C <- read_tsv("trial-6_N/20241017_trial-6_59200_L_Time_budget.tsv")

# Trial 7: 排遺放置於左側
Tr7_E <- read_tsv("trial-7_N/20241024_trial-7_yellow_L_Time_budget.tsv")
Tr7_C <- read_tsv("trial-7_N/20241024_trial-7_yellow_R_Time_budget.tsv")

# Trial 9: 排遺放置於左側
Tr9_E <- read_tsv("trial-9_N/20241026_trial-9_59187_L_Time_budget.tsv")
Tr9_C <- read_tsv("trial-9_N/20241026_trial-9_59187_R_Time_budget.tsv")

# Trial 10: 排遺放置於右側
Tr10_E <- read_tsv("trial-10/20241217_trial-10_CL2-23_R_Time_budget.tsv")
Tr10_C <- read_tsv("trial-10/20241217_trial-10_CL2-23_L_Time_budget.tsv")

# Trial 11: 排遺放置於左側
Tr11_E <- read_tsv("trial-11/20241218_trial-11_CL2-16_L_Time_budget.tsv")
Tr11_C <- read_tsv("trial-11/20241218_trial-11_CL2-16_R_Time_budget.tsv")

# Trial 12: 排遺放置於右側
Tr12_E <- read_tsv("trial-12/20241218_trial-12_CL2-24_R_Time_budget.tsv")
Tr12_C <- read_tsv("trial-12/20241218_trial-12_CL2-24_L_Time_budget.tsv")

# Trial 13: 排遺放置於左側
Tr13_E <- read_tsv("trial-13/20241218_trial-13_CL1-7_L_Time_budget.tsv")
Tr13_C <- read_tsv("trial-13/20241218_trial-13_CL1-7_R_Time_budget.tsv")

# Trial 14: 排遺放置於右側
Tr14_E <- read_tsv("trial-14/20241218_trial-14_CL1-4_R_Time_budget.tsv")
Tr14_C <- read_tsv("trial-14/20241218_trial-14_CL1-4_L_Time_budget.tsv")

# Trial 16: 排遺放置於右側
Tr16_E <- read_tsv("trial-16/20250216_trial-16_CL5-5_R_Time_budget.tsv")
Tr16_C <- read_tsv("trial-16/20250216_trial-16_CL5-5_L_Time_budget.tsv")

# Trial 17: 排遺放置於左側
Tr17_E <- read_tsv("trial-17/20250217_trial-17_CL5-14_L_Time_budget.tsv")
Tr17_C <- read_tsv("trial-17/20250217_trial-17_CL5-14_R_Time_budget.tsv")

# Trial 18: 排遺放置於右側
Tr18_E <- read_tsv("trial-18/20250217_trial-18_CL3-20_R_Time_budget.tsv")
Tr18_C <- read_tsv("trial-18/20250217_trial-18_CL3-20_L_Time_budget.tsv")

# Trial 19: 排遺放置於右側
Tr19_E <- read_tsv("trial-19/20250219_trial-19_CL3-8-2_R_Time_budget.tsv")
Tr19_C <- read_tsv("trial-19/20250219_trial-19_CL3-8-2_L_Time_budget.tsv")





# 高田進食重量比較

Food_weight <- Info %>% select(`Exp剩餘食物量(g)`,`Control剩餘食物量(g)`,
                               `Exp被叼出食物量(g)`,`Control被叼出食物量(g)`) 

# 刪去部分數據（未在實驗當下準備食物／將叼出食物放回盤內秤重／因錄影失誤而重複進行之實驗）
Food_weight <- Food_weight %>% slice(-1,-2,-3,-9,-19) # slice:使用橫列索引選取特定橫列

Food_weight$`Exp被叼出食物量(g)` <- as.numeric(Food_weight$`Exp被叼出食物量(g)`)
Food_weight$`Control被叼出食物量(g)` <- as.numeric(Food_weight$`Control被叼出食物量(g)`)

Food_weight <- Food_weight %>% mutate(intake_E = 30 - `Exp剩餘食物量(g)` - `Exp被叼出食物量(g)`, 
                                      intake_C = 30 - `Control剩餘食物量(g)` - `Control被叼出食物量(g)`)

str(Food_weight)

# Dependent-Samples Sign Test
SIGN.test(Food_weight$intake_E, Food_weight$intake_C, alternative = "two.sided")

# data:  Food_weight$intake_E and Food_weight$intake_C
# S = 8, p-value = 0.7905
# alternative hypothesis: true median difference is not equal to 0
# 95 percent confidence interval:
# -0.6580769  1.7082967
# sample estimates:
# median of x-y 
#           0.24 

# Achieved and Interpolated Confidence Intervals: 
  
#                   Conf.Level  L.E.pt U.E.pt
# Lower Achieved CI     0.9426 -0.6000 1.7000
# Interpolated CI       0.9500 -0.6581 1.7083
# Upper Achieved CI     0.9871 -0.9500 1.7500





# 高田碰觸時間比較
# list[[column]][row]
Time_E <- c(Tr3_E[[11]][1], Tr4_E[[11]][1], Tr5_E[[11]][1], Tr6_E[[11]][1],
            Tr7_E[[11]][1], Tr9_E[[11]][1], Tr10_E[[11]][1], Tr11_E[[11]][1],
            Tr12_E[[11]][1], Tr13_E[[11]][1], Tr14_E[[11]][1], Tr16_E[[11]][1],
            Tr17_E[[11]][1], Tr18_E[[11]][1], Tr19_E[[11]][1])

Time_C <- c(Tr3_C[[11]][1], Tr4_C[[11]][1], Tr5_C[[11]][1], Tr6_C[[11]][1],
            Tr7_C[[11]][1], Tr9_C[[11]][1], Tr10_C[[11]][1], Tr11_C[[11]][1],
            Tr12_C[[11]][1], Tr13_C[[11]][1], Tr14_C[[11]][1], Tr16_C[[11]][1],
            Tr17_C[[11]][1], Tr18_C[[11]][1], Tr19_C[[11]][1])

# 將每個值無條件捨去為整數
Time_E <- floor(Time_E)
Time_C <- floor(Time_C)

Time <- data.frame(Time_E, Time_C)
Time <- Time %>% mutate(Difference = Time_E - Time_C)
str(Time)

# summary
summary(Time_E)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.0   489.0   806.0   675.6   900.0  1084.0 
summary(Time_C)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 111.0   433.5   718.0   677.3   828.5  1385.0 

# Shapiro-Wilk test
shapiro.test(Time_E) # W = 0.92456, p-value = 0.226
shapiro.test(Time_C) # W = 0.95744, p-value = 0.648

# QQ Plot
qqnorm(Time_E)
qqline(Time_E, col = "red")

qqnorm(Time_C)
qqline(Time_C, col = "blue")

# Dependent-Samples Sign Test
SIGN.test(Time_E, Time_C, alternative = "two.sided")

# data:  Time_E and Time_C
# S = 8, p-value = 1
# alternative hypothesis: true median difference is not equal to 0
# 95 percent confidence interval:
#  -345.5169  466.8218
# sample estimates:
# median of x-y 
#           69 

# Achieved and Interpolated Confidence Intervals: 
  
#                   Conf.Level    L.E.pt   U.E.pt
# Lower Achieved CI     0.8815 -311.0000 466.0000
# Interpolated CI       0.9500 -345.5169 466.8218
# Upper Achieved CI     0.9648 -353.0000 467.0000

# ggplot
Time_gg <- Time %>%
  select(Time_E, Time_C) %>%
  rename(`Experimental group`=Time_E, `Control group`=Time_C)%>%
  pivot_longer(cols = everything(), 
               names_to = "Group", 
               values_to = "Duration")
# boxplot
Time_box <- ggplot(data=Time_gg,
                   mapping=aes(x = Group, y = Duration, fill = Group))+
  geom_boxplot() +
  labs(title = "Total Contact Time", y = "Duration (s)", x = "")
Time_box
ggsave(Time_box,filename = "Outputs/Time_box.png",width = 15,
       height = 15,units = "cm")

# histogram
Time_histogram <- ggplot(data=Time_gg,
                         mapping=aes(x = Duration , fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Total contact time")
Time_histogram
ggsave(Time_histogram,filename = "Outputs/Time_histogram.png",width = 15,
       height = 15,units = "cm")







# 高田嗅聞次數比較

Sniffing <- function(x){
  idx <- which(x[[8]] == "Sniffing")
  x[[10]][idx]
}

Sniff_E <- c(Sniffing(Tr3_E), Sniffing(Tr4_E), Sniffing(Tr5_E), Sniffing(Tr6_E),
             Sniffing(Tr7_E), Sniffing(Tr9_E), Sniffing(Tr10_E), Sniffing(Tr11_E),
             Sniffing(Tr12_E), Sniffing(Tr13_E), Sniffing(Tr14_E), Sniffing(Tr16_E),
             Sniffing(Tr17_E), Sniffing(Tr18_E), Sniffing(Tr19_E))

Sniff_C <- c(Sniffing(Tr3_C), Sniffing(Tr4_C), Sniffing(Tr5_C), Sniffing(Tr6_C),
             Sniffing(Tr7_C), Sniffing(Tr9_C), Sniffing(Tr10_C), Sniffing(Tr11_C),
             Sniffing(Tr12_C), Sniffing(Tr13_C), Sniffing(Tr14_C), Sniffing(Tr16_C),
             Sniffing(Tr17_C), Sniffing(Tr18_C), Sniffing(Tr19_C))

Sniff <- data.frame(Sniff_E, Sniff_C)
Sniff <- Sniff %>% mutate(Difference = Sniff_E - Sniff_C)

# summary
summary(Sniff_E)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.0    13.0    25.0    31.0    40.5    85.0 
summary(Sniff_C)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.00   11.50   23.00   29.93   37.50   90.00 

# Shapiro-Wilk test
shapiro.test(Sniff_E) # W = 0.86258, p-value = 0.02629
shapiro.test(Sniff_C) # W = 0.82397, p-value = 0.007592

# QQ Plot
qqnorm(Sniff_E)
qqline(Sniff_E, col = "red")

qqnorm(Sniff_C)
qqline(Sniff_C, col = "blue")

# Dependent-Samples Sign Test
SIGN.test(Sniff_E, Sniff_C, alternative = "two.sided")

# data:  Sniff_E and Sniff_C
# S = 8, p-value = 1
# alternative hypothesis: true median difference is not equal to 0
# 95 percent confidence interval:
#  -13.93099  10.93099
# sample estimates:
# median of x-y 
#           1 

# Achieved and Interpolated Confidence Intervals: 
  
#                   Conf.Level  L.E.pt U.E.pt
# Lower Achieved CI     0.8815  -9.000  6.000
# Interpolated CI       0.9500 -13.931 10.931
# Upper Achieved CI     0.9648 -15.000 12.000


# ggplot
Sniff_gg <- Sniff %>%
  select(Sniff_E, Sniff_C) %>%
  rename(`Experimental group`=Sniff_E, `Control group`=Sniff_C)%>%
  pivot_longer(cols = everything(), 
               names_to = "Group", 
               values_to = "Count")

# boxplot
Sniff_box <- ggplot(data=Sniff_gg,
                   mapping=aes(x = Group, y = Count, fill = Group))+
  geom_boxplot() +
  labs(title = "Number of sniffs", y = "Count", x = "")
Sniff_box
ggsave(Sniff_box,filename = "Outputs/Sniff_box.png",width = 15,
       height = 15,units = "cm")

# histogram
Sniff_histogram <- ggplot(data=Sniff_gg, 
                          mapping=aes(x = Count, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Number of sniffs", y = "Count", fill = "Object", title = "Number of sniffs")
Sniff_histogram
ggsave(Sniff_histogram,filename = "Outputs/Sniff_histogram.png",width = 15,
       height = 15,units = "cm")

