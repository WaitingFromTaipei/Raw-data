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
Tr3_E <- read_tsv("trial-3_N/20241013_trial-3_59215_R_Time_budget.tsv")
Tr3_C <- read_tsv("trial-3_N/20241013_trial-3_59215_L_Time_budget.tsv")

Tr4_E <- read_tsv("trial-4_N/20241018_trial-4_59181_L_Time_budget.tsv")
Tr4_C <- read_tsv("trial-4_N/20241018_trial-4_59181_R_Time_budget.tsv")

Tr5_E <- read_tsv("trial-5_N/20241017_trial-5_12693_L_Time_budget.tsv")
Tr5_C <- read_tsv("trial-5_N/20241017_trial-5_12693_R_Time_budget.tsv")

Tr6_E <- read_tsv("trial-6_N/20241017_trial-6_59200_R_Time_budget.tsv")
Tr6_C <- read_tsv("trial-6_N/20241017_trial-6_59200_L_Time_budget.tsv")

Tr7_E <- read_tsv("trial-7_N/20241024_trial-7_yellow_L_Time_budget.tsv")
Tr7_C <- read_tsv("trial-7_N/20241024_trial-7_yellow_R_Time_budget.tsv")

Tr9_E <- read_tsv("trial-9_N/20241026_trial-9_59187_R_Time_budget.tsv")
Tr9_C <- read_tsv("trial-9_N/20241026_trial-9_59187_L_Time_budget.tsv")

Tr10_E <- read_tsv("trial-10/20241217_trial-10_CL2-23_R_Time_budget.tsv")
Tr10_C <- read_tsv("trial-10/20241217_trial-10_CL2-23_L_Time_budget.tsv")

Tr11_E <- read_tsv("trial-11/20241218_trial-11_CL2-16_L_Time_budget.tsv")
Tr11_C <- read_tsv("trial-11/20241218_trial-11_CL2-16_R_Time_budget.tsv")

Tr12_E <- read_tsv("trial-12/20241218_trial-12_CL2-24_R_Time_budget.tsv")
Tr12_C <- read_tsv("trial-12/20241218_trial-12_CL2-24_L_Time_budget.tsv")

Tr13_E <- read_tsv("trial-13/20241218_trial-13_CL1-7_R_Time_budget.tsv")
Tr13_C <- read_tsv("trial-13/20241218_trial-13_CL1-7_L_Time_budget.tsv")

Tr14_E <- read_tsv("trial-14/20241218_trial-14_CL1-4_L_Time_budget.tsv")
Tr14_C <- read_tsv("trial-14/20241218_trial-14_CL1-4_R_Time_budget.tsv")

Tr16_E <- read_tsv("trial-16/20250216_trial-16_CL5-5_R_Time_budget.tsv")
Tr16_C <- read_tsv("trial-16/20250216_trial-16_CL5-5_L_Time_budget.tsv")

Tr17_E <- read_tsv("trial-17/20250217_trial-17_CL5-14_L_Time_budget.tsv")
Tr17_C <- read_tsv("trial-17/20250217_trial-17_CL5-14_R_Time_budget.tsv")

Tr18_E <- read_tsv("trial-18/20250217_trial-18_CL3-20_R_Time_budget.tsv")
Tr18_C <- read_tsv("trial-18/20250217_trial-18_CL3-20_L_Time_budget.tsv")

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
# S = 8, p-value = 0.7905
# 95 percent confidence interval:
#  -0.6580769  1.7082967
# sample estimates:
# median of x-y 
#           0.24  





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

# summary
summary(Time_E)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.0   500.5   808.0   705.7   900.0  1266.0 
summary(Time_C)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 111.0   444.5   604.0   647.1   803.0  1385.0

# Shapiro-Wilk test
shapiro.test(Time_E) # W = 0.9541, p-value = 0.5911
shapiro.test(Time_C) # W = 0.96128, p-value = 0.7146

# QQ Plot
qqnorm(Time_E)
qqline(Time_E, col = "red")

qqnorm(Time_C)
qqline(Time_C, col = "blue")

# Dependent-Samples Sign Test
SIGN.test(Time_E, Time_C, alternative = "two.sided")

# S = 9, p-value = 0.6072

# 95 percent confidence interval:
# -299.2409  466.8218

# sample estimates:
# median of x-y
#           204

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
# 7.00   11.00   22.00   27.93   40.00   75.00
summary(Sniff_C)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.0    13.5    25.0    33.0    40.0    90.0 

# 常態性檢定:Shapiro-Wilk test
shapiro.test(Sniff_E) # W = 0.83673, p-value = 0.01133
shapiro.test(Sniff_C) # W = 0.83282, p-value = 0.01001

# QQ Plot
qqnorm(Sniff_E)
qqline(Sniff_E, col = "red")

qqnorm(Sniff_C)
qqline(Sniff_C, col = "blue")


# Dependent-Samples Sign Test
SIGN.test(Sniff_E, Sniff_C, alternative = "two.sided")
# S = 7, p-value = 1
# 95 percent confidence interval:
#  -13.93099  10.93099
# sample estimates:
# median of x-y 
#           -4 


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

