# 將手動整合完的excel檔另存成csv檔後匯入
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

df <- read_csv("Intergrated Data/20241011_trial-2_59187.csv") %>%
  mutate(Time = as.numeric(Time))



# 行為出現次數統計
# 計算各種行為的出現次數
behavior_counts <- df %>%
  filter(`Behavior type` != "STOP") %>%
  group_by(Behavior) %>%
  summarise(Count = n())

# 自訂順序
order <- c('Left cage','Left','Middle','Right','Right cage')
behavior_counts$Behavior <- factor(behavior_counts$Behavior, 
                                   levels = c(order, setdiff(behavior_counts$Behavior, order)))
  
# 繪製行為次數長條圖
ggplot(behavior_counts, aes(x = Behavior, y = Count, fill = Behavior)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Number of occurrences of behaviors",
       x = "Behaviors", y = "Number of occurrences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 行為總持續時間統計
# 計算各種行為的總持續時間
behavior_durations <- df %>%
  filter(`Behavior type` %in% c("START", "STOP")) %>%
  group_by(Behavior) %>%
  mutate(Next_Time = lead(Time)) %>%  # 找出下一行為的時間（應該是 STOP 的時間）
  filter(`Behavior type` == "START") %>%  # 只保留 START，計算 START 到 STOP 的時間差
  mutate(Duration = Next_Time - Time) %>%
  group_by(Behavior) %>%  # 按行為類型分組
  summarise(Total_Duration = sum(Duration, na.rm = TRUE))  # 計算總持續時間

# 自訂順序
behavior_durations$Behavior <- factor(behavior_durations$Behavior, 
                                   levels = c(order, setdiff(behavior_counts$Behavior, order)))

# 繪製行為持續時間長條圖
ggplot(behavior_durations, aes(x = Behavior, y = Total_Duration, fill = Behavior)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Durations of behaviors",
       x = "Behaviors", y = "Duration (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Event timeline plot/Gantt chart
# 配對 START 和 STOP 計算行為持續時間
df_duration <- df %>%
  filter(`Behavior type` %in% c("START", "STOP")) %>%
  group_by(Subject, Behavior) %>%
  arrange(Time) %>%
  mutate(Next_Time = lead(Time)) %>%
  filter(`Behavior type` == "START") %>%
  mutate(Duration = Next_Time - Time) %>%
  ungroup()

# 轉換時間格式
df_duration <- df_duration %>%
  mutate(Time = as.POSIXct(Time, origin = "1970-01-01", tz = "UTC"),
         End_Time = as.POSIXct(Next_Time, origin = "1970-01-01", tz = "UTC"))
T0 <- as.Date(df_duration$Time[1])  # 取第一個時間點的日期
df_duration$End_Time[1] <- as.POSIXct(paste(T0, "00:00:00"), tz = "UTC")


# 繪製時間軸行為對應圖
ggplot(df_duration, aes(x = Time, xend = End_Time, y = Behavior, yend = Behavior, color = Behavior)) +
  geom_segment(size = 5) +  # 使用線條代表時間持續長度
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "5 min") +  # 設定時間標籤
  theme_minimal() +
  labs(title = "20241011_trial-2_59187",
       x = "Time (HH:MM:SS)",
       y = "Behavior",
       color = "Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
