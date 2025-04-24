# 將手動整合完的excel檔另存成csv檔後匯入
library(readr)
library(dplyr)
library(tidyr)

# 讀入資料
df <- read_csv("Intergrated Data/20241013_trial-3_59215.csv") %>%
  mutate(Time = as.numeric(Time))

# Total number of occurences
# 行為出現次數統計
# 計算各種行為的出現次數
behavior_counts <- df %>%
  filter(`Behavior type` != "STOP") %>%
  group_by(Behavior) %>%
  summarise(Count = n())

# Total duration (s)
behavior_durations <- df %>%
  filter(`Behavior type` %in% c("START", "STOP")) %>%
  group_by(Behavior) %>%
  mutate(Next_Time = lead(Time)) %>%  # 找出下一行為的時間（應該是 STOP 的時間）
  filter(`Behavior type` == "START") %>%  # 只保留 START，計算 START 到 STOP 的時間差
  mutate(Duration = Next_Time - Time) %>%
  group_by(Behavior) %>%  # 按行為類型分組
  summarise(Total_Duration = sum(Duration, na.rm = TRUE))  # 計算總持續時間

# 合併次數與持續時間統計
behavior_summary <- behavior_counts %>%
  left_join(behavior_durations, by = "Behavior")

# Duration mean (s)
duration <- df %>%
  filter(`Behavior type` %in% c("START", "STOP")) %>%
  arrange(Behavior, Time) %>%
  group_by(Behavior) %>%
  mutate(event_group = cumsum(`Behavior type` == "START")) %>%
  pivot_wider(names_from = `Behavior type`, values_from = Time) %>%
  filter(!is.na(START) & !is.na(STOP)) %>%
  mutate(duration = STOP - START) %>%
  ungroup() %>%
  mutate(event_type = "duration")


# 未完成
# Duration std dev
# inter-event intervals mean (s)
# inter-event intervals std dev
# % of total length