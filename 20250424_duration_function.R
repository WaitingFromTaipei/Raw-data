library(readr)
library(dplyr)

Trial1_Exp <- read_tsv("trial-1/20241010_trial-1_yellow_R.tsv")
Trial1_Ctr <- read_tsv("trial-1/20241010_trial-1_yellow_L.tsv")

# 高田碰觸時間比較

# Trial 1 實驗組
Tr1_Exp_1 <- Trial1_Exp %>% filter(Behavior=="Right cage") %>%
  select(`Behavior type`,Time)

Tr1_Exp_Start <- Tr1_Exp_1 %>% filter(`Behavior type`=="START") %>% rename(Start = Time)
Tr1_Exp_Stop <- Tr1_Exp_1 %>% filter(`Behavior type`=="STOP") %>% rename(Stop = Time)
Dura_Tr1_Exp <- Tr1_Exp_Start %>% select(Start) %>% mutate(Stop = Tr1_Exp_Stop$Stop) %>% mutate(Duration = Stop-Start)

# Trial 1 對照組
Tr1_Ctr_1 <- Trial1_Ctr %>% filter(Behavior=="Left cage") %>%
  select(`Behavior type`,Time)

Tr1_Ctr_Start <- Tr1_Ctr_1 %>% filter(`Behavior type`=="START") %>% rename(Start = Time)
Tr1_Ctr_Stop <- Tr1_Ctr_1 %>% filter(`Behavior type`=="STOP") %>% rename(Stop = Time)
Dura_Tr1_Ctr <- Tr1_Ctr_Start %>% select(Start) %>% mutate(Stop = Tr1_Ctr_Stop$Stop) %>% mutate(Duration = Stop-Start)

# 改寫成函式
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
