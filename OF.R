library (readxl) #需要先library("readxl"), 才可用read_excel讀取 excel 檔
library (tidyverse) 
library (lawstat)

# 學如何將不同的excel併在一起
OF_raw_data_1 <- read_excel ("D:\\碩士資料庫\\高田研究\\personality test\\Open field test\\OF excel from Boris\\OF_1.xlsx")
OF_raw_data_2 <- read_excel ("D:\\碩士資料庫\\高田研究\\personality test\\Open field test\\OF excel from Boris\\OF_2.xlsx")

OF_1 <- OF_raw_data_1[, c("Observation id", "Subject", "Behavior", "Behavior type", "Time")] %>%
  filter (Behavior != "Start" & Behavior != "Stop") %>%
  select ("Observation id", Subject, Behavior, Time) %>% 
  mutate (Time = as.numeric (Time)) %>%
  arrange ("Observation id", Subject, Behavior) %>%
  pivot_wider (id_cols = c("Observation id", Subject), names_from = Behavior, values_from = Time, values_fn = list)

OF_2 <- OF_raw_data_2[, c("Observation id", "Subject", "Behavior", "Behavior type", "Time")] %>%
  filter (Behavior != "Start" & Behavior != "Stop") %>%
  select ("Observation id", Subject, Behavior, Time) %>% 
  mutate (Time = as.numeric (Time)) %>%
  arrange ("Observation id", Subject, Behavior) %>%
  pivot_wider (id_cols = c("Observation id", Subject), names_from = Behavior, values_from = Time, values_fn = list)

Period <- function (cell){
  c.sub <- c(0, 0)
  ifelse (is.null (cell), 0, {
    for (i in 1:(length (cell) / 2)){ c.sub[i] <- cell[i * 2] - cell[i * 2 - 1]}
    c.sum <- round (sum (c.sub), digits = 3)
    return(c.sum)}
  )}  

Subject_1 <- c("59009", "59706", "59712", "59726", "59776")
Date_1 <- rep (c(20240112, 20240114, 20240116, 20240118), each = 5)

Subject_2 <- c("59715", "59722", "59701", "59727", "59709", "59704")
Date_2 <- rep (c(20240216, 20240218, 20240220, 20240222), each = 6)
Name <- c("Date", "Subject", "Outside", "Inside", "Corner")

OF_df_1 <- Date_1 %>%
  cbind (Subject_1) %>% 
  cbind (unlist (lapply (OF_1$Outside, Period))) %>% # Before using the function, Repeatability, data need converting to un-list data
  cbind (unlist (lapply (OF_1$Inside, Period))) %>%
  cbind (unlist (lapply (OF_1$Corner, Period))) %>%
  `colnames<-`(Name) %>% 
  as.data.frame (.)# Before using the function, Repeatability, data need converting to a data frame.

OF_df_2 <- Date_2 %>%
  cbind (Subject_2) %>% 
  cbind (unlist (lapply (OF_2$Outside, Period))) %>% # Before using the function, Repeatability, data need converting to un-list data
  cbind (unlist (lapply (OF_2$Inside, Period))) %>%
  cbind (unlist (lapply (OF_2$Corner, Period))) %>%
  `colnames<-`(Name) %>% 
  as.data.frame (.)

OF_df <- rbind (OF_df_1, OF_df_2)

# Create a function, Repeatability, to calculate repeatability of each behavior.
Repeatability <- function (Y, treatment, Data){
  data_1 <- lm (Y ~ treatment, data = Data)
  a <- anova (data_1)$"Mean Sq"
  b <- a[1] / (a[1] + a[2])
  return(b)}  

Name_1 <- c("Stay in outside", "Stay in inside", "Stay in corner", "Path length")

OF_path_raw_data <- read_excel ("D:\\碩士資料庫\\高田研究\\personality test\\Open field test\\Path length result\\Path length result.xlsx")

OF_Rep <- Repeatability (Y = OF_df$Outside, treatment = OF_df$Subject, Data = OF_df) %>% 
  cbind (Repeatability (Y = OF_df$Inside, treatment = OF_df$Subject, Data = OF_df)) %>% 
  cbind (Repeatability (Y = OF_df$Corner, treatment = OF_df$Subject, Data = OF_df)) %>% 
  cbind (Repeatability (Y = OF_path_raw_data$`Path length (m)`, treatment = OF_path_raw_data$Subject, Data = OF_path_raw_data)) %>%  
  `colnames<-` (Name_1) %>% 
  `row.names<-` ("Repeatability")
