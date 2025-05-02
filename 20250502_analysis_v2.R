library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)

Info <- read_csv("20250331高田前測進度.csv")

# Time budget from BORIS
Trial10_Exp <- read_tsv("trial-10/20241217_trial-10_CL2-23_R_Time_budget.tsv")
Trial10_Ctr <- read_tsv("trial-10/20241217_trial-10_CL2-23_L_Time_budget.tsv")

