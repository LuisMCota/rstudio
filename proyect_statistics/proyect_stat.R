library(dplyr)
censo <- read.csv("INE_ENTIDAD_2020.csv",sep =",", dec = ",", header = T, encoding = "UTF-8")
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}
total <- censo %>% 
  select(ENT, NOM_ENT, P3A5_NOA, P6A11_NOA, P12A14NOA, P_3A5, P_6A11, P_12A14)%>%
  rename(STATE = NOM_ENT, Age_3_5 = P3A5_NOA, Age_6_11 = P6A11_NOA, Age_12_14 = P12A14NOA, Age_3_5_ = P_3A5, Age_6_11_ = P_6A11, Age_12_14_ = P_12A14)%>%
  mutate(Total_no_study = Age_3_5 + Age_6_11 + Age_12_14) %>%
  mutate(Total_population = Age_3_5_ + Age_6_11_ + Age_12_14_) %>%
  select(ENT, STATE, Total_no_study, Total_population) %>%
  mutate(Total_study = Total_population - Total_no_study)

Total <- total %>%
  select_if(is.numeric) %>%
  colSums()

Total <- bind_rows(total, Total)
Total[(dim(Total)[1]), 1] = "$"
Total[(dim(Total)[1]), 2] = "Total"

rm(censo)

results <- data.frame(
  "Measures" = c("Mean", "Median", "Mode", "Variance", "Standard Deviation"),
  "Total_study" = round(c(mean(total$Total_study), median(total$Total_study), mode(total$Total_study), var(total$Total_study), sd(total$Total_study))),
  "Total_no_study" = round(c(mean(total$Total_no_study), median(total$Total_no_study), mode(total$Total_no_study), var(total$Total_no_study), sd(total$Total_no_study))),
  "Total_population"= round(c(mean(total$Total_population), median(total$Total_population), mode(total$Total_population), var(total$Total_population), sd(total$Total_population))))



