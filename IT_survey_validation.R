# validating IT Survey Data (preferred help question)

library(dplyr)
library(readxl)
setwd("C:/Users/Hunter/OneDrive/Documents/Datasets")
path <- "C:/Users/Hunter/OneDrive/Documents/Datasets/Fall 2020 IT Services Survey for Miami University_November 10, 2020_16.32.xlsx"
survey <- read_xlsx(path, sheet = 1)

survey_trim <- survey %>% select("Help Channel Chat",
                                "Help Channel Phone",
                                "Help Channel Email",
                                "Help Channel SMS/Text Messaging",
                                "Help Channel Social Media",
                                "Help Channel Walk-in Location",
                                )

survey_trim <- na.omit(survey_trim)

sum(survey_trim$`Help Channel Chat` > 6 || 
      survey_trim$`Help Channel Chat` < 1) # 0, good

sum(survey_trim$`Help Channel Phone` > 6 || 
      survey_trim$`Help Channel Phone` < 1) # 0, good

sum(survey_trim$`Help Channel Email` > 6 || 
      survey_trim$`Help Channel Email` < 1) # 0, good

sum(survey_trim$`Help Channel SMS/Text Messaging` > 6 || 
      survey_trim$`Help Channel SMS/Text Messaging` < 1) # 0, good

sum(survey_trim$`Help Channel Social Media` > 6 || 
      survey_trim$`Help Channel Social Media` < 1) # 0, good

sum(survey_trim$`Help Channel Walk-in Location` > 6 || 
      survey_trim$`Help Channel Walk-in Location` < 1) # 0, good

t_vec <- rep.int(0, nrow(survey_trim))

for(row in (1:nrow(survey_trim))){
  t_vec[row] <- 
    (survey_trim$`Help Channel Chat`[row] +
    survey_trim$`Help Channel Phone`[row] +
    survey_trim$`Help Channel Email`[row] +
    survey_trim$`Help Channel SMS/Text Messaging`[row] +
    survey_trim$`Help Channel Social Media`[row] +
    survey_trim$`Help Channel Walk-in Location`[row])
  
}

sum(t_vec == 21) / length(t_vec) # all of the rows have scores that add to 21

  