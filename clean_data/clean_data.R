library(tidyverse)
library(tidyr)
library(magrittr)

df_count <- readxl::read_xlsx("clean_data/18-140 T Cell Flow Summary Table - VM_20200824.xlsx", 
                              sheet = "Gated Cell Counts")%>%
  janitor::clean_names()
colnames(df_count) <- gsub("_cells$", "", colnames(df_count))
colnames(df_count) <- gsub("_cells", "", colnames(df_count))
colnames(df_count) <- gsub("percent", "count", colnames(df_count))

df_pec <- readxl::read_xlsx("clean_data/18-140 T Cell Flow Summary Table - VM_20200824.xlsx", 
                            sheet = "Results Summary", skip = 1, n_max = 436) %>%
  janitor::clean_names()

colnames(df_pec)


df_pec  %>%
  fill(colnames(.), .direction = "down") %>%
  fill(colnames(.), .direction = "up") -> df_pec

nrow(df_count) == nrow(df_pec)

df <- merge.data.frame(df_pec, 
                 df_count, 
                 by.x = "staining_sample_id_msk_trial_protocol_18_140", 
                 by.y = "sample_id")

write.csv(df_pec, "clean_data/cleaned_pec.csv", row.names = F)  
write.csv(df_count, "clean_data/cleaned_count.csv", row.names = F)  
write.csv(df, "clean_data/cleaned.csv", row.names = F)  
