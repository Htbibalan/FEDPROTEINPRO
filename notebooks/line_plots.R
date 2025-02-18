
#################################################################################################
#######################################################################################################################
#########################################################PELLET DYNAMICS ##############################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################

###############################################################################
## 1) Load Required Packages
###############################################################################
# install.packages("tidyr")       # if not installed
# install.packages("dplyr")       # if not installed
# install.packages("stringr")     # if not installed
# install.packages("lmerTest")    # if not installed
# install.packages("emmeans")     # if not installed

library(tidyr)
library(dplyr)
library(stringr)
library(lmerTest)
library(emmeans)

###############################################################################
## 2) Read the Wide-Format CSV
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/pellets/aligned_pellets_per_day.csv"
df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

###############################################################################
## 3) Convert Wide -> Long
###############################################################################
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "pellets"
  ) %>%
  mutate(day = as.integer(str_remove(day, "day_")))

###############################################################################
## 4) Exclude training days (day 0, day 1)
###############################################################################
df_long <- df_long %>% filter(day >= 2)

###############################################################################
## 5) Define Epoch & day_in_epoch
##    - Then convert day_in_epoch into a FACTOR
###############################################################################
df_long <- df_long %>%
  mutate(
    epoch = case_when(
      day >=  2 & day <=  8  ~ "E1",
      day >=  9 & day <= 15  ~ "E2",
      day >= 16 & day <= 22  ~ "E3",
      day >= 23 & day <= 29  ~ "E4",
      TRUE ~ NA_character_
    ),
    epoch_start = case_when(
      epoch == "E1" ~ 2,
      epoch == "E2" ~ 9,
      epoch == "E3" ~ 16,
      epoch == "E4" ~ 23,
      TRUE          ~ NA_real_
    ),
    day_in_epoch = day - epoch_start + 1,
    epoch = factor(epoch, levels = c("E1", "E2", "E3", "E4"))
  ) %>%
  # Make day_in_epoch a factor for day-by-day comparisons
  mutate(day_in_epoch_factor = factor(day_in_epoch, levels = 1:7))

###############################################################################
## 6) Some Descriptive Statistics (optional)
###############################################################################
desc_by_epoch_day <- df_long %>%
  group_by(epoch, day_in_epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(pellets),
    sd     = sd(pellets),
    median = median(pellets),
    min    = min(pellets),
    max    = max(pellets),
    .groups = "drop"
  )

###############################################################################
## 7) Fit LMER Model with 'epoch' * factor(day_in_epoch_factor)
##    Random: (day_in_epoch_factor | mouse_id) won't generally work, as factor
##    slopes are tricky. So, let's use random intercept for mouse_id only
##    if you specifically want day-by-day random slopes, you'd keep day_in_epoch
##    numeric. But then you lose the discrete day post-hoc.
###############################################################################
model_lmer <- lmer(
  pellets ~ epoch * day_in_epoch_factor +
    (1 | mouse_id),
  data = df_long
)

###############################################################################
## 8) ANOVA (Type III) + Model Summary
###############################################################################
model_anova   <- anova(model_lmer, type = 3)
model_summary <- summary(model_lmer)

###############################################################################
## 9) Post-Hoc Comparisons
##    (A) Among epochs (averaged over day_in_epoch_factor)
##    (B) Among days within each epoch
###############################################################################
# A) Compare E1/E2/E3/E4 ignoring day_in_epoch differences
epoch_emm <- emmeans(model_lmer, ~ epoch)
epoch_pairs <- contrast(epoch_emm, method = "pairwise", adjust = "holm")

# B) Compare day=1..7 within each epoch
emm_epoch_day <- emmeans(model_lmer, ~ day_in_epoch_factor | epoch)
epoch_day_pairs <- contrast(emm_epoch_day, method = "pairwise", adjust = "holm")

###############################################################################
## 10) Save Results
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/pellets/"

### (A) Descriptives
write.csv(desc_by_epoch_day, file.path(out_dir, "desc_by_epoch_day.csv"), row.names = FALSE)

### (B) ANOVA
anova_table <- as.data.frame(model_anova)
anova_table$Effect <- rownames(model_anova)
rownames(anova_table) <- NULL
anova_table <- anova_table[, c(ncol(anova_table), 1:(ncol(anova_table)-1))]
write.csv(anova_table, file.path(out_dir, "mixed_model_ANOVA.csv"), row.names = FALSE)

### (C) Model summary as text
capture.output(model_summary, file = file.path(out_dir, "mixed_model_summary.txt"))

### (D) Post-hoc CSVs
epoch_pairs_df <- as.data.frame(epoch_pairs)
write.csv(epoch_pairs_df, file.path(out_dir, "posthoc_epoch_comparisons.csv"), row.names = FALSE)

epoch_day_pairs_df <- as.data.frame(epoch_day_pairs)
write.csv(epoch_day_pairs_df, file.path(out_dir, "posthoc_day_in_epoch_within_epoch.csv"), row.names = FALSE)

###############################################################################
## 11) Print Key Results to Console (Optional)
###############################################################################
cat("\n--- MODEL SUMMARY ---\n")
print(model_summary)

cat("\n--- ANOVA TABLE (TYPE III) ---\n")
print(model_anova)

cat("\n--- EPOCH PAIRWISE (HOLM) ---\n")
print(epoch_pairs)

cat("\n--- DAY_IN_EPOCH (FACTOR) WITHIN EACH EPOCH ---\n")
print(epoch_day_pairs)









#################################################################################################
#######################################################################################################################
#########################################################MEAL  DYNAMICS ##############################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################



library(tidyr)
library(dplyr)
library(stringr)
library(lmerTest)
library(emmeans)

###############################################################################
## 2) Read the Wide-Format CSV
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/meals/aligned_meals_per_day.csv"
df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

###############################################################################
## 3) Convert Wide -> Long
###############################################################################
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "pellets"
  ) %>%
  mutate(day = as.integer(str_remove(day, "day_")))

###############################################################################
## 4) Exclude training days (day 0, day 1)
###############################################################################
df_long <- df_long %>% filter(day >= 2)

###############################################################################
## 5) Define Epoch & day_in_epoch
##    - Then convert day_in_epoch into a FACTOR
###############################################################################
df_long <- df_long %>%
  mutate(
    epoch = case_when(
      day >=  2 & day <=  8  ~ "E1",
      day >=  9 & day <= 15  ~ "E2",
      day >= 16 & day <= 22  ~ "E3",
      day >= 23 & day <= 29  ~ "E4",
      TRUE ~ NA_character_
    ),
    epoch_start = case_when(
      epoch == "E1" ~ 2,
      epoch == "E2" ~ 9,
      epoch == "E3" ~ 16,
      epoch == "E4" ~ 23,
      TRUE          ~ NA_real_
    ),
    day_in_epoch = day - epoch_start + 1,
    epoch = factor(epoch, levels = c("E1", "E2", "E3", "E4"))
  ) %>%
  # Make day_in_epoch a factor for day-by-day comparisons
  mutate(day_in_epoch_factor = factor(day_in_epoch, levels = 1:7))

###############################################################################
## 6) Some Descriptive Statistics (optional)
###############################################################################
desc_by_epoch_day <- df_long %>%
  group_by(epoch, day_in_epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(pellets),
    sd     = sd(pellets),
    median = median(pellets),
    min    = min(pellets),
    max    = max(pellets),
    .groups = "drop"
  )

###############################################################################
## 7) Fit LMER Model with 'epoch' * factor(day_in_epoch_factor)
##    Random: (day_in_epoch_factor | mouse_id) won't generally work, as factor
##    slopes are tricky. So, let's use random intercept for mouse_id only
##    if you specifically want day-by-day random slopes, you'd keep day_in_epoch
##    numeric. But then you lose the discrete day post-hoc.
###############################################################################
model_lmer <- lmer(
  pellets ~ epoch * day_in_epoch_factor +
    (1 | mouse_id),
  data = df_long
)

###############################################################################
## 8) ANOVA (Type III) + Model Summary
###############################################################################
model_anova   <- anova(model_lmer, type = 3)
model_summary <- summary(model_lmer)

###############################################################################
## 9) Post-Hoc Comparisons
##    (A) Among epochs (averaged over day_in_epoch_factor)
##    (B) Among days within each epoch
###############################################################################
# A) Compare E1/E2/E3/E4 ignoring day_in_epoch differences
epoch_emm <- emmeans(model_lmer, ~ epoch)
epoch_pairs <- contrast(epoch_emm, method = "pairwise", adjust = "holm")

# B) Compare day=1..7 within each epoch
emm_epoch_day <- emmeans(model_lmer, ~ day_in_epoch_factor | epoch)
epoch_day_pairs <- contrast(emm_epoch_day, method = "pairwise", adjust = "holm")

###############################################################################
## 10) Save Results
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/meals/"

### (A) Descriptives
write.csv(desc_by_epoch_day, file.path(out_dir, "desc_by_epoch_day.csv"), row.names = FALSE)

### (B) ANOVA
anova_table <- as.data.frame(model_anova)
anova_table$Effect <- rownames(model_anova)
rownames(anova_table) <- NULL
anova_table <- anova_table[, c(ncol(anova_table), 1:(ncol(anova_table)-1))]
write.csv(anova_table, file.path(out_dir, "mixed_model_ANOVA.csv"), row.names = FALSE)

### (C) Model summary as text
capture.output(model_summary, file = file.path(out_dir, "mixed_model_summary.txt"))

### (D) Post-hoc CSVs
epoch_pairs_df <- as.data.frame(epoch_pairs)
write.csv(epoch_pairs_df, file.path(out_dir, "posthoc_epoch_comparisons.csv"), row.names = FALSE)

epoch_day_pairs_df <- as.data.frame(epoch_day_pairs)
write.csv(epoch_day_pairs_df, file.path(out_dir, "posthoc_day_in_epoch_within_epoch.csv"), row.names = FALSE)

###############################################################################
## 11) Print Key Results to Console (Optional)
###############################################################################
cat("\n--- MODEL SUMMARY ---\n")
print(model_summary)

cat("\n--- ANOVA TABLE (TYPE III) ---\n")
print(model_anova)

cat("\n--- EPOCH PAIRWISE (HOLM) ---\n")
print(epoch_pairs)

cat("\n--- DAY_IN_EPOCH (FACTOR) WITHIN EACH EPOCH ---\n")
print(epoch_day_pairs)








#################################################################################################
#######################################################################################################################
######################################################### SNACKS  DYNAMICS ##############################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################



library(tidyr)
library(dplyr)
library(stringr)
library(lmerTest)
library(emmeans)

###############################################################################
## 2) Read the Wide-Format CSV
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/snacks/aligned_snacks_per_day.csv"
df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

###############################################################################
## 3) Convert Wide -> Long
###############################################################################
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "pellets"
  ) %>%
  mutate(day = as.integer(str_remove(day, "day_")))

###############################################################################
## 4) Exclude training days (day 0, day 1)
###############################################################################
df_long <- df_long %>% filter(day >= 2)

###############################################################################
## 5) Define Epoch & day_in_epoch
##    - Then convert day_in_epoch into a FACTOR
###############################################################################
df_long <- df_long %>%
  mutate(
    epoch = case_when(
      day >=  2 & day <=  8  ~ "E1",
      day >=  9 & day <= 15  ~ "E2",
      day >= 16 & day <= 22  ~ "E3",
      day >= 23 & day <= 29  ~ "E4",
      TRUE ~ NA_character_
    ),
    epoch_start = case_when(
      epoch == "E1" ~ 2,
      epoch == "E2" ~ 9,
      epoch == "E3" ~ 16,
      epoch == "E4" ~ 23,
      TRUE          ~ NA_real_
    ),
    day_in_epoch = day - epoch_start + 1,
    epoch = factor(epoch, levels = c("E1", "E2", "E3", "E4"))
  ) %>%
  # Make day_in_epoch a factor for day-by-day comparisons
  mutate(day_in_epoch_factor = factor(day_in_epoch, levels = 1:7))

###############################################################################
## 6) Some Descriptive Statistics (optional)
###############################################################################
desc_by_epoch_day <- df_long %>%
  group_by(epoch, day_in_epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(pellets),
    sd     = sd(pellets),
    median = median(pellets),
    min    = min(pellets),
    max    = max(pellets),
    .groups = "drop"
  )

###############################################################################
## 7) Fit LMER Model with 'epoch' * factor(day_in_epoch_factor)
##    Random: (day_in_epoch_factor | mouse_id) won't generally work, as factor
##    slopes are tricky. So, let's use random intercept for mouse_id only
##    if you specifically want day-by-day random slopes, you'd keep day_in_epoch
##    numeric. But then you lose the discrete day post-hoc.
###############################################################################
model_lmer <- lmer(
  pellets ~ epoch * day_in_epoch_factor +
    (1 | mouse_id),
  data = df_long
)

###############################################################################
## 8) ANOVA (Type III) + Model Summary
###############################################################################
model_anova   <- anova(model_lmer, type = 3)
model_summary <- summary(model_lmer)

###############################################################################
## 9) Post-Hoc Comparisons
##    (A) Among epochs (averaged over day_in_epoch_factor)
##    (B) Among days within each epoch
###############################################################################
# A) Compare E1/E2/E3/E4 ignoring day_in_epoch differences
epoch_emm <- emmeans(model_lmer, ~ epoch)
epoch_pairs <- contrast(epoch_emm, method = "pairwise", adjust = "holm")

# B) Compare day=1..7 within each epoch
emm_epoch_day <- emmeans(model_lmer, ~ day_in_epoch_factor | epoch)
epoch_day_pairs <- contrast(emm_epoch_day, method = "pairwise", adjust = "holm")

###############################################################################
## 10) Save Results
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/snacks/"

### (A) Descriptives
write.csv(desc_by_epoch_day, file.path(out_dir, "desc_by_epoch_day.csv"), row.names = FALSE)

### (B) ANOVA
anova_table <- as.data.frame(model_anova)
anova_table$Effect <- rownames(model_anova)
rownames(anova_table) <- NULL
anova_table <- anova_table[, c(ncol(anova_table), 1:(ncol(anova_table)-1))]
write.csv(anova_table, file.path(out_dir, "mixed_model_ANOVA.csv"), row.names = FALSE)

### (C) Model summary as text
capture.output(model_summary, file = file.path(out_dir, "mixed_model_summary.txt"))

### (D) Post-hoc CSVs
epoch_pairs_df <- as.data.frame(epoch_pairs)
write.csv(epoch_pairs_df, file.path(out_dir, "posthoc_epoch_comparisons.csv"), row.names = FALSE)

epoch_day_pairs_df <- as.data.frame(epoch_day_pairs)
write.csv(epoch_day_pairs_df, file.path(out_dir, "posthoc_day_in_epoch_within_epoch.csv"), row.names = FALSE)

###############################################################################
## 11) Print Key Results to Console (Optional)
###############################################################################
cat("\n--- MODEL SUMMARY ---\n")
print(model_summary)

cat("\n--- ANOVA TABLE (TYPE III) ---\n")
print(model_anova)

cat("\n--- EPOCH PAIRWISE (HOLM) ---\n")
print(epoch_pairs)

cat("\n--- DAY_IN_EPOCH (FACTOR) WITHIN EACH EPOCH ---\n")
print(epoch_day_pairs)







#################################################################################################
#######################################################################################################################
######################################################### FEAST  DYNAMICS ##############################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################



library(tidyr)
library(dplyr)
library(stringr)
library(lmerTest)
library(emmeans)

###############################################################################
## 2) Read the Wide-Format CSV
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/feasts/aligned_mega_meal_per_day.csv"
df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

###############################################################################
## 3) Convert Wide -> Long
###############################################################################
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "pellets"
  ) %>%
  mutate(day = as.integer(str_remove(day, "day_")))

###############################################################################
## 4) Exclude training days (day 0, day 1)
###############################################################################
df_long <- df_long %>% filter(day >= 2)

###############################################################################
## 5) Define Epoch & day_in_epoch
##    - Then convert day_in_epoch into a FACTOR
###############################################################################
df_long <- df_long %>%
  mutate(
    epoch = case_when(
      day >=  2 & day <=  8  ~ "E1",
      day >=  9 & day <= 15  ~ "E2",
      day >= 16 & day <= 22  ~ "E3",
      day >= 23 & day <= 29  ~ "E4",
      TRUE ~ NA_character_
    ),
    epoch_start = case_when(
      epoch == "E1" ~ 2,
      epoch == "E2" ~ 9,
      epoch == "E3" ~ 16,
      epoch == "E4" ~ 23,
      TRUE          ~ NA_real_
    ),
    day_in_epoch = day - epoch_start + 1,
    epoch = factor(epoch, levels = c("E1", "E2", "E3", "E4"))
  ) %>%
  # Make day_in_epoch a factor for day-by-day comparisons
  mutate(day_in_epoch_factor = factor(day_in_epoch, levels = 1:7))

###############################################################################
## 6) Some Descriptive Statistics (optional)
###############################################################################
desc_by_epoch_day <- df_long %>%
  group_by(epoch, day_in_epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(pellets),
    sd     = sd(pellets),
    median = median(pellets),
    min    = min(pellets),
    max    = max(pellets),
    .groups = "drop"
  )

###############################################################################
## 7) Fit LMER Model with 'epoch' * factor(day_in_epoch_factor)
##    Random: (day_in_epoch_factor | mouse_id) won't generally work, as factor
##    slopes are tricky. So, let's use random intercept for mouse_id only
##    if you specifically want day-by-day random slopes, you'd keep day_in_epoch
##    numeric. But then you lose the discrete day post-hoc.
###############################################################################
model_lmer <- lmer(
  pellets ~ epoch * day_in_epoch_factor +
    (1 | mouse_id),
  data = df_long
)

###############################################################################
## 8) ANOVA (Type III) + Model Summary
###############################################################################
model_anova   <- anova(model_lmer, type = 3)
model_summary <- summary(model_lmer)

###############################################################################
## 9) Post-Hoc Comparisons
##    (A) Among epochs (averaged over day_in_epoch_factor)
##    (B) Among days within each epoch
###############################################################################
# A) Compare E1/E2/E3/E4 ignoring day_in_epoch differences
epoch_emm <- emmeans(model_lmer, ~ epoch)
epoch_pairs <- contrast(epoch_emm, method = "pairwise", adjust = "holm")

# B) Compare day=1..7 within each epoch
emm_epoch_day <- emmeans(model_lmer, ~ day_in_epoch_factor | epoch)
epoch_day_pairs <- contrast(emm_epoch_day, method = "pairwise", adjust = "holm")

###############################################################################
## 10) Save Results
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Line_plots/feasts/"

### (A) Descriptives
write.csv(desc_by_epoch_day, file.path(out_dir, "desc_by_epoch_day.csv"), row.names = FALSE)

### (B) ANOVA
anova_table <- as.data.frame(model_anova)
anova_table$Effect <- rownames(model_anova)
rownames(anova_table) <- NULL
anova_table <- anova_table[, c(ncol(anova_table), 1:(ncol(anova_table)-1))]
write.csv(anova_table, file.path(out_dir, "mixed_model_ANOVA.csv"), row.names = FALSE)

### (C) Model summary as text
capture.output(model_summary, file = file.path(out_dir, "mixed_model_summary.txt"))

### (D) Post-hoc CSVs
epoch_pairs_df <- as.data.frame(epoch_pairs)
write.csv(epoch_pairs_df, file.path(out_dir, "posthoc_epoch_comparisons.csv"), row.names = FALSE)

epoch_day_pairs_df <- as.data.frame(epoch_day_pairs)
write.csv(epoch_day_pairs_df, file.path(out_dir, "posthoc_day_in_epoch_within_epoch.csv"), row.names = FALSE)

###############################################################################
## 11) Print Key Results to Console (Optional)
###############################################################################
cat("\n--- MODEL SUMMARY ---\n")
print(model_summary)

cat("\n--- ANOVA TABLE (TYPE III) ---\n")
print(model_anova)

cat("\n--- EPOCH PAIRWISE (HOLM) ---\n")
print(epoch_pairs)

cat("\n--- DAY_IN_EPOCH (FACTOR) WITHIN EACH EPOCH ---\n")
print(epoch_day_pairs)



