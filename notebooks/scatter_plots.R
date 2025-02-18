
#############################################################################################
#############################################################################################
#############################################################################################

###############################################################################
## 1) Load Required Packages
###############################################################################
# install.packages("tidyr")     # Uncomment if not installed
# install.packages("dplyr")     # Uncomment if not installed
# install.packages("broom")     # Uncomment if not installed

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/pellets/order2_number_of_pellets.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "pellets"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(pellets),
    sd     = sd(pellets),
    median = median(pellets),
    min    = min(pellets),
    max    = max(pellets)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(pellets ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$pellets,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/pellets/"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)





##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################MEAL MEAL MEAL MEAL MEAL#################################################################
##############################################################################################################################
#####################################################################MEAL NUMBER #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/number/order2_number_of_meals.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/number"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################MEAL MEAL MEAL MEAL MEAL#################################################################
##############################################################################################################################
##################################################################### MEAL FREQUECNY #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/freq/order2_meal_frequency.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/freq"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)



##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################MEAL MEAL MEAL MEAL MEAL#################################################################
##############################################################################################################################
##################################################################### MEAL SIZE #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/size/order2_meal_size.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/meal/size"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)






##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################FEAST FEAST FEAST FEAST FEAST #################################################################
##############################################################################################################################
##################################################################### FEAST NUMBER #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/number/order2_number_of_mega_meals.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/number"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)





##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################FEAST FEAST FEAST FEAST FEAST #################################################################
##############################################################################################################################
##################################################################### FEAST freq #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/freq/order2_mega_meal_frequency.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/freq"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)






##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################FEAST FEAST FEAST FEAST FEAST #################################################################
##############################################################################################################################
##################################################################### FEAST SIZE #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/size/order2_mega_meal_size.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/feast/size"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)






##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################SNACK SNACK SNACK SNACK #################################################################
##############################################################################################################################
##################################################################### SNACK NUMBER #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/snack/number/order2_number_of_snacks.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/snack/number"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)






##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############################################################SNACK SNACK SNACK SNACK #################################################################
##############################################################################################################################
##################################################################### SNACK FREQUENCY #########################################################

library(tidyr)
library(dplyr)
library(broom)

###############################################################################
## 2) Read & Reshape Your Data
###############################################################################
data_file <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/snack/freq/order2_snack_frequency.csv"

df_wide <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Convert from wide to long format:
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("epoch_"),
    names_to = "epoch",
    values_to = "meals"
  )

###############################################################################
## 3) Descriptive Statistics per Epoch
###############################################################################
descriptive_stats <- df_long %>%
  group_by(epoch) %>%
  summarize(
    n      = n(),
    mean   = mean(meals),
    sd     = sd(meals),
    median = median(meals),
    min    = min(meals),
    max    = max(meals)
  )

###############################################################################
## 4) Repeated-Measures ANOVA (Traditional aov)
###############################################################################
# Model: pellets ~ epoch + Error(mouse_id/epoch)
anova_model <- aov(meals ~ epoch + Error(mouse_id/epoch), data = df_long)
anova_summary <- summary(anova_model)

# The "summary(anova_model)" for repeated-measures typically returns 
# a list with multiple error strata. For example:
#   anova_summary[[1]] => Error: mouse_id
#   anova_summary[[2]] => Error: mouse_id:epoch (contains the epoch effect)

# Let's extract the second stratum (the within-subject factor "epoch").
# Each stratum is itself a list; the actual ANOVA table is anova_summary[[2]][[1]].
anova_stratum2 <- anova_summary[[2]][[1]]   # This is a matrix-like "anova" object.

# Convert that matrix-like object into a data frame
anova_table <- as.data.frame(anova_stratum2)

# Create a new column for the row names (epoch, Residuals)
anova_table$Effect <- rownames(anova_stratum2)
rownames(anova_table) <- NULL

# Reorder columns so 'Effect' appears first
anova_table <- anova_table %>%
  relocate(Effect)

###############################################################################
## 5) Post-Hoc (Pairwise) Tests with Holm Correction
###############################################################################
posthoc_result <- pairwise.t.test(
  x = df_long$meals,
  g = df_long$epoch,
  paired = TRUE,
  p.adjust.method = "holm"
)

# Convert the p-value matrix into a tidy data frame
posthoc_pvals <- as.data.frame(as.table(posthoc_result$p.value))
colnames(posthoc_pvals) <- c("Group1", "Group2", "p_value")

###############################################################################
## 6) Save All Results as CSV in the Same Folder
###############################################################################
out_dir <- "C:/Users/hta031/Github/FEDPROTEINPRO/stats/Scatter_plots/snack/freq"

## (a) Descriptive stats
write.csv(
  descriptive_stats,
  file.path(out_dir, "descriptive_stats.csv"),
  row.names = FALSE
)

## (b) ANOVA table (epoch effect)
#   This is the repeated-measures ANOVA for the within-subject factor 'epoch'.
write.csv(
  anova_table,
  file.path(out_dir, "anova_results.csv"),
  row.names = FALSE
)

## (c) Post-Hoc pairwise comparisons (Holm-corrected p-values)
write.csv(
  posthoc_pvals,
  file.path(out_dir, "posthoc_holm_results.csv"),
  row.names = FALSE
)

###############################################################################
## 7) Print to Console (Optional)
###############################################################################
cat("\n-- Descriptive Statistics --\n")
print(descriptive_stats)

cat("\n-- ANOVA Summary (Raw) --\n")
print(anova_summary)

cat("\n-- ANOVA Table for 'epoch' (Parsed) --\n")
print(anova_table)

cat("\n-- Post-Hoc Holm Results (Raw) --\n")
print(posthoc_result)

cat("\n-- Post-Hoc P-value Table --\n")
print(posthoc_pvals)