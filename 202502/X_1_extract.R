gc()
library(tidyverse)
mod_path <- 'demo_talk/out/mods'
mods <- list.files(mod_path)

# For AUC, one should input probabilities.
cal_heldout_p <- function(truth, estimate){
  # Create a contingency table
  contingency_table <- table(truth,estimate)
  # Perform Chi-Squared Test
  return(fisher.test(contingency_table, workspace = 2e8))
}
# Usage
# cal_heldout_p(pre_affectAudio_feature_mod_pred_churchmallpark$predictions$truth,
#               pre_affectAudio_feature_mod_pred_churchmallpark$predictions$estimate)


# ---- 1. pre, post ----
temp <- mods[grep("post_.*affect.*", mods, ignore.case = TRUE)]
post_affect_577 <- temp[!grepl("stack", temp, ignore.case = TRUE)]
post_affect_577
for (mod in post_affect_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

temp <- mods[grep("post_.*neutral.*", mods, ignore.case = TRUE)]
post_neutral_577 <- temp[!grepl("stack", temp, ignore.case = TRUE)]
post_neutral_577
for (mod in post_neutral_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

temp <- mods[grep("pre_.*affect.*", mods, ignore.case = TRUE)]
pre_affect_577 <- temp[!grepl("stack", temp, ignore.case = TRUE)]
pre_affect_577
for (mod in pre_affect_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

temp <- mods[grep("pre_.*neutral.*", mods, ignore.case = TRUE)]
pre_neutral_577 <- temp[!grepl("stack", temp, ignore.case = TRUE)]
pre_neutral_577
for (mod in pre_neutral_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

# ---- 2. stack ----
stack_affect_577 <- mods[grep("stack_.*affect.*", mods, ignore.case = TRUE)]
stack_affect_577
for (mod in stack_affect_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

stack_neutral_577 <- mods[grep("stack_.*neutral.*", mods, ignore.case = TRUE)]
stack_neutral_577
for (mod in stack_neutral_577){
  if (length(grep('churchmallpark', mod)==1)){next}
  print(readRDS(paste0(mod_path,'/',mod))$results$estimate)
  print(readRDS(paste0(mod_path,'/',mod))$results$p.value)
}

# ---- 3. classify ----
churchmallpark_affect_577 <- mods[grep("affect.*churchmallpark.*", mods, ignore.case = TRUE)]
churchmallpark_affect_577
for (mod in churchmallpark_affect_577){
  temp <- readRDS(paste0(mod_path,'/',mod))
  print(temp$results_metrics$.estimate[8])
  print(cal_heldout_p(temp$predictions$truth, temp$predictions$estimate))
}

churchmallpark_neutral_577 <- mods[grep("neutral.*churchmallpark.*", mods, ignore.case = TRUE)]
churchmallpark_neutral_577
for (mod in churchmallpark_neutral_577){
  temp <- readRDS(paste0(mod_path,'/',mod))
  print(temp$results_metrics$.estimate[8])
  print(cal_heldout_p(temp$predictions$truth, temp$predictions$estimate))
}


