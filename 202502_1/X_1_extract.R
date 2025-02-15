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
temp <- mods[grep("mixedPred.*panas_nega.*", mods, ignore.case = TRUE)]
mod_645 <- readRDS(paste0(mod_path,'/', temp))
i <- 1
for (ele in mod_645$panas_nega){
  print(names(mod_645$panas_nega[i]))
  print(unname(ele$results$estimate))
  print(ele$results$p.value)
  i <- i + 1
}

temp <- mods[grep("mixedPred.*panas_posi.*", mods, ignore.case = TRUE)]
mod_645 <- readRDS(paste0(mod_path,'/', temp))
i <- 1
for (ele in mod_645$panas_posi){
  print(names(mod_645$panas_posi[i]))
  print(unname(ele$results$estimate))
  print(ele$results$p.value)
  i <- i + 1
}

# ---- 2. conditions ----
temp <- mods[grep("mixedPred.*con3.*", mods, ignore.case = TRUE)]
mod_645 <- readRDS(paste0(mod_path,'/', temp))
i <- 1
for (ele in mod_645$con3){
  print(names(mod_645$con3[i][1]))
  print(unname(unlist(ele$results_metrics[8,3]))) # AUC
  print(cal_heldout_p(
    ele$predictions$truth,
    ele$predictions$estimate
  )$p.value)
  i <- i + 1
}

temp <- mods[grep("mixedPred.*churchmall.*", mods, ignore.case = TRUE)]
mod_645 <- readRDS(paste0(mod_path,'/', temp))
i <- 1
for (ele in mod_645$churchmall){
  print(names(mod_645$churchmall[i][1]))
  print(unname(unlist(ele$results_metrics[8,3]))) # AUC
  print(cal_heldout_p(
    ele$predictions$truth,
    ele$predictions$estimate
  )$p.value)
  i <- i + 1
}


q()
# ---- 2. stack ----
#stack_affect_577 <- mods[grep("stack_.*affect.*", mods, ignore.case = TRUE)]
NULL

# ---- 3. classify ----
churchmallpark_affect_577 <- mods[grep("affect.*churchmallpark.*", mods, ignore.case = TRUE)]
churchmallpark_affect_577
for (mod in churchmallpark_affect_577){
  temp <- readRDS(paste0(mod_path,'/',mod))
  print(temp$results_metrics$.estimate[8])
  print(cal_heldout_p(temp$predictions$truth, temp$predictions$estimate))
}



