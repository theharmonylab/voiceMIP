rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
library(furrr)
library(text)


# ---- 0.1 voice dat input ----
df <- readRDS('demo_talk/out/emb/script_2_1_df.rds')


# ---- 1.1 pred ----
# panas nega
if (FALSE){
  output_mods <- list()
  output_mods$panas_nega <- list()
  
  output_mods$panas_nega$post_phraseEmbMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postPhrase")], .name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_nega$post_affectAudioTranscriptMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postAffectTranscript")], .name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_nega$post_voiceWhisper_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1280, "_postVoice")],.name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_nega$post_transcriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
              df[,paste0("Dim",1:1024, "_postAffectTranscript")],
              df[,paste0("Dim",1:1280, "_postVoice")]
              ),.name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_nega$post_phraseTranscriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postPhrase")],
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  saveRDS(output_mods,
          'demo_talk/out/mods/mods_mixedPred_pred_panas_nega.rds')
}

# panas posi
if (FALSE){
  output_mods <- list()
  output_mods$panas_posi <- list()
  
  output_mods$panas_posi$post_phraseEmbMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postPhrase")], .name_repair='minimal'),
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_posi$post_affectAudioTranscriptMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postAffectTranscript")], .name_repair='minimal'),
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_posi$post_voiceWhisper_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1280, "_postVoice")],.name_repair='minimal'),
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_posi$post_transcriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  output_mods$panas_posi$post_phraseTranscriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postPhrase")],
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  
  saveRDS(output_mods,
          'demo_talk/out/mods/mods_mixedPred_pred_panas_posi.rds')
}


# classify 3 conditions
if (FALSE){
  output_mods <- list()
  output_mods$con3 <- list()
  
  output_mods$con3$post_phraseEmbMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postPhrase")], .name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$con3$post_affectAudioTranscriptMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postAffectTranscript")], .name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$con3$post_voiceWhisper_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1280, "_postVoice")],.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$con3$post_transcriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$con3$post_phraseTranscriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postPhrase")],
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  saveRDS(output_mods,
          'demo_talk/out/mods/mods_mixedPred_pred_con3.rds')
}


# classify church/mall
if (FALSE){
  df <- dplyr::filter(df, pPlace != 'park')
  
  output_mods <- list()
  output_mods$churchmall <- list()
  
  output_mods$churchmall$post_phraseEmbMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postPhrase")], .name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "logistic",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$churchmall$post_affectAudioTranscriptMixB_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1024, "_postAffectTranscript")], .name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "logistic",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$churchmall$post_voiceWhisper_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1280, "_postVoice")],.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "logistic",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$churchmall$post_transcriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "logistic",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  output_mods$churchmall$post_phraseTranscriptVoice_mod <- textTrainRegression(
    x = tibble::as_tibble(cbind(
      df[,paste0("Dim",1:1024, "_postPhrase")],
      df[,paste0("Dim",1:1024, "_postAffectTranscript")],
      df[,paste0("Dim",1:1280, "_postVoice")]
    ),.name_repair='minimal'),
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "logistic",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  
  saveRDS(output_mods,
          'demo_talk/out/mods/mods_mixedPred_pred_churchmall.rds')
}


# ---- 1.3 test the classification sig ----
classify_mods <- readRDS('demo_talk/out/mods/mods_mixedPred_pred_con3.rds')

# For AUC, one should input probabilities.
cal_heldout_p <- function(truth, estimate){
  # Create a contingency table
  contingency_table <- table(truth,estimate)
  # Perform Chi-Squared Test
  return(fisher.test(contingency_table, workspace = 2e8))
}
# Usage
cal_heldout_p(classify_mods[["con3"]][["post_phraseEmbMixB_mod"]]$predictions$truth,
              classify_mods[["con3"]][["post_phraseEmbMixB_mod"]]$predictions$estimate)
cal_heldout_p(classify_mods[["con3"]]$post_affectAudioTranscriptMixB_mod$predictions$truth,
              classify_mods[["con3"]]$post_affectAudioTranscriptMixB_mod$predictions$estimate)
cal_heldout_p(classify_mods[["con3"]]$post_voiceWhisper_mod$predictions$truth,
              classify_mods[["con3"]]$post_voiceWhisper_mod$predictions$estimate)
cal_heldout_p(classify_mods[["con3"]]$post_transcriptVoice_mod$predictions$truth,
              classify_mods[["con3"]]$post_transcriptVoice_mod$predictions$estimate)
cal_heldout_p(classify_mods[["con3"]]$post_phraseTranscriptVoice_mod$predictions$truth,
              classify_mods[["con3"]]$post_phraseTranscriptVoice_mod$predictions$estimate)

# ---- 2.0 BERT predict ----
if (FALSE){
  df <- readRDS('demo_talk/out/emb/script_2_1_df.rds')
  
  aaa <- textEmbed(df$pPostPhrase_all,keep_token_embeddings = FALSE, device='mps')
  test1 <- textTrainRegression(
    aaa$texts$texts,
    y = df$PANAS_post_positive
  )
  test1$results$estimate
  # .5839
  test2 <- textTrainRegression(
    aaa$texts$texts,
    y = df$PANAS_post_negative
  )
  test2$results$estimate
  # .5387
}
