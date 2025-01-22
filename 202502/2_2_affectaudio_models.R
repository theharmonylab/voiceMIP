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
df <- readRDS('demo_talk/out/emb/2_1_df.rds')

# ---- 1.1 pred ----
# panas nega
if (FALSE){
  post_affectAudioFeature_mod <- textTrainRegression(
    x = tibble::as_tibble(df[,paste0("Dim",1:1280, "_postVoice")], .name_repair='minimal'),
    y = df[,'PANAS_post_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(post_affectAudioFeature_mod,
          'demo_talk/out/mods/post_affectAudio_feature_mod_pred_post_nega_panas.rds')
  pre_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_preVoice")],
    y = df[,'PANAS_pre_negative'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(pre_affectAudioFeature_mod,
          'demo_talk/out/mods/pre_affectAudio_feature_mod_pred_pre_nega_panas.rds')
}

# panas posi
if (FALSE){
  post_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_postVoice")],
    y = df[,'PANAS_post_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(post_affectAudioFeature_mod,
          'demo_talk/out/mods/post_affectAudio_feature_mod_pred_post_posi_panas.rds')
  pre_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_preVoice")],
    y = df[,'PANAS_pre_positive'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(pre_affectAudioFeature_mod,
          'demo_talk/out/mods/pre_affectAudio_feature_mod_pred_pre_posi_panas.rds')
}

# pred valence
if (FALSE){
  post_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_postVoice")],
    y = df[,'post_pred_valence'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(post_affectAudioFeature_mod,
          'demo_talk/out/mods/post_affectAudio_feature_mod_pred_post_valence.rds')
  pre_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_preVoice")],
    y = df[,'pre_pred_valence'],
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(pre_affectAudioFeature_mod,
          'demo_talk/out/mods/pre_affectAudio_feature_mod_pred_pre_valence.rds')
}

# prepost similarity
if (TRUE){
  df$affectaudio_cos_sim_prepost <- textSimilarity(
    df[,paste0('Dim', 1:1280, '_postVoice')],
    df[,paste0('Dim', 1:1280, '_preVoice')])
  cor.test(df$pre_posi_panas,df$affectaudio_cos_sim_prepost)
  cor.test(df$pre_nega_panas,df$affectaudio_cos_sim_prepost)
  cor.test(df$pre_pred_valence,df$affectaudio_cos_sim_prepost)
  cor.test(df$post_posi_panas,df$affectaudio_cos_sim_prepost)
  cor.test(df$post_nega_panas,df$affectaudio_cos_sim_prepost)
  cor.test(df$post_pred_valence,df$affectaudio_cos_sim_prepost)
  df$neutralaudio_cos_sim_prepost <- textSimilarity(
    df[,paste0('Dim', 1:1280, '_postNeutral')],
    df[,paste0('Dim', 1:1280, '_preNeutral')])
  cor.test(df$pre_posi_panas,df$neutralaudio_cos_sim_prepost)
  cor.test(df$pre_nega_panas,df$neutralaudio_cos_sim_prepost)
  cor.test(df$pre_pred_valence,df$neutralaudio_cos_sim_prepost)
  cor.test(df$post_posi_panas,df$neutralaudio_cos_sim_prepost)
  cor.test(df$post_nega_panas,df$neutralaudio_cos_sim_prepost)
  cor.test(df$post_pred_valence,df$neutralaudio_cos_sim_prepost)

  # post_affectAudioFeature_mod <- textTrainRegression(
  #   x = df[,paste0("Dim",1:1280, "_postVoice")],
  #   y = df[,'cos_sim_prepost'],
  #   inside_folds = 9/10,
  #   simulate.p.value = TRUE,
  #   seed = 42
  # )
  # saveRDS(post_affectAudioFeature_mod,
  #         'demo_talk/out/mods/post_affectAudio_feature_mod_pred_prepost_similarity.rds')
  # pre_affectAudioFeature_mod <- textTrainRegression(
  #   x = df[,paste0("Dim",1:1280, "_preVoice")],
  #   y = df[,'cos_sim_prepost'],
  #   inside_folds = 9/10,
  #   simulate.p.value = TRUE,
  #   seed = 42
  # )
  # saveRDS(pre_affectAudioFeature_mod,
  #         'demo_talk/out/mods/pre_affectAudio_feature_mod_pred_prepost_similarity.rds')
}

# classify
if (FALSE){
  post_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_postVoice")],
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  saveRDS(post_affectAudioFeature_mod,
          'demo_talk/out/mods/post_affectAudio_feature_mod_pred_churchmallpark.rds')
  pre_affectAudioFeature_mod <- textTrainRegression(
    x = df[,paste0("Dim",1:1280, "_preVoice")],
    y = as.factor(unlist(df$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  saveRDS(pre_affectAudioFeature_mod,
          'demo_talk/out/mods/pre_affectAudio_feature_mod_pred_churchmallpark.rds')
}

# ---- 1.2 stack pre-post ----
# stack
if (FALSE){
  temp1 <- df[,paste0("Dim",1:1280, "_postVoice")]
  names(temp1) <- paste0("Dim",1:1280)
  temp2 <-df[,paste0("Dim",1:1280, "_preVoice")]
  names(temp2) <- paste0("Dim",1:1280)
  stackedEmb <- rbind(temp1, temp2) %>% as_tibble(.,.name_repiar='minimal')
  
  stackedPANAS_nega <- c(unlist(df[,'PANAS_post_negative']),
                             unlist(df[,'PANAS_pre_negative'])) %>% as_tibble(.,.name_repiar='minimal')
  names(stackedPANAS_nega) <- c('PANAS_negative')
  
  stackedPANAS_posi <- c(unlist(df[,'PANAS_post_positive']),
                         unlist(df[,'PANAS_pre_positive'])) %>% as_tibble(.,.name_repiar='minimal')
  names(stackedPANAS_posi) <- c('PANAS_positive')
  
  stackedPred_valence <- c(unlist(df[,'post_pred_valence']),
                         unlist(df[,'pre_pred_valence'])) %>% as_tibble(.,.name_repiar='minimal')
  names(stackedPred_valence) <- c('pred_valence')
  
  stacked_churchmallpark <- c(unlist(df[,'pPlace']),
                           unlist(df[,'pPlace'])) %>% as_tibble(.,.name_repiar='minimal')
  names(stacked_churchmallpark) <- c('pPlace')
}

if (FALSE){

  stack_affectAudioFeature_mod_panas_nega <- textTrainRegression(
    x = stackedEmb,
    y = stackedPANAS_nega,
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(stack_affectAudioFeature_mod_panas_nega,
          'demo_talk/out/mods/stack_affectAudioFeature_mod_panas_nega.rds')

  stack_affectAudioFeature_mod_panas_posi <- textTrainRegression(
    x = stackedEmb,
    y = stackedPANAS_posi,
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(stack_affectAudioFeature_mod_panas_posi,
          'demo_talk/out/mods/stack_affectAudioFeature_mod_panas_posi.rds')

  stack_affectAudioFeature_mod_pred_valence <- textTrainRegression(
    x = stackedEmb,
    y = stackedPred_valence,
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    seed = 42
  )
  saveRDS(stack_affectAudioFeature_mod_pred_valence,
          'demo_talk/out/mods/stack_affectAudioFeature_mod_pred_valence.rds')

  stack_affectAudioFeature_mod_churchmallpark <- textTrainRegression(
    x = stackedEmb,
    y = as.factor(unlist(stacked_churchmallpark$pPlace)),
    inside_folds = 9/10,
    simulate.p.value = TRUE,
    model = "multinomial",
    eval_measure = 'bal_accuracy',
    seed = 42
  )
  saveRDS(stack_affectAudioFeature_mod_churchmallpark,
          'demo_talk/out/mods/stack_affectAudioFeature_mod_churchmallpark.rds')
}

q()
  