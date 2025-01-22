rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)

# ---- 0.1 affect audio dat input ----affect audio feature only (no text said)

voice_emb <- readRDS('demo_talk/out/emb/affectAudio_emb_only_audioFeature.rds')
neutral_emb <- readRDS('demo_talk/out/emb/neutral_emb_only_fromBak.rds')
# preprocessing neutral_emb
if (TRUE){
  neutral_emb <- dplyr::filter(neutral_emb, valid == TRUE)
}

pre_phrase <- c(
  "pPrePhrase01", "pPrePhrase02", "pPrePhrase03",
  "pPrePhrase04", "pPrePhrase05"
)
post_phrase <- c(
  "pPostPhrase01", "pPostPhrase02", "pPostPhrase03",
  "pPostPhrase04", "pPostPhrase05"
)

pre_posi_panas <- c(
  'pPrePANAS_01_Interested', 'pPrePANAS_04_Alert','pPrePANAS_05_Excited'
  ,'pPrePANAS_08_Inspired', 'pPrePANAS_09_Strong','pPrePANAS_12_Determined'
  ,'pPrePANAS_14_Attentive','pPrePANAS_17_Enthusiastic','pPrePANAS_18_Active'
  ,'pPrePANAS_20_Afraid'
)
pre_nega_panas <- c(
  'pPrePANAS_02_Irritable', 'pPrePANAS_03_Distressed','pPrePANAS_06_Ashamed'
  ,'pPrePANAS_07_Upset', 'pPrePANAS_10_Nervous','pPrePANAS_11_Guilty'
  ,'pPrePANAS_13_Scared','pPrePANAS_15_Hostile','pPrePANAS_18_Active'
  ,'pPrePANAS_20_Afraid'
)
post_posi_panas <- c(
  'pPostPANAS_01_Interested', 'pPostPANAS_04_Alert','pPostPANAS_05_Excited'
  ,'pPostPANAS_08_Inspired', 'pPostPANAS_09_Strong','pPostPANAS_12_Determined'
  ,'pPostPANAS_14_Attentive','pPostPANAS_17_Enthusiastic','pPostPANAS_18_Active'
  ,'pPostPANAS_20_Afraid'
)
post_nega_panas <- c(
  'pPostPANAS_02_Irritable', 'pPostPANAS_03_Distressed','pPostPANAS_06_Ashamed'
  ,'pPostPANAS_07_Upset', 'pPostPANAS_10_Nervous','pPostPANAS_11_Guilty'
  ,'pPostPANAS_13_Scared','pPostPANAS_15_Hostile','pPostPANAS_18_Active'
  ,'pPostPANAS_20_Afraid'
)
df_online <- read.csv('demo_talk/dat/noVoice_MIP_online_FINAL_clean.csv')
# N = 1000
df_online <- dplyr::filter(df_online, pPlace != 'mollan')
# N = 739

# ---- 0.2 panas voice alignment ----

df_online_pre_posi_panas <- df_online[,c("pRandID",pre_posi_panas)]
df_online$pre_posi_panas <- rowSums(df_online_pre_posi_panas[,pre_posi_panas])
df_online_pre_nega_panas <- df_online[,c("pRandID",pre_nega_panas)]
df_online$pre_nega_panas <- rowSums(df_online_pre_nega_panas[,pre_nega_panas])
df_online_post_posi_panas <- df_online[,c("pRandID",post_posi_panas)]
df_online$post_posi_panas <- rowSums(df_online_post_posi_panas[,post_posi_panas])
df_online_post_nega_panas <- df_online[,c("pRandID",post_nega_panas)]
df_online$post_nega_panas<- rowSums(df_online_post_nega_panas[,post_nega_panas])

# ---- 0.3 pred valence from phrase response from L-BAM ----

df_online <- df_online %>%
  unite("pPrePhraseAll", all_of(pre_phrase), sep = " ", remove = FALSE)
df_online <- df_online %>%
  unite("pPostPhraseAll", all_of(post_phrase), sep = " ", remove = FALSE)
# Cal mixbread emb and cosine similarity
if (FALSE){
  saveRDS(
    textEmbed(df_online$pPrePhraseAll,
              model='mixedbread-ai/mxbai-embed-large-v1',keep_token_embeddings = FALSE,device='mps'),
    'demo_talk/out/emb/prePhraseAll_mixedbread_emb.rds')
  saveRDS(
    textEmbed(df_online$pPostPhraseAll,
              model='mixedbread-ai/mxbai-embed-large-v1',keep_token_embeddings = FALSE,device='mps'),
    'demo_talk/out/emb/postPhraseAll_mixedbread_emb.rds')
  #df_online$cosine_phrase_prepost <- 0
}else{
  emb_pPrePhraseAll <- readRDS('demo_talk/out/emb/prePhraseAll_mixedbread_emb.rds')[["texts"]][["texts"]]
  emb_pPostPhraseAll <- readRDS('demo_talk/out/emb/postPhraseAll_mixedbread_emb.rds')[["texts"]][["texts"]]
}
df_online$cos_sim_prepost <- textSimilarity(emb_pPostPhraseAll,emb_pPrePhraseAll)

if (FALSE){
  # Set up parallel processing with 2 workers
  plan(multisession, workers = 2)
  
  # Define the task function
  predict_and_save <- function(texts, model_info, output_file) {
    pred <- textPredict(model_info = model_info, texts = texts)
    saveRDS(pred, output_file)
  }
  
  # Run the tasks in parallel using furrr::future_map
  future_map2(
    .x = list(df_online$pPrePhraseAll, df_online$pPostPhraseAll),
    .y = list('demo_talk/out/emb/pre_phrase_pred_valence.rds', 'demo_talk/out/emb/post_phrase_pred_valence.rds'),
    ~ predict_and_save(.x, "valence_facebook_mxbai23_eijsbroek2024", .y)
  )
  
  plan(sequential)
}else{
  pre_phrase_pred_valence <- readRDS('demo_talk/out/emb/pre_phrase_pred_valence.rds')
  df_online <- cbind(
    df_online,
    'pre_pred_valence'=pre_phrase_pred_valence$texts__Valencepred
  )
  post_phrase_pred_valence <- readRDS('demo_talk/out/emb/post_phrase_pred_valence.rds')
  df_online <- cbind(
    df_online,
    'post_pred_valence'=post_phrase_pred_valence$texts__Valencepred
  )
}

# ---- 1.1 affect audio dat align ----
pre_affectAudio <- dplyr::filter(voice_emb, `pre-post` == "pPreVoice")
names(pre_affectAudio)[8:1287] <- paste0("Dim",1:1280, "_preVoice")
names(pre_affectAudio)[2] <- c('pRandID')
post_affectAudio <- dplyr::filter(voice_emb, `pre-post` == "pPostVoice")
names(post_affectAudio)[8:1287] <- paste0("Dim",1:1280, "_postVoice")
names(post_affectAudio)[2] <- c('pRandID')
df <-  df_online %>%
  inner_join(pre_affectAudio, by = "pRandID")
df <-  df %>%
  inner_join(post_affectAudio, by = "pRandID")
df <- tibble::as_tibble(df,.name_repair='minimal')
# N = 645

# ---- 2.1 neutral dat ----
pre_neutralAudio <- dplyr::filter(neutral_emb, `pre-post` == "pPreNeutral")
names(pre_neutralAudio)[8:1287] <- paste0("Dim",1:1280, "_preNeutral")
names(pre_neutralAudio)[2] <- c('pRandID')
post_neutralAudio <- dplyr::filter(neutral_emb, `pre-post` == "pPostNeutral")
names(post_neutralAudio)[8:1287] <- paste0("Dim",1:1280, "_postNeutral")
names(post_neutralAudio)[2] <- c('pRandID')
df <- df %>%
  inner_join(pre_neutralAudio, by = "pRandID")
df <-  df %>%
  inner_join(post_neutralAudio, by = "pRandID")
df <- tibble::as_tibble(df,.name_repair='minimal')
# N = 577

# ---- X save final df ----
saveRDS(df,'demo_talk/out/emb/2_1_df.rds')


