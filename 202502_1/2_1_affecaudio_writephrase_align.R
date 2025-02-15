rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)

# ---- 0.1 affect audio dat input ----affect audio feature only (no text included)

voice_emb <- readRDS('demo_talk/out/emb/affectAudio_emb_only_audioFeature.rds')
affectAudioTranscript <- readRDS('demo_talk/out/emb/mixb_online_affectAudioTranscriptEmbed.rds')

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


# ---- 0.3 write phrase alignment ----
temp <- readRDS('demo_talk/out/emb/mixb_online_phraseAll.rds')
names(temp[["pPrePhraseAll"]][["texts"]][["texts"]]) <- c(paste0('Dim', 1:1024, '_prePhrase'))
names(temp[["pPostPhraseAll"]][["texts"]][["texts"]]) <- c(paste0('Dim', 1:1024, '_postPhrase'))
df_online <- cbind(df_online, temp[["pPrePhraseAll"]][["texts"]][["texts"]], 
                   temp[["pPostPhraseAll"]][["texts"]][["texts"]])

# ---- 1.1 affect audio dat align ----
pre_affectAudio <- dplyr::filter(voice_emb, `pre-post` == "pPreVoice")
names(pre_affectAudio)[8:1287] <- paste0("Dim",1:1280, "_preVoice")
names(pre_affectAudio)[2] <- c('pRandID')
post_affectAudio <- dplyr::filter(voice_emb, `pre-post` == "pPostVoice")
names(post_affectAudio)[8:1287] <- paste0("Dim",1:1280, "_postVoice")
names(post_affectAudio)[2] <- c('pRandID')
names(affectAudioTranscript$preAffectTranscript)[1] <- c('pRandID')
names(affectAudioTranscript$preAffectTranscript)[2:1025] <- c(paste0('Dim',1:1024,
                                                                     '_preAffectTranscript'))
names(affectAudioTranscript$postAffectTranscript)[1] <- c('pRandID')
names(affectAudioTranscript$postAffectTranscript)[2:1025] <- c(paste0('Dim',1:1024,
                                                                      '_postAffectTranscript'))

df <-  df_online %>%
  inner_join(pre_affectAudio, by = "pRandID") %>%
  inner_join(post_affectAudio, by = "pRandID") %>%
  inner_join(affectAudioTranscript$preAffectTranscript, by = 'pRandID') %>%
  inner_join(affectAudioTranscript$postAffectTranscript, by = 'pRandID') %>%
  tibble::as_tibble(.name_repair='minimal')
# N = 645

# ---- X save final df ----
saveRDS(df,'demo_talk/out/emb/script_2_1_df.rds')


