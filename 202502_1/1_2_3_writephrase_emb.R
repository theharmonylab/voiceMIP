rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
library(text)

pre_phrase <- c(
  "pPrePhrase01", "pPrePhrase02", "pPrePhrase03",
  "pPrePhrase04", "pPrePhrase05"
)
post_phrase <- c(
  "pPostPhrase01", "pPostPhrase02", "pPostPhrase03",
  "pPostPhrase04", "pPostPhrase05"
)

df_online <- read.csv('demo_talk/dat/noVoice_MIP_online_FINAL_clean.csv')
# N = 1000
df_online <- dplyr::filter(df_online, pPlace != 'mollan')
# N = 739

df_online <- df_online %>%
  unite("pPrePhraseAll", all_of(pre_phrase), sep = " ", remove = FALSE) %>%
  unite("pPostPhraseAll", all_of(post_phrase), sep = " ", remove = FALSE)

temp <- list()
temp$pPrePhraseAll <- textEmbed(df_online$pPrePhraseAll,
                  model = 'mixedbread-ai/mxbai-embed-large-v1',
                  keep_token_embeddings = FALSE,device='mps')
temp$pPostPhraseAll <- textEmbed(df_online$pPostPhraseAll,
                                 model = 'mixedbread-ai/mxbai-embed-large-v1',
                                 keep_token_embeddings = FALSE,device='mps')
temp$pRandID <- df_online$pRandID

saveRDS(
  temp,
  'demo_talk/out/emb/mixb_online_phraseAll.rds'
)




