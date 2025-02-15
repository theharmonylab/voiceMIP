rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
library(text)

affectSpeechTranscript <- readRDS('demo_talk/out/emb/affectAudio_emb_only_audioFeature.rds')

pre1 <- affectSpeechTranscript %>% filter(`pre-post`=='pPreVoice')
post1 <- affectSpeechTranscript %>% filter(`pre-post`=='pPostVoice')


temp <- list()
temp$preAffectTranscript <- textEmbed(pre1$transcript,
                  model = 'mixedbread-ai/mxbai-embed-large-v1',
                  keep_token_embeddings = FALSE,device='mps')[["texts"]][["texts"]]
temp$postAffectTranscript <- textEmbed(post1$transcript,
                                 model = 'mixedbread-ai/mxbai-embed-large-v1',
                                 keep_token_embeddings = FALSE,device='mps')[["texts"]][["texts"]]
temp$preAffectTranscript <- cbind('pRandID' = pre1$randID,temp$preAffectTranscript)

temp$postAffectTranscript <- cbind('pRandID' = post1$randID,temp$postAffectTranscript)


saveRDS(
  temp,
  'demo_talk/out/emb/mixb_online_affectAudioTranscriptEmbed.rds'
)




