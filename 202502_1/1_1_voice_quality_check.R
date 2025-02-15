#rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
# devtools::install_github('theharmonylab/talk')
library(talk)


# ---- 1.0 voice data input ----
test <- FALSE
testNum <- 1
vocFolder <- 'demo_talk/to_wavs/processed'
vocFiles <- list.files(vocFolder, pattern = "full", full.names = TRUE)
vocFiles <- list.files(vocFolder, pattern = "Voice", full.names = TRUE) # Voice / Neutral
if (test){vocFiles <- vocFiles[1:testNum]}

# ---- 1.1 embed, tested with taleText ----
# model = 'openai/whisper-large-v3' for talkEmbed containing 1280 dims
output <- tibble::as_tibble(
  matrix(nrow=length(vocFiles), ncol=7+1280),.name_repair = 'minimal')
names(output)[1:7] <- c("procID", "randID", "pre-post","neutral","valid","transcript","filepath")
names(output)[8:1287] <- c(paste0("Dim",1:1280))
output$neutral <- 'voice'
output[,8:1287] <- 0

# ---- 1.2 quality check with transcripts ----

i <- 1
for (file1 in vocFiles){
  keys <- unlist(strsplit(file1, "_"))[5:7]
  output[["pre-post"]][i] <- keys[1]
  output[["procID"]][i] <- keys[2]
  output[["randID"]][i] <- sub("\\.wav$", "", keys[3])
  output[["filepath"]][i] <- file1
  
  output[["transcript"]][i] <- talkText(file1,device='mps') # mps works
  # it needs to contain at least 3 words. and it should be english (not able to detect), and it should be character vector.
  if (!is.character(output[["transcript"]][i])){output[["valid"]][i] <- FALSE}
  if (length(unlist(strsplit(output[["transcript"]][i], "\\s+"))) > 2){
    output[["valid"]][i] <- TRUE
  }else{output[["valid"]][i] <- FALSE}
  
  if(i %% 50 == 0){saveRDS(output,paste0("demo_talk/to_wavs/qualitycheck/voice_quality_check_", as.character(i),".rds"))}
  i <- i + 1
}
# voice_quality_check_full$valid[1092] # FALSE set to TRUE
# voice_quality_check_full$transcript[1092]
# [1] " Okay."
# voice_quality_check_full$valid[1947]
# [1] FALSE ; to TRUE
# voice_quality_check_full$transcript[1947]
# " Sleepy." -> corrected_voice_quality_check_full.rds

saveRDS(output,"demo_talk/to_wavs/qualitycheck/voice_quality_check_full.rds")

q()

# ---- 2.0 voice data input ----
test <- FALSE
testNum <- 1
vocFolder <- 'demo_talk/to_wavs/processed'
vocFiles <- list.files(vocFolder, pattern = "full", full.names = TRUE)
vocFiles <- list.files(vocFolder, pattern = "Neutral", full.names = TRUE)
if (test){vocFiles <- vocFiles[1:testNum]}

# ---- 2.1 neutral embed, tested with taleText ----
# model = 'openai/whisper-large-v3' for talkEmbed containing 1280 dims
output <- tibble::as_tibble(
  matrix(nrow=length(vocFiles), ncol=7+1280),.name_repair = 'minimal')
names(output)[1:7] <- c("procID", "randID", "pre-post","neutral","valid","transcript","filepath")
names(output)[8:1287] <- c(paste0("Dim",1:1280))
output$neutral <- 'neutral'
output[,8:1287] <- 0

# ---- 2.2 quality check with transcripts neutral sents ----
i <- 1
for (file1 in vocFiles){
  keys <- unlist(strsplit(file1, "_"))[5:7]
  output[["pre-post"]][i] <- keys[1]
  output[["procID"]][i] <- keys[2]
  output[["randID"]][i] <- sub("\\.wav$", "", keys[3])
  output[["filepath"]][i] <- file1
  
  output[["transcript"]][i] <- talkText(file1,device='mps') # mps works
  if(grepl("on the screen",  output[["transcript"]][i])){output[["valid"]][i] <- TRUE}else{output[["valid"]][i] <- FALSE}

  if(i %% 50 == 0){saveRDS(output,paste0("demo_talk/to_wavs/qualitycheck/neutral_quality_check_", as.character(i),".rds"))}
  i <- i + 1
}


saveRDS(output,"demo_talk/to_wavs/qualitycheck/neutral_quality_check_full.rds")

# ---- misc code ----
if (FALSE){
  
  
  NULL
}

