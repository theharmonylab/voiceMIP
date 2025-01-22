# if talkEmbed produces a system error, just run .rs.restartR(). It may work.
rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
# devtools::install_github('theharmonylab/talk')
library(talk)
library(furrr)
path1 <- getwd()

# ---- 0. voice data input ----
test <- FALSE
vocFolder <- 'demo_talk/to_wavs/processed'
vocFiles <- list.files(vocFolder, pattern = "full", full.names = TRUE)
vocFiles <- list.files(vocFolder, pattern = "Voice", full.names = TRUE)
if (test){vocFiles <- vocFiles[1:testNum]}

# ---- 1. embed voice only, not text ----
test <- FALSE
voice_emb_only <- readRDS(paste0(path1,'/',
                                 'demo_talk/to_wavs/qualitycheck/corrected_voice_quality_check_full.rds'))
if (test){
  voice_emb_only <- voice_emb_only[1:20,]
  chunk_size <- 5
}else{chunk_size <- 25}
if (TRUE){
  # further filter based on language == en
  if (!requireNamespace("cld2", quietly = TRUE)) {
    install.packages("cld2")
  }
  library(cld2)
  
  # visual checking in rows which lang = NA during filter stage below
  remove_procID_lang_NA <- c(220, 407, 170, 186, 374, 774, 992)
  
  temp = voice_emb_only
  temp$lang = cld2::detect_language(voice_emb_only$transcript)
  voice_emb_only <- cbind(
    voice_emb_only[,1:4],
    'lang'=temp[['lang']],
    voice_emb_only[,5:1287]
  ) %>% filter(valid==TRUE) %>% filter (lang == 'en' | is.na(lang)) %>%
    filter(!(procID %in% remove_procID_lang_NA & is.na(lang)))
  temp <- NULL
  voice_emb_only <- voice_emb_only[, !names(voice_emb_only) %in% 'lang']
}


# ---- 1.1 get embed ----
if (TRUE){
  # Set up parallel backend with 2 cores
  plan(multisession, workers = 2)
  
  # Function to process a single row
  process_row <- function(filepath, valid) {
    names1 <- paste0('Dim', 1:1280)
    if (valid) {
      temp <- talkEmbed(
        filepath,
        model = 'openai/whisper-large-v3',
        audio_transcriptions = FALSE, 
        use_decode = FALSE
      )
      names(temp) <- names1
      return(temp)
    } else {
      temp <- as_tibble(matrix(0,nrow=1,ncol=1280),.name_repair='minimal')
      names(temp) <- names1
      return(temp) # Replace 1280 with the correct embedding size
    }
  }
  
  # Process rows in chunks to reduce memory usage
  chunk_size <- chunk_size  # Adjust this depending on available memory
  row_indices <- seq_len(nrow(voice_emb_only))
  
  # Create an empty matrix for storing embeddings
  embeddings_matrix <- matrix(0, nrow = nrow(voice_emb_only), ncol = 1280) # Adjust column size as needed
  
  for (chunk_start in seq(1, nrow(voice_emb_only), by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, nrow(voice_emb_only))
    chunk_indices <- row_indices[chunk_start:chunk_end]
    cat('#############################','\n',
        '    Now chunk row ',as.character(chunk_start), ' / ',
        'total ', as.character(nrow(voice_emb_only)),' in the chunk.\n',
        '    row_start: ', as.character(chunk_start),'\n',
        '    row_end: ', as.character(chunk_end),'\n',
        '    START of the chunk!!!!!!!!!!','\n',
        '#############################\n')
    
    # Process rows in parallel for the current chunk
    chunk_embeddings <- future_map2_dfr(
      voice_emb_only$filepath[chunk_indices],
      voice_emb_only$valid[chunk_indices],
      process_row,
      .options = furrr_options(seed = TRUE) # Ensures reproducibility
    )
    
    # Store chunk results in the embeddings matrix
    embeddings_matrix[chunk_indices, ] <- as.matrix(chunk_embeddings)
    # Combine the results into the original data frame
    embeddings_matrix <- as_tibble(embeddings_matrix, .name_repair='minimal')
    names(embeddings_matrix) <- paste0("Dim",1:1280)
    voice_emb_only[chunk_indices, 8:1287] <- embeddings_matrix[chunk_indices,]
    saveRDS(voice_emb_only,
            paste0(path1,'/','demo_talk/out/backup_temp_voice_emb_only.rds'))
    
    cat('#############################','\n',
        '    Now chunk row ',as.character(chunk_start), ' / ',
        'total ', as.character(nrow(voice_emb_only)),' in the chunk.\n',
        '    row_start: ', as.character(chunk_start),'\n',
        '    row_end: ', as.character(chunk_end),'\n',
        '    END of the chunk!!!!!!!!!!','\n',
        '#############################\n')
  }
  
  # Combine the results into the original data frame
  embeddings_matrix <- as_tibble(embeddings_matrix, .name_repair='minimal')
  names(embeddings_matrix) <- paste0("Dim",1:1280)
  voice_emb_only <- cbind(voice_emb_only[, -c(8:1287)], embeddings_matrix)
  saveRDS(voice_emb_only,
          paste0(path1,'/','demo_talk/out/voice_emb_only.rds'))
  
  # Shut down the parallel backend
  plan(sequential)
}



# ---- misc code ----
if (FALSE){
    NULL
}

