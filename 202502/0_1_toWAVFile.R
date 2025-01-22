rm(list=ls())
gc()

Sys.setenv(OMP_NUM_THREADS = "1") 
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE") 

set.seed(42)
Sys.setenv(LANG = "C.UTF-8", LC_ALL="C.UTF-8")

library(tidyverse)
# devtools::install.github('theharmonylab/talk')
#library(talk)

# ---- 0.1 dat input ----
datFolder <- 'demo_talk/dat/'
to_wavs_folder <- 'demo_talk/to_wavs/'

# online 
if (TRUE){
  file1 <- file.path(paste0(
    datFolder, 'tidyData_result_withVoice.csv'))
  dfonl <- tibble::as_tibble(read.csv(file1), .name_repair = 'minimal') %>%
    dplyr::slice_sample(n = nrow(.)) #%>%
    #dplyr::slice_head(n = 200)
}

# ---- 0.2 online Transform base64 to wav files ----
if (!requireNamespace('base64enc', quietly = TRUE)) {
  install.packages('base64enc', dependencies = TRUE)
}
library('base64enc', character.only = TRUE)

# Initialize a tibble to record files 
files_log <- tibble(
  index = character(),
  pRandID = character(),
  filename = character()
)

# function to iterate
process_voice_files <- function(df, i, to_wavs_folder, small_files_log) {
  
  # Helper function to process individual voice entries
  process_single_voice <- function(b64_string, type, index, pRandID) {
    # Ensure there is no extra whitespace/newlines in the base64 string
    b64_string_clean <- gsub("\\s+", "", b64_string)
    
    wav_raw <- base64decode(b64_string)
    
    # Check if decoding returned any data.
    if (length(wav_raw) == 0) {
      warning(paste("Decoding failed for", type, "at index", index))
      return(data.frame(
        index = index,
        pRandID = pRandID,
        filename = NA,
        status = "error: decoding failed",
        stringsAsFactors = FALSE
      ))
    }
    
    size <- length(wav_raw)
    
    # Note: File size filter based on clear human voice
    if (size >= 40 * 1024 || (grepl('Neutral', type)  && size >= 34 * 1024)
        ){
      output_file <- file.path(to_wavs_folder, paste0("output_full_", type, "_", index, '_',pRandID,".webm"))
      
      tryCatch({
        writeBin(wav_raw, output_file)
      }, error = function(e) {
        warning(paste("Error writing file:", output_file, ":", e$message))
      })
      
      return(data.frame(
        index = index,
        pRandID = pRandID,
        filename = paste0("output_full_", type, "_", index, '_',pRandID,".webm"),
        stringsAsFactors = FALSE
      ))
    } else {
      output_file <- file.path(to_wavs_folder, paste0("output_tooSmall_", type, "_", index, '_',pRandID, ".webm"))
      
      tryCatch({
        writeBin(wav_raw, output_file)
      }, error = function(e) {
        warning(paste("Error writing file:", output_file, ":", e$message))
      })
      
      return(data.frame(
        index = index,
        pRandID = pRandID,
        filename = paste0("output_tooSmall_", type, "_", index, '_',pRandID, ".webm"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Process Pre
  pre_log <- process_single_voice(df$pPreVoice[i], "pPreVoice", i, df$pRandID[i])
  preNeu_log <- process_single_voice(df$pPreNeutral[i], "pPreNeutral", i, df$pRandID[i])
  
  # Process Post
  post_log <- process_single_voice(df$pPostVoice[i], "pPostVoice", i, df$pRandID[i])
  postNeu_log <- process_single_voice(df$pPostNeutral[i], "pPostNeutral", i, df$pRandID[i])
  
  # Combine logs
  if (i == 1){
    files_log <- bind_rows(pre_log, preNeu_log, post_log, postNeu_log)
    return (files_log)
  }else{
    files_log <- bind_rows(files_log, pre_log, preNeu_log, post_log, postNeu_log)
    return(files_log)
  }
  
}

# Iterate over each row of the dataframe
for (i in 1:nrow(dfonl)) {
  print(i)
  files_log <- process_voice_files(dfonl, i, to_wavs_folder, files_log)
  #if (i==5){break}
}

files_log_wide <- files_log %>%
  mutate(
    file_type = str_extract(filename, "pPreVoice|pPreNeutral|pPostVoice|pPostNeutral")
  ) %>%
  pivot_wider(
    id_cols = c(index, pRandID),
    names_from = file_type,
    values_from = filename
  ) %>%
  select(index, pRandID, pPreVoice, pPreNeutral, pPostVoice, pPostNeutral) %>%
  # Add the 'tooSmall' column
  mutate(
    tooSmall = if_else(
      if_any(c(pPreVoice, pPreNeutral, pPostVoice, pPostNeutral), ~ str_detect(.x, "tooSmall")),
      1,
      0
    )
  ) %>%
  write_csv(.,file.path(to_wavs_folder, "online_small_files_log.csv"))

# ---- 0.3 recode the .webm files into .wav files talk can handle ----

if (!requireNamespace('av', quietly = TRUE)) {
  install.packages('av', dependencies = TRUE)
}
library(av)

# online 
webmFiles <- list.files(to_wavs_folder, pattern = "\\.webm$", full.names = TRUE)
webmFiles <- webmFiles[!grepl("tooSmall", webmFiles)]

# Create a directory for output files (optional)
output_dir <- file.path(to_wavs_folder, "processed")
if (dir.exists(output_dir)){unlink(output_dir, recursive = TRUE, force = TRUE)}
dir.create(output_dir, showWarnings = FALSE)

# Loop through the files and recode them
lapply(webmFiles, function(input_file) {
  # Construct the output file path
  output_file <- file.path(output_dir, basename(input_file))
  output_file <- sub("\\.webm$", ".wav", output_file)
  
  # Construct the ffmpeg command
  # -f wav forces the wav muxer (standard header)
  # -ar and -ac explicitly set sample rate and channels
  # -map_metadata -1 strips any extra metadata
  #' command <- sprintf( # -f wav
  #'   'ffmpeg -loglevel quiet -i "%s" -ac "%s" -ar "%s" -map_metadata -1 "%s"',
  #'   #'ffmpeg -y -i "%s" -f wav -c:a pcm_s16le -ar 48000 -ac 1 -map_metadata -1 "%s"',
  #'   input_file, 
  #'   1, # channel
  #'   48000, # samping rate
  #'   output_file
  #' )
  
  # Execute the command
  #system(command)
  av_audio_convert(input_file, output_file)
})

cat("Processing complete. Recoded files are saved in:", output_dir)

stop('---- End of the Program. ----')


# ---- X1 Get offline MIP voice files ----

# Copied from 'MIP/offlineMIP_analysis/result.csv'
df <- read.csv(paste0(datFolder, 'offline_voice_result.csv'))
df <- tibble::as_tibble(df,.name_repair = 'minimal')

#clean data
# from noUPLOAD_OskarPreProc_offlineMIP_translation_more.R

#remove tester
df <- df %>% filter(pAge !="August") # It cannot remove all "August".
df <- df %>% filter(!procID %in% c(42, 43, 46)) # pAge = 'August'
df <- df %>% filter(pAge !="august")
df <- df %>% filter(pAge !="TEST")
df <- df %>% filter(pAge !="Oskar")
df <- df %>% filter(procID !="1")
df <- df %>% filter(procID !="2")

#Age with only numbers
df[5,9] <- "47"
df[68,9] <-"20"

#remove incomplete answers
df <- df %>% filter(procID !=100) #not finnished
df <- df %>% filter(procID !=119) #not finnished
df <- df %>% filter(procID !=137) #Norwegian
df <- df %>% filter(procID !=172) #Danish
#remove under the age 18
df <- df %>% filter(procID !=59)
df <- df %>% filter(procID !=73)
df <- df %>% filter(procID !=80)
df <- df %>% filter(procID !=98)

if (nrow(df) != 153){
  nrow(df)
  Stop('The dataset is wrong!')}else{cat('Continue!')}

# function to iterate
process_voice_files <- function(df, i, to_wavs_folder, small_files_log) {
  
  # Helper function to process individual voice entries
  process_single_voice <- function(b64_string, type, index, pRandID) {
    wav_raw <- base64decode(b64_string)
    size <- length(wav_raw)
    
    # Note: File size filter based on clear human voice
    if (size >= 40 * 1024 || (grepl('Neutral', type)  && size >= 20 * 1024) ){
      output_file <- file.path(to_wavs_folder, paste0("output_full_", type, "_", index, '_',pRandID,".wav"))
      writeBin(wav_raw, output_file)
      return(data.frame(
        index = index,
        pRandID = pRandID,
        filename = paste0("output_full_", type, "_", index, '_',pRandID,".wav"),
        stringsAsFactors = FALSE
      ))
    } else {
      output_file <- file.path(to_wavs_folder, paste0("output_tooSmall_", type, "_", index, '_',pRandID, ".wav"))
      writeBin(wav_raw # '-1'
               , paste0(output_file))
      return(data.frame(
        index = index,
        pRandID = pRandID,
        filename = paste0("output_tooSmall_", type, "_", index, '_',pRandID, ".wav"),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Process Pre
  pre_log <- process_single_voice(df$pPreVoice[i], "pPreVoice", i, df$pRandID[i])
  preNeu_log <- process_single_voice(df$pPreNeutral[i], "pPreNeutral", i, df$pRandID[i])
  
  # Process Post
  post_log <- process_single_voice(df$pPostVoice[i], "pPostVoice", i, df$pRandID[i])
  postNeu_log <- process_single_voice(df$pPostNeutral[i], "pPostNeutral", i, df$pRandID[i])
  
  # Combine logs
  if (i == 1){
    files_log <- bind_rows(pre_log, preNeu_log, post_log, postNeu_log)
    return (files_log)
  }else{
    files_log <- bind_rows(files_log, pre_log, preNeu_log, post_log, postNeu_log)
    return(files_log)
  }
  
}

# Iterate over each row of the dataframe
for (i in 1:nrow(df)) {
  print(i)
  files_log <- process_voice_files(df, i, to_wavs_folder, files_log)
  #if (i ==100){break}
}

files_log_wide <- files_log %>%
  mutate(
    file_type = str_extract(filename, "pPreVoice|pPreNeutral|pPostVoice|pPostNeutral")
  ) %>%
  pivot_wider(
    id_cols = c(index, pRandID),
    names_from = file_type,
    values_from = filename
  ) %>%
  select(index, pRandID, pPreVoice, pPreNeutral, pPostVoice, pPostNeutral) %>%
  # Add the 'tooSmall' column
  mutate(
    tooSmall = if_else(
      if_any(c(pPreVoice, pPreNeutral, pPostVoice, pPostNeutral), ~ str_detect(.x, "tooSmall")),
      1,
      0
    )
  ) %>%
  write_csv(.,file.path(to_wavs_folder, "small_files_log.csv"))

# Print a summary message
cat("Processing complete.\n")
#cat(paste0("Number of files written:",(nrow(df) * 4) - nrow(files_log),"\n"))
cat("Number of small files recorded:", nrow(files_log), "\n")

# ---- Misc. Invalid file notes ----
Invalid <- c(
  'pPostNeutral_154_izaEaz4HpB'
)