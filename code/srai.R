library(tidyverse)
library(janitor)

d_lookup <- read_csv("/cloud/project/code/lookup.csv")

questions_escaped <- d_lookup$sub_field %>% 
  str_replace_all("\\s+", " ") %>% 
  str_replace_all("([.*+?()\\[\\]{}|^$])", "\\\\\\1")

# d_lookup <- tibble(sub_field = questions,
#                    sub_label = labels,
#                    sub_varname = var_names)

# # Functions -------------------------------------------------------------

split_sub_value <- function(x) {
  if (startsWith(x, "Upload")) {
    upload_id <- str_extract(x, "Upload \\d+")
    file_name <- str_match(x, "Upload \\d+(.*?)- [A-Z]+\\d+[A-Z]+")[, 2]
    file_type <- str_extract(x, "(?<=- )([A-Z]+)")
    file_size <- str_extract(x, "(?<= )[0-9.]+[A-Z]+")
    content <- str_match(x, "\\d+[A-Z]+(.*)")[, 2]
    
    new_value <- paste(file_name, content)
    return(new_value)
  } else {
    return(x)
  }
}

# # Code ------------------------------------------------------------------


d <- readxl::read_xlsx("/cloud/data/safe_responsible_ai.xlsx")

# remove unnecessary columns and update format for submission number and date
d <- d %>% 
  mutate(sub_num = parse_number(sub_num),
         sub_date = lubridate::dmy(sub_date)) %>% 
  select(-c(`web-scraper-order`, `web-scraper-start-url`, sub_link, `sub_link-href`, sub_dl, page))

# Split text data to separate question (in sub_field) and answer (in sub_value)
d <- d %>% 
  filter(!str_detect(sub_textelements, "^Published")) %>% # Published name [name of entity]
  mutate(sub_textelements = str_replace_all(sub_textelements, "\\s+", " ")) %>% 
  mutate(
    sub_field = str_extract(sub_textelements, 
                            paste0("(", str_c(questions_escaped, collapse="|"), ")")),
    sub_value = str_remove(sub_textelements, 
                           paste0("(", str_c(questions_escaped, collapse="|"), ")"))
  ) %>% 
  select(-sub_textelements) %>% 
  left_join(d_lookup, by = "sub_field")

# Extract metadata from the upload/transcription string (e.g., filename) and combine with the transcription.
d <- d %>%
  filter(!(sub_label == 'Transcription' & !str_detect(sub_value, "^Upload"))) %>%
  mutate(
    sub_value = if_else(
      sub_label == 'Transcription' & str_detect(sub_value, "^Upload"),
      str_replace(sub_value, "^(Upload \\d)(.*?)- ([A-Z]+)(\\d+\\.?\\d*[A-Z]{2})(.*)$", "FILENAME: \\2 | CONTENT: \\5"), # Replace with captured filename and content, separated by a space
      sub_value
    )
  )

d_short <- d %>% 
  mutate(sub_value = str_sub(sub_value, 1, 1000)) %>% 
  select(sub_num, sub_name, sub_label, sub_value)



# This code transforms a dataframe 'd' by grouping it by 'sub_num' and 'sub_name', 
# creating new variables based on conditions, and reshaping it to a wider format.
d_wide <- d %>%
  
  # Group data by 'sub_num' and 'sub_name'
  group_by(sub_num, sub_name) %>%
  
  # Create a new column 'written_submission' with value 1 if any 'sub_label' is "Transcription", otherwise 0
  mutate(written_submission = ifelse(any(sub_label == "Transcription"), 1, 0)) %>%
  
  # Shorten 'sub_value' to the first 500 characters if 'sub_label' is "Transcription"
  mutate(sub_value = ifelse(sub_label == "Transcription", str_sub(sub_value, 1, 1000), sub_value)) %>%
  
  # Modify 'sub_varname' by appending a row number if its value is "transcription"
  mutate(sub_varname = ifelse(sub_varname == "transcription", 
                              paste(sub_varname, row_number(), sep = "_"), 
                              sub_varname)) %>%
  
  # Drop the columns 'sub_field' and 'sub_label'
  select(-sub_field, -sub_label) %>%
  
  # Pivot the dataframe to a wider format
  pivot_wider(names_from = sub_varname, values_from = sub_value) %>%
  
  # Reorder columns
  select(
    starts_with("sub"), # Select columns starting with "sub"
    written_submission,
    general_comment, 
    transcription_1, transcription_2, transcription_3, # Assuming there's a transcription_3, include it
    var_names[3:22], # Select columns starting with 'q' based on the order in 'var_names'
    everything() # Include any remaining columns
  )

write_csv(d, "srai_full.csv", na = "")
write_csv(d_short, "srai_shortened.csv", na = "")
write_csv(d_wide, "srai_wide_questions.csv", na = "")

# identify any submission fields with specific words and flag them.
d %>% 
  # filter(!is.na(sub_field)) %>% 
  group_by(sub_num) %>%
  summarise(risk = ifelse(
    any(str_detect(
      sub_value, "catastrophic risk|existential risk"), na.rm = TRUE) == TRUE, 1, 0)) %>% 
  write_csv(., "catrisk.csv", na = "")


extract_context <- function(string, keyword_pattern, n_words = 25) {
  pattern <- sprintf("\\b(?:\\w+\\W+){0,%s}(%s)\\W+(?:\\w+\\W+){0,%s}\\b", n_words, keyword_pattern, n_words)
  matches <- str_extract_all(string, pattern)
  paste(matches[[1]], collapse = " ; ")
}

d_test <-   d %>% 
  filter(str_detect(sub_value, "catastrophic risk|existential risk")) %>% 
  filter(!sub_label == "Transcription") %>% 
  slice_head(n = 10) %>% 
  select(sub_varname, sub_value)

d_test %>% 
  mutate(context = extract_context(sub_value, "catastrophic risk|existential risk")) 

  # filter(str_detect(sub_value, "catastrophic risk|existential risk")) %>% 
  mutate(context = extract_context(sub_value, "catastrophic risk|existential risk")) %>% 
  # filter(!is.na(context)) %>% 
  # select(context)
# 
