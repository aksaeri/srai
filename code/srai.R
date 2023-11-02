# Safe and Responsible AI analysis ----------------------------------------
# 
# Author: Alexander Saeri (alexander@aksaeri.com)


# # Startup ---------------------------------------------------------------

library(tidyverse)
library(janitor)

d_lookup <- read_csv("/cloud/project/code/lookup.csv")
d <- readxl::read_xlsx("/cloud/project/data/safe_responsible_ai.xlsx")

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
    d_lookup$sub_varname[3:22], # Select columns starting with 'q' based on the order in 'var_names'
    everything() # Include any remaining columns
  )

write_rds(d, "/cloud/project/data/output/srai_full.rds")
write_csv(d, "/cloud/project/data/output/srai_full.csv", na = "")
write_csv(d_short, "/cloud/project/data/output/srai_shortened.csv", na = "")
write_csv(d_wide, "/cloud/project/data/output/srai_wide_questions.csv", na = "")


# # Additional analyses ---------------------------------------------------

# identify any submission fields with specific words and flag them.
d %>% 
  # filter(!is.na(sub_field)) %>% 
  group_by(sub_num) %>%
  summarise(risk = ifelse(
    any(str_detect(
      sub_value, "catastrophic risk|existential risk"), na.rm = TRUE) == TRUE, 1, 0))



# Define a regular expression for an email address
email_regex <- "[[:alnum:]_.]+@[[:alnum:]_.]+\\.[[:alnum:]_.]{2,}"
email_to_remove <- "digitaleconomy@industry.gov.au"

# Use mutate along with rowwise to extract all email addresses
d_emails <- d %>%
  filter(sub_varname == "transcription") %>% 
  rowwise() %>%
  mutate(emails = list(str_extract_all(sub_value, email_regex))) %>% 
  mutate(emails = map_chr(emails, ~paste(.x, collapse = ", "))) %>% 
  # Remove the specific email address
  mutate(emails = str_replace_all(emails, regex(email_to_remove, ignore_case = TRUE), "")) %>%
  # Remove any trailing commas left after the email removal
  mutate(emails = str_replace_all(emails, "\\s*,\\s*,", ", ")) %>%
  # Trim whitespace which may be left at the start or end of the string
  mutate(emails = str_trim(emails)) %>% 
  group_by(sub_num) %>%
  summarise(all_emails = paste(unique(emails), collapse = ", ")) %>%
  ungroup()


# get_risk_context <- function(string, pattern, n_words = 10) {
#   full_pattern <- paste0("([^\\s]+\\s+){",n_words,"}",pattern,"(\\s+[^\\s]+){",n_words,"}")
#   str_extract_all(string, full_pattern)
# }

# d_test <- d %>% 
#   filter(str_detect(sub_value, "catastrophic risk|existential risk")) %>% 
#   filter(!sub_label == "Transcription") %>% 
#   slice_head(n = 10) %>% 
#   select(sub_varname, sub_value)
# 
# d_test %>% 
#   mutate(context = str_extract(sub_value, paste0("(\\w+){",10,"}","catastrophic risk","(\\w+){",10,"}")))
# 
# d_test %>% 
#   mutate(context = get_risk_context(sub_value, "catastrophic risk|existential risk")) %>% select(context)
