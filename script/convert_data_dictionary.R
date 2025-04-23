###################################
# Convert PVDOMICS data dictionary
library(dplyr)
library(purrr)
library(jsonlite)
library(digest)

# Load and preprocess new format data dictionary
data_dict <- read.csv("data/PVD_NSRR_DD.csv", stringsAsFactors = FALSE) %>%
  setNames(tolower(names(.))) %>%
  mutate(
    variable.name = ifelse(variable.name %in% c("", "."), NA, variable.name),
    is_variable = !is.na(variable.name),
    group_id = cumsum(is_variable)
  )%>%
  select(dataset,variable.name,label,is_variable,group_id)

# Create standardized structure matching original format
full_data_dict <- data_dict %>%
  group_by(group_id) %>%
  mutate(
    form_name = tolower(first(dataset)),
    question_name = tolower(first(variable.name)),
    question_text = tolower(first(label)),
    codevalue = ifelse(row_number() == 1, 0, row_number() - 1),
    display = tolower(ifelse(row_number() == 1, NA, label))
  ) %>%
  ungroup() %>%
  select(form_name, question_name, question_text, codevalue, display) %>% 
  filter(!(is.na(codevalue) & is.na(display) & row_number() > 1))  
         
# Create base table with all variables
table1 <- full_data_dict %>%
  group_by(question_name) %>%
  summarise(
    folder = first(form_name),
    display_name = first(question_text),
    description = "",
    type = if_else(any(!is.na(codevalue)), "choices", "continuous"),
    units = "",
    domain = NA_character_,
    labels = NA_character_,
    calculation = "",
    commonly_used = "",
    forms = first(form_name),
    .groups = "drop"
  )

# Process categorical variables
categorical_data <- full_data_dict %>%
  filter(codevalue!=0 & codevalue != "") %>%
  select(question_name, codevalue, display) %>%
  arrange(question_name, codevalue)
# drop the leading ".     " in the display column
categorical_data$display <- sub("^\\.\\s{5}", "", categorical_data$display)

if(nrow(categorical_data) > 0) {
  # Generate JSON objects
  json_objects <- categorical_data %>%
    group_by(question_name) %>%
    summarise(
      levels = list(
        map2(codevalue, display, ~ list(
          value = .x,
          display_name = .y,
          description = ""
        ))
      ),
      n_levels = n(),
      .groups = "drop"
    ) %>%
    mutate(
      json_name = paste0(question_name, "_", n_levels),
      json_str = map_chr(levels, ~ toJSON(.x, auto_unbox = TRUE))
    )
  
  # Consolidate duplicate JSONs
  json_objects <- json_objects %>%
    mutate(json_hash = map_chr(json_str, digest)) %>%
    group_by(json_hash) %>%
    mutate(
      group_id = cur_group_id(),
      consolidated_name = first(json_name)
    ) %>%
    ungroup()
  
  # Update domain mapping
  domain_mapping <- json_objects %>%
    select(question_name, domain = consolidated_name)
  
  table1 <- table1 %>%
    left_join(domain_mapping, by = "question_name") %>%
    mutate(
      domain = if_else(type == "choices", coalesce(domain.y, domain.x), domain.x)
    ) %>%
    select(-domain.x, -domain.y) %>%
    rename(domain = domain)
  
  # Create domain directory
  if(!dir.exists("domain")) dir.create("domain", recursive = TRUE)
  
  # Write JSON files
  json_objects %>%
    group_by(group_id) %>%
    slice(1) %>%
    ungroup() %>%
    select(consolidated_name, levels) %>%
    pwalk(function(consolidated_name, levels) {
      json_path <- file.path("domain", paste0(consolidated_name, ".json"))
      write_json(levels, json_path, auto_unbox = TRUE, pretty = TRUE)
    })
}

table1<-table1 %>% 
  rename_with(tolower)%>%
  dplyr::rename(id=question_name)%>%
  select(folder,id,display_name,description,type,units,domain,labels,calculation,commonly_used,forms)
# Save final table
write.csv(
  table1, 
  "spout_pvd_dd.csv", 
  row.names = FALSE, 
  na = ""
)
