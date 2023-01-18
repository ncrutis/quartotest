#ATC code extraction

library(readr)
NCI_thesaurus <- read_delim("~/Documents/COVICIS Overview/ImmunoVax/NCI_thesaurus.csv",
                            delim = "\t", escape_double = FALSE,
                            col_names = FALSE, trim_ws = TRUE) %>%
  rename(code = X1,
         concept_iri = X2,
         parents = X3,
         synonyms = X4,
         definition = X5,
         display_name = X6,
         status = X7,
         semantic_type = X8
  ) %>%
  filter(semantic_type == "Therapeutic or Preventive Procedure" | grepl("Pharmacologic Substance", semantic_type)) %>%
  dplyr::select(1,3:7) %>%
  separate_rows(synonyms, sep = "\\|")


View(NCI_thesaurus)

test <- NCI_thesaurus %>% filter(is.na(parents))


nci_synonyms <- paste(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)", "", NCI_thesaurus$synonyms, perl=TRUE)), collapse = "|")
mod_test <- mod_data %>%
    mutate(NCI_meds= sapply(str_extract_all(mod_data$`Immunosuppressive therapy`,regex(nci_synonyms, ignore_case = TRUE)),
                            function(x) paste(x,collapse=" | ")))   %>%
  mutate(NCI_meds = ifelse(NCI_meds == "NA", "", NCI_meds))  %>%
  separate_rows(NCI_meds, sep = " \\| ")


table(mod_test$NCI_meds)


test <- as.data.frame(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)", "", NCI_thesaurus$synonyms)))


ATC_code_extract <- function(data, #data frame
                             extvar, #medication variable
                             idvar, #subject ID / grouping variable
                             source, #source medication list
                             s_atc, #source atc list
                             s_medname #source medication name

){

  #
  # EMA_name <- paste(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)", "", EMA_med_list$medicine_name, perl=TRUE)), collapse = "|")
  # EMA_substance <- paste(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)*", "", EMA_med_list$active_substance, perl=TRUE)), collapse = "|")
  # EMA_generic <- paste(gsub("-|,|;", " ",gsub("\\s+\\([^\\)]*\\)", "", EMA_med_list$international_non_proprietary_name_inn_common_name, perl=TRUE)), collapse = "|")
  #
  # EMA_meds <- paste(EMA_generic, EMA_substance, EMA_name,  collapse = "|")

# mod_data2 <- mod_data %>%
#   mutate(EMA_meds= sapply(str_extract_all(mod_data$`Immunosuppressive therapy`,regex(EMA_meds , ignore_case = TRUE)),
#                           function(x) paste(x,collapse=" | ")))

#first make a list of medication names to extract


 data_temp <- data %>%
   mutate(med_names = sapply(str_extract_all(col2,regex(EMA_name, ignore_case = TRUE)),
                           function(x) paste(x,collapse=" | ")))
}



EMA_med_list_2 <- EMA_med_list %>% mutate(medicine_name= strsplit(as.character(medicine_name), "\\(previously known as |\\(previously |/|\\(")) %>%
   unnest(medicine_name) %>%
  mutate(medicine_name = gsub("\\)|in Italy: ", "", trimws(medicine_name)))
EMA_name <- paste(EMA_med_list_2$medicine_name, collapse = "|")
 #Icandra  previously Vildagliptin   metformin
 #Dafiro HCT
 #Briviact  in Italy: Nubriveo
 test <- data.frame(col1 = c("A", "B", "C", "D", "E", "F"), col2= c("Vildagliptin", "something something Nubriveo", "Nubriveo", "Dafiro", "Dafiro HCT", "Briviact  in Italy: Nubriveo "))


 test <- test %>%
   mutate(EMA_meds= sapply(str_extract_all(col2,regex(EMA_name, ignore_case = TRUE)),
                             function(x) paste(x,collapse=" | "))) %>%
   separate_rows(EMA_meds, sep = " \\| ")
