# library(tidyverse)
# #library(plyr)
# library(tidyr)
# library(dplyr)
# library(readxl)
# library(stringr)
# #library(magrittr)
# #library(stringi)
# library(janitor)
# #library(gsubfn)
# #setwd("/home/ni3853/Documents/quartoproj")

source("analysis/R_functions/project_libraries.R")
raw_data <- read_excel("data/Masterfile_10.06.2022_FinalPaper.xlsx")


ATC_codes <- clean_names(read_excel("data/ATC codes.xlsx")) %>%
  filter(!is.na(atc_code)) %>%
  separate_rows(brand_name_s, sep = ", ") %>%
  separate_rows(name, sep = ", ") %>%
  separate_rows(immuno_vax_labels, sep = ", ") %>%
  unique()



EMA_med_list <- clean_names(read_excel("data/Medicines_output_european_public_assessment_reports.xlsx",
                                       skip = 8)) %>%
  filter(category == "Human") %>%
  filter(authorisation_status == "Authorised") %>%
  filter(!is.na(international_non_proprietary_name_inn_common_name))


#findimmsup <- "(?<= \\d{4}\\.) | (?<=\\d{4}\\,\\s)| (?<= \\d{4}\\.\\t) | (?<=\\d{4}\\,\\t) |
#(?<=\t) | (then) | ( \\,\\s(?![\\d{2}\\.\\d{2}]) ) | (?<=\\s\\+\\s) | (;\\s(?=[A-Z])) |  (\\,(?=\\s\\w))  "


#findimmsup <- "(?<= \\d{4}\\.(?![\\d{2}\\.\\d{2}]))|(?<=\\d{4}\\,\\s(?![\\d{2}\\.\\d{2}]))| (?<=\t) | (then) |(?<=\\,\\s(?![\\d{2}\\.\\d{2}]|[^(]*\\)|[a-z]))| (?<=;\\s(?=[A-Z]))|(?<=\\s\\d{1}-\\d{1}-\\d{1}\\s)|(?<=\\s\\d{4}-\\d{1}-\\d{4}\\s)|(?<=and\\s(?![\\d{1}]))|(\\+)"

#https://cheatography.com/davechild/cheat-sheets/regular-expressions/

# findimmsup <-  paste(c("(?<= \\d{4}\\.(?![\\d{2}\\.\\d{2}]))", "(?<=\\d{4}\\,\\s(?![\\d{2}\\.\\d{2}]))", "(then)", "\\sand\\s","\\s(E|e)t\\s", "(?<=\\,\\s(?![\\d{2}\\.\\d{2}]|[^(]*\\)|[a-z]))",
#                        "(?<=;\\s(?=[A-Z]))", "(?<=\\s\\d{1}-\\d{1}-\\d{1}\\s)", "(?<=/(?i)[j|m|s|w|d|mois|jour|semaine|week|day](?-i))[\\,]*\\s(?=[A-Z][^top])",
#                        #"(?<=\\s\\d{4}-\\d{1}-\\d{4}\\s)",
#                        "(?<=and\\s(?![\\d{1}]))", "\\+[^)]*\\(", "\\,\\s(?=\\d{4}\\:)", "(?<=(?i)[MG])\\s\\+\\s(?=[A-Z])"), collapse="|")
#
# findmg <- paste(c( "(?i)\\b[0-9]+(\\,*|\\.*)[0-9]*[ ]*[m]*g"  ), collapse="|")
#
# finddose <- paste(c("(?i)\\b/[j|s|w|d|x|m|k][a-z]+\\b","(?i)\\b[0-9][ ]*x[ ]*/[ ]*[0-9]*[a-z]+\\b",  "(?i)\\b[0-9]+[ ]*x\\b", "(?i)\\bx[ ]*[0-9]+\\b",
#                     "\\b[0-9]+(\\,*|\\.*)[0-9]*\\-[0-9]+(\\,*|\\.*)[0-9]*\\-[0-9]+(\\,*|\\.*)[0-9]*\\b",
#                     "(?i)\\bweek[ly]*\\b","(?i)\\bmonth[ly]*\\b"), collapse="|")
#
#
# findmed <- paste(c("(?i)\\b([^ :]*?[^gr|h|J| d]a(n|b|d|t)e*)\\b",  "(?i)\\b([^ :]*?[^a|h| ]i(n|b|d|c|t)e*)\\b",  "(?i)\\b([^ :\\)]*?[^ l|N|i|c]o(l|n|b|d|t)e*)\\b",
#                    "(?i)\\b([^ :\\)]*?[^ |h]e(l|n|b|t)e*)\\b", "(?i)\\b([^ :]*?mus)\\b", "(?i)\\b((B|b)eva[^ :]*?)\\b",
#                    "(?i)\\b([^ :]*?(a|e)x)\\b", "(?i)\\b([^ :]*?graf)\\b", "(?i)\\b([^ :]*?lan)\\b", "(?i)\\b(?i)ipi nivo\\b",
#                    "(?i)\\b([^ :]*?[^ ]un)\\b", "(?i)\\b([^ :]*?une)\\b", "(?i)\\b([^ :]*?exa)\\b", "(?i)\\b([^ :]*?cept)\\b", "(?i)\\b([^ :]*?ra(-c| ))\\b", "(?i)\\b([^ :]*?bro)\\b"), collapse="|")


mod_data <- raw_data %>%
  #dplyr::select(-c(2:23)) %>%
  mutate(ptgroup = str_extract(`PTID 4`, "\\D{3}")) %>%
  mutate(`Immunosuppressive therapy` = gsub(":", ": ", `Immunosuppressive therapy`, perl=TRUE)) %>%
  mutate(`Immunosuppressive therapy` = gsub("\\d\\s(?i)x", "\\dx", `Immunosuppressive therapy`, perl=TRUE)) %>%
  mutate(`Immunosuppressive therapy` = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", `Immunosuppressive therapy`, perl=TRUE)) %>%
  mutate(`Immunosuppressive therapy` = gsub("\b(?:[a-z][io]ne)\b(*SKIP)(*FAIL)|\\b(\\pL)",  "\\U\\1", `Immunosuppressive therapy`, perl=TRUE)) %>%
  mutate(`Analysis date` = format(as.Date(`Analysis date`), "%d/%b/%Y"))


# ATC_codes$list1 <- paste('"',gsub("‘|,|’| ", "",ATC_codes$name),'"', " = ",'"',ATC_codes$atc_code,'"')
#
# atccodellist1 <-gsub(" ", "",  paste(ATC_codes$list1, collapse = ","))
#
# ATC_codes_str <- gsub(" ", "",paste("c(", paste("'",ATC_codes$atc_code,"'", collapse = ",")))
# ATC_name_str <- gsub(" ", "", paste("c(",paste("'",toupper(ATC_codes$name),"'", collapse = ",")))

ATC_name <- gsub(", ", "|", paste(na.omit(ATC_codes$name), collapse = "|"))
ATC_brand <- gsub(", ", "|", paste(na.omit(ATC_codes$brand_name_s), collapse = "|"))
ATC_meds <- paste(ATC_name, ATC_brand, collapse = "|")
ATC_label <- gsub(", ", "|", paste(na.omit(ATC_codes$immuno_vax_labels), collapse = "|"))
ATC_meds <- paste(ATC_meds, ATC_label, collapse = "|")


EMA_name <- paste(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)", "", EMA_med_list$medicine_name, perl=TRUE)), collapse = "|")
EMA_substance <- paste(gsub("-|,|;|\\.|\\/|\\)|\\(|\\[|\\]", " ",gsub("\\s*\\(*\\([^\\)]*\\)\\)*", "", EMA_med_list$active_substance, perl=TRUE)), collapse = "|")
EMA_generic <- paste(gsub("-|,|;", " ",gsub("\\s+\\([^\\)]*\\)", "", EMA_med_list$international_non_proprietary_name_inn_common_name, perl=TRUE)), collapse = "|")

EMA_meds <- paste(EMA_generic, EMA_substance, EMA_name,  collapse = "|")
#EMA_actsub_ext <- paste(gsub("-|,|;", " ",gsub("\\s+\\([^\\)]*\\)", "", EMA_med_list$international_non_proprietary_name_inn_common_name,EMA_med_list$active_substance,   perl=TRUE)), collapse = "|")

#recode names

#ATC codelist
atcnames <- toupper(ATC_codes$name)
atcbrands <- toupper(ATC_codes$brand_name_s)
atclabels <- toupper(ATC_codes$immuno_vax_labels)


#convert brand names into generic names
ATC2 <- ATC_codes %>% filter(!is.na(brand_name_s))
names(ATC2$atc_code) <- toupper(ATC2$brand_name_s)
atc_code_to_brand <-unlist(list(ATC2$atc_code))

#convert immvax labels into generic names
ATC2 <- ATC_codes %>% filter(!is.na(immuno_vax_labels))
names(ATC2$atc_code) <- toupper(ATC2$immuno_vax_labels)
atc_code_to_label <-unlist(list(ATC2$atc_code))

#convert generic names into atc codes
names(ATC_codes$atc_code) <- toupper(ATC_codes$name)
atc_code_to_name <-unlist(list(ATC_codes$atc_code))



#EMA codelist
emanames <- toupper(EMA_med_list$international_non_proprietary_name_inn_common_name)
emabrand <- toupper(EMA_med_list$medicine_name)
emasubst <- toupper(EMA_med_list$active_substance)

names(EMA_med_list$atc_code) <- toupper(EMA_med_list$international_non_proprietary_name_inn_common_name)
ema_code_to_name <-unlist(list(EMA_med_list$atc_code))

names(EMA_med_list$atc_code) <- toupper(EMA_med_list$medicine_name)
ema_code_to_brand <-unlist(list(EMA_med_list$atc_code))

names(EMA_med_list$atc_code) <- toupper(EMA_med_list$active_substance)
ema_code_to_subst <-unlist(list(EMA_med_list$atc_code))

mod_data2 <- mod_data %>%
  mutate(EMA_meds= sapply(str_extract_all(mod_data$`Immunosuppressive therapy`,regex(EMA_meds , ignore_case = TRUE)),
                          function(x) paste(x,collapse=" | ")))  %>%
  mutate(codelist_meds= sapply(str_extract_all(mod_data$`Immunosuppressive therapy`,regex(ATC_meds, ignore_case = TRUE)),
                               function(x) paste(x,collapse=" | ")))  %>%
  separate_rows(EMA_meds, sep = " \\| ") %>%
  separate_rows(codelist_meds, sep = " \\| ") %>%
  mutate(atc1 = case_when(
    toupper(codelist_meds) %in% atcnames ~ recode(toupper(codelist_meds), !!!atc_code_to_name, .missing = codelist_meds),
    toupper(codelist_meds) %in% atcbrands ~ recode(toupper(codelist_meds), !!!atc_code_to_brand, .missing = codelist_meds),
    toupper(codelist_meds) %in% atclabels ~ recode(toupper(codelist_meds), !!!atc_code_to_label, .missing = codelist_meds)
  ),
  ema1 = case_when(
    toupper(EMA_meds) %in% emanames ~ recode(toupper(EMA_meds), !!!ema_code_to_name, .missing = EMA_meds),
    toupper(EMA_meds) %in% emabrand ~ recode(toupper(EMA_meds), !!!ema_code_to_brand, .missing = EMA_meds),
    toupper(EMA_meds) %in% emasubst ~ recode(toupper(EMA_meds), !!!ema_code_to_subst, .missing = EMA_meds)
  )) %>%
  group_by(`PTID 4`) %>%
  mutate(EMA_meds= gsub("| ", "", paste(unique(EMA_meds), collapse =" | "))) %>%
  mutate(codelist_meds= gsub("| ", "", paste(unique(codelist_meds), collapse =" | "))) %>%
  mutate(atc_codelist = gsub("| ", "", paste(unique(atc1), collapse =" | ")) ) %>%
  mutate(ema_codelist = gsub("| ", "", paste(unique(ema1), collapse =" | ")) ) %>%
  dplyr::select(-c(atc1, ema1)) %>%
  ungroup() %>%
  unique()




write_csv(mod_data2, "analysis/derived_data/ImmunoVax_ATCcodes.csv")
