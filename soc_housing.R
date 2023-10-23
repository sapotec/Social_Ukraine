packages <- c("ggbump", "tidyverse", "data.table", "sf", "plotly", "leaflet",
              "stringi", "igraph", "ggraph", "rgeos", "rworldmap", "stringr")

# Identify the packages that need to be installed
p_to_inst <- packages[!packages %in% installed.packages()[, "Package"]]

# Install the missing packages
if (length(p_to_inst)) install.packages(p_to_inst)

# Load the libraries
lapply(packages, require, character.only = TRUE)


reestr_12_19 <- "resources/reestrsubsidii_12_19.zip"
reestr_12_20 <- "resources/reestrsubsidii_12_20.zip"
reestr_12_21 <- "resources/reestrsubsidii_12_21.zip"
reestr_12_22 <- "resources/reestrsubsidii_12_22.zip"

subs_raw_12_19 <- fread(utils::unzip(reestr_12_19, overwrite = TRUE),
                        dec = ".", encoding = "UTF-8",
                        na.strings = c(NA_character_, "NULL")) %>%
  rename(identifier = 1, addressAdminUnit = 2, addressPostName = 3,
         addressThoroughfare = 4, addressLocatorDesignator = 5,
         addressflat = 6, stateofsubsidy = 7,
         SubsidyamountpermonthcommunalservicesUAH = 8,
         SubsidyamountperyearliquefiedgasUAH = 9,
         Locationofrecipientspersonalfile = 10,
         Dateoffirstgrantappointment = 11,
         Entryintoforceoflastsubsidy = 12,
         Endoflastsubsidyappointment = 13,
         TheamountofsubsidyinthenonheatingperiodUAH = 14,
         TheamountofsubsidyintheheatingperiodUAH = 15) %>%
  mutate(across(c(8, 9, 14, 15), as.numeric),
         across(c(11:13), ~ as.IDate(., format = "%d.%m.%Y")),
         date_stamp = as.IDate("2019-12-01", format = "%Y-%m-%d")) %>%
  select(-identifier) %>%
  distinct()

subs_raw_12_20 <- fread(utils::unzip(reestr_12_20, overwrite = TRUE),
                        dec = ".", encoding = "UTF-8",
                        na.strings = c(NA_character_, "NULL")) %>%
  mutate(across(c(8, 9, 14, 15), as.numeric),
         date_stamp = as.IDate("2020-12-01", format = "%Y-%m-%d")) %>%
  select(-identifier) %>%
  distinct()

subs_raw_12_21 <- fread(utils::unzip(reestr_12_21, overwrite = TRUE),
                        dec = ".", encoding = "UTF-8",
                        na.strings = c(NA_character_, "NULL")) %>%
  mutate(across(c(8, 9, 14, 15), as.numeric),
         across(10, str_to_upper),
         date_stamp = as.IDate("2021-12-01", format = "%Y-%m-%d")) %>%
  select(-identifier) %>%
  distinct()

subs_raw_12_22 <- fread(utils::unzip(reestr_12_22, overwrite = TRUE),
                        dec = ".", encoding = "UTF-8",
                        na.strings = c(NA_character_, "NULL")) %>%
  mutate(across(c(8, 9, 14, 15), as.numeric),
         across(1, as.integer),
         across(10, str_to_upper),
         across(c(11:13), ~ as.IDate(., format = "%Y-%m-%d")),
         date_stamp = as.IDate("2022-12-01", format = "%Y-%m-%d")) %>%
  select(-identifier) %>%
  distinct()

subs_raw <- bind_rows(subs_raw_12_19, subs_raw_12_20, subs_raw_12_21,
                      subs_raw_12_22) %>%
  distinct(across(-date_stamp), .keep_all = TRUE)

subs_raw <- subs_raw %>%
  rename(oblast = 1, town = 2, street = 3, address = 4,
         flat = 5, status = 6, monthly_amount = 7,
         LPG_amount_year = 8, issuer = 9, first_assignment = 10,
         from = 11, to = 12, amount_non_heating = 13,
         amount_heating = 14)

pat <- c("ОБЛАСТЬ", "ОБЛ.", "М.")
repl <- c("", "")

subsidies <- subs_raw %>%
  mutate(across(c(oblast, street),
                ~ stri_replace_all_fixed(., pat, repl, vectorize_all = FALSE)),
         across(c(town, street, oblast), ~ str_to_title(.x)),
         across(c(town, street, oblast), ~ trimws(.x)),
         across(c(oblast, status), as.factor)) %>%
  filter(!is.na(from), !is.na(to),
         !is.na(amount_heating) | !is.na(amount_non_heating),
         amount_heating > 0 | amount_non_heating > 0,
         from < to) %>%
  distinct(., oblast, town, street, address,
           flat, status, first_assignment, from, to,
           .keep_all = TRUE) %>%
  relocate(date_stamp, .before = oblast)

fwrite(subsidies, "reestr_subsidii_16_23.csv",
       sep = ";", quote = FALSE, col.names = TRUE)
