# Upload libraries
library(dplyr)
library(tidyr)
library(writexl)
library(countrycode)
library(readxl)

setwd("")
getwd()

#1. Upload data from Singh and Persson (2024)
#Trade flow - physical
Deforestation <- read_excel("Singh et al 2024 - Commodity-driven deforestation, carbon emissions & trade 2001-2022.xlsx", 
                                                                                               sheet = "Trade flows-physical")

#2. Upload the commodity list and merge it with the DF data
#Clean the commodity data
commodities <- read_excel("commodities.xlsx")

commodities <- commodities %>% dplyr::select(-c(`Group name`, Note))

Deforestation <- Deforestation %>%
  left_join(commodities, by = "Commodity")

#3. Compute the total deforested land in the 5 years prior to attribution to compute the occupation impact oon biodiversity
Deforestation$OccupiedLand <- Deforestation$`Deforestation risk (ha)` * 5

#4. Upload CF data
#Clean the dataset for transformation
CFs_transf_average <- read_excel("CFs_land_Use_average.xlsx", 
                                   sheet = "transf. avg country 100y", skip = 1)

CFs_transf_average <- CFs_transf_average %>% rename(Country = ...1, `Annual crops median` = `Annual crops`, `Annual crops lower 95%` = ...3, `Annual crops upper 95%` = ...4, `Permanent crops median` = `Permanent crops`, `Permanent crops lower 95%` = ...6, `Permanent crops upper 95%` = ...7, `Pasture median` = Pasture, `Pasture lower 95%` = ...9, `Pasture upper 95%` = ...10)

CFs_transf_average <- CFs_transf_average[-1,]
CFs_transf_average <- CFs_transf_average %>% dplyr::select(Country: `Pasture upper 95%`)

CFs_transf_long <- CFs_transf_average %>%
  pivot_longer(
    cols = -Country,
    names_to = "CF_type",
    values_to = "CF"
  ) %>%
  mutate(
    Crop_type = case_when(
      grepl("^Annual crops", CF_type) ~ "Annual crop",
      grepl("^Permanent crops", CF_type) ~ "Permanent crop",
      grepl("^Pasture", CF_type) ~ "Pasture",
      TRUE ~ NA_character_
    ),
    Confidence_interval = case_when(
      grepl("median", CF_type, ignore.case = TRUE) ~ "Median",
      grepl("lower 95%", CF_type, ignore.case = TRUE) ~ "Lower 95%",
      grepl("upper 95%", CF_type, ignore.case = TRUE) ~ "Upper 95%",
      TRUE ~ NA_character_
    ),
    CF = as.numeric(CF)  # opzionale, se vuoi convertire in numerico
  )

CFs_transf_long <- CFs_transf_long[,-2]

#Clean the dataset for occupation
CFs_occ_average <- read_excel("CFs_land_Use_average.xlsx", 
                                 sheet = "occupation average country", skip = 1)

CFs_occ_average <- CFs_occ_average %>% rename(Country = ...1, `Annual crops median` = `Annual crops`, `Annual crops lower 95%` = ...3, `Annual crops upper 95%` = ...4, `Permanent crops median` = `Permanent crops`, `Permanent crops lower 95%` = ...6, `Permanent crops upper 95%` = ...7, `Pasture median` = Pasture, `Pasture lower 95%` = ...9, `Pasture upper 95%` = ...10)

CFs_occ_average <- CFs_occ_average[-1,]
CFs_occ_average <- CFs_occ_average %>% dplyr::select(Country: `Pasture upper 95%`)

CFs_occ_long <- CFs_occ_average %>%
  pivot_longer(
    cols = -Country,
    names_to = "CF_type",
    values_to = "CF"
  ) %>%
  mutate(
    Crop_type = case_when(
      grepl("^Annual crops", CF_type) ~ "Annual crop",
      grepl("^Permanent crops", CF_type) ~ "Permanent crop",
      grepl("^Pasture", CF_type) ~ "Pasture",
      TRUE ~ NA_character_
    ),
    Confidence_interval = case_when(
      grepl("median", CF_type, ignore.case = TRUE) ~ "Median",
      grepl("lower 95%", CF_type, ignore.case = TRUE) ~ "Lower 95%",
      grepl("upper 95%", CF_type, ignore.case = TRUE) ~ "Upper 95%",
      TRUE ~ NA_character_
    ),
    CF = as.numeric(CF)  # opzionale, se vuoi convertire in numerico
  )

CFs_occ_long <- CFs_occ_long[,-2]

#5. Merge the dataset

#Correct the keys for the merge function
# a. Harmonize country names in CFs
cf_name_recode <- c(
  "Brunei Darussalam" = "Brunei",
  "Czech Republic" = "Czechia",
  "Congo DRC" = "Democratic Republic of the Congo",
  "Mexico" = "México",
  "The Former Yugoslav Republic of Macedonia" = "North Macedonia",
  "Congo" = "Republic of the Congo",
  "Russian Federation" = "Russia",
  "Sao Tome and Principe" = "São Tomé and Príncipe"
)

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = recode(Country, !!!cf_name_recode))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = recode(Country, !!!cf_name_recode))

# b. Missing in CF: Cabo Verde

#c. Serbia and Montenegro, and Sudan and South Sudan are togheter in the Singh and Persson dataset
#c.1 Compute average for Serbia and Montenegro
serbia_montenegro_transf <- CFs_transf_long %>%
  filter(Country %in% c("Serbia", "Montenegro")) %>%
  group_by(Crop_type, Confidence_interval) %>%
  summarise(
    CF = mean(as.numeric(CF), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = "Serbia and Montenegro")

serbia_montenegro_occ <- CFs_occ_long %>%
  filter(Country %in% c("Serbia", "Montenegro")) %>%
  group_by(Crop_type, Confidence_interval) %>%
  summarise(
    CF = mean(as.numeric(CF), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = "Serbia and Montenegro")

#c.2 Compute average for Sudan and South Sudan
sudan_combined_transf <- CFs_transf_long %>%
  filter(Country %in% c("Sudan", "South Sudan")) %>%
  group_by(Crop_type, Confidence_interval) %>%
  summarise(
    CF = mean(as.numeric(CF), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = "Sudan and South Sudan")

sudan_combined_occ <- CFs_occ_long %>%
  filter(Country %in% c("Sudan", "South Sudan")) %>%
  group_by(Crop_type, Confidence_interval) %>%
  summarise(
    CF = mean(as.numeric(CF), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = "Sudan and South Sudan")

#c.3 Add new rows to the CF dataframe
CFs_transf_long <- bind_rows(CFs_transf_long, serbia_montenegro_transf, sudan_combined_transf)
CFs_occ_long <- bind_rows(CFs_occ_long, serbia_montenegro_occ, sudan_combined_occ)

#Rename the CF per each dataset
CFs_transf_long <- CFs_transf_long %>% rename("CF trans avg" = CF)
CFs_occ_long <- CFs_occ_long %>% rename("CF occ avg" = CF)

#Compute deforestation risk in m2
Deforestation <- Deforestation %>%
  mutate(`Deforestation risk (m2)` = `Deforestation risk (ha)` * 10000)

Deforestation <- Deforestation %>%
  mutate(`OccupiedLand (m2)` = OccupiedLand * 10000)

#Merge the datasets
Deforestation <- merge(
  Deforestation,
  CFs_transf_long,
  by.x = c("Producer country", "Crop type"),
  by.y = c("Country", "Crop_type"),
  all.x = TRUE
)

Deforestation <- merge(
  Deforestation,
  CFs_occ_long,
  by.x = c("Producer country", "Crop type", "Confidence_interval"),
  by.y = c("Country", "Crop_type", "Confidence_interval"),
  all.x = TRUE
)

#Check for values in which the merge didn't happen
Deforestation %>% 
  filter(is.na(`CF trans avg`)) %>% distinct(`Producer country`)

Deforestation %>% 
  filter(is.na(`CF occ avg`)) %>% distinct(`Producer country`)

#Delete the data for Cabo Verde for which no CF exist
Deforestation <- Deforestation%>%
  filter(!is.na(`CF trans avg`))

#6. Compute the Biodiversity Footprint for average land transformation
Deforestation$BR_trans_avg <- Deforestation$`Deforestation risk (m2)`*Deforestation$`CF trans avg`
Deforestation$BR_occ_avg <- Deforestation$`OccupiedLand (m2)`*Deforestation$`CF occ avg`
Deforestation$BR_total <- Deforestation$BR_trans_avg + Deforestation$BR_occ_avg

#7. Add ISO3 codes directly with countrycode
iso_custom_match <- c(
  "Bolivia" = "BOL",
  "Brunei" = "BRN",
  "China, mainland" = "CHN",
  "Iran" = "IRN",
  "Laos" = "LAO",
  "México" = "MEX",
  "Moldova" = "MDA",
  "Netherlands" = "NLD",
  "North Korea" = "PRK",
  "Republic of the Congo" = "COG",
  "Russia" = "RUS",
  "São Tomé and Príncipe" = "STP",
  "South Korea" = "KOR",
  "Swaziland" = "SWZ",
  "Syria" = "SYR",
  "Tanzania" = "TZA",
  "Turkey" = "TUR",
  "United Kingdom" = "GBR",
  "United States" = "USA",
  "Venezuela" = "VEN",
  "Vietnam" = "VNM",
  "Taiwan" = "TWN",
  "Micronesia" = "FSM"
)

Deforestation <- Deforestation %>%
  mutate(
    `Producer ISO` = countrycode(
      `Producer country`,
      origin = "country.name",
      destination = "iso3c",
      custom_match = iso_custom_match,
      warn = TRUE
    ),
    `Consumer ISO` = countrycode(
      `Consumer country`,
      origin = "country.name",
      destination = "iso3c",
      custom_match = iso_custom_match,
      warn = TRUE
    )
  )

# Check if the conversion didn't happen for some values
Deforestation %>%
  filter(is.na(`Producer ISO`)) %>%
  distinct(`Producer country`) %>%
  slice_head(n = 22)

Deforestation %>%
  filter(is.na(`Consumer ISO`)) %>%
  distinct(`Consumer country`) %>%
  slice_head(n = 22)

# Deal with aggregated countries
Deforestation <- Deforestation %>%
  mutate(
    `Producer ISO` = case_when(
      `Producer country` == "Serbia and Montenegro" ~ "SRB-MNE",
      `Producer country` == "Sudan and South Sudan" ~ "SDN-SSD",
      TRUE ~ `Producer ISO`
    ),
    `Consumer ISO` = case_when(
      `Consumer country` == "Serbia and Montenegro" ~ "SRB-MNE",
      `Consumer country` == "Sudan and South Sudan" ~ "SDN-SSD",
      TRUE ~ `Consumer ISO`
    )
  )

#Save it
write_xlsx(Deforestation, path = "BiodiversityRisk_data.xlsx")


