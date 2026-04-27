# Upload libraries
library(dplyr)
library(tidyr)
library(writexl)

setwd("")
getwd()

#1. Upload data from Singh and Persson (2024)
#Trade flow - physical
Deforestation <- read_excel("Singh and Persson (2024)/Singh et al 2024 - Commodity-driven deforestation, carbon emissions & trade 2001-2022.xlsx", 
                                                                                               sheet = "Trade flows-physical")

#2. Extract the commodity list to write on excel the corresponding land-use type (from FAO)
commodities <- unique(Deforestation$Commodity)
commodities <- data.frame(Commodity = commodities)

#3. Upload the commodity list and merge it with the DF data
#Clean the commodity data
commodities <- read_excel("commodities.xlsx")

commodities <- commodities %>% dplyr::select(-c(`Group name`, Note))

Deforestation <- Deforestation %>%
  left_join(commodities, by = "Commodity")

#4. Compute the total deforested land in the 5 years prior to attribution to compute the occupation impact oon biodiversity
Deforestation$OccupiedLand <- Deforestation$`Deforestation risk (ha)` * 5

#5. Upload CF data
#Clean the dataset for transformation
CFs_transf_average <- read_excel("Verones et al. (2020)/CFs_land_Use_average.xlsx", 
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
CFs_occ_average <- read_excel("Verones et al. (2020)/CFs_land_Use_average.xlsx", 
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
#a. Brunei Darussalam, Czech Republic, Congo DRC, Mexico, The Former Yugoslav Republic of Macedonia, Congo, Russian Federation, Sao Tome and Principe in CFs
#transformation
CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Brunei Darussalam", "Brunei", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Czech Republic", "Czechia", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Congo DRC", "Democratic Republic of the Congo", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Mexico", "México", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "The Former Yugoslav Republic of Macedonia", "North Macedonia", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Congo", "Republic of the Congo", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country))

CFs_transf_long <- CFs_transf_long %>%
  mutate(Country = ifelse(Country == "Sao Tome and Principe", "São Tomé and Príncipe", Country))

#occupation
CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Brunei Darussalam", "Brunei", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Czech Republic", "Czechia", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Congo DRC", "Democratic Republic of the Congo", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Mexico", "México", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "The Former Yugoslav Republic of Macedonia", "North Macedonia", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Congo", "Republic of the Congo", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country))

CFs_occ_long <- CFs_occ_long %>%
  mutate(Country = ifelse(Country == "Sao Tome and Principe", "São Tomé and Príncipe", Country))

#b. Missing in CF: Cabo verde

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

#7. Merge the dataset with ISO3 code from FAOSTAT
Country.Code <- read.csv("~/Desktop/Ricerche/Colonial Origin/FAOSTAT/Trade_DetailedTradeMatrix_E_All_Data/Country Code.csv")
Country.Code <- Country.Code %>% dplyr::select(Country, ISO3.Code) %>%
  distinct()

#Correct the country names in Country.Code
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Bolivia (Plurinational State of)", "Bolivia", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Brunei Darussalam", "Brunei", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Iran (Islamic Republic of)", "Iran", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Lao People's Democratic Republic", "Laos", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Mexico", "México", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Republic of Moldova", "Moldova", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Netherlands (Kingdom of the)", "Netherlands", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Democratic People's Republic of Korea", "North Korea", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Congo", "Republic of the Congo", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Russian Federation", "Russia", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Sao Tome and Principe", "São Tomé and Príncipe", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Republic of Korea", "South Korea", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Eswatini", "Swaziland", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Syrian Arab Republic", "Syria", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "United Republic of Tanzania", "Tanzania", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Türkiye", "Turkey", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "United States of America", "United States", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Venezuela (Bolivarian Republic of)", "Venezuela", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Viet Nam", "Vietnam", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "China, Taiwan Province of", "Taiwan", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Micronesia (Federated States of)", "Micronesia", Country))
Country.Code <- Country.Code %>%
  mutate(Country = ifelse(Country == "Micronesia (Federated States of)", "Micronesia", Country))

Deforestation <- Deforestation %>%
  mutate(`Producer country` = ifelse(`Producer country` == "China", "China, mainland", `Producer country`))
Deforestation <- Deforestation %>%
  mutate(`Consumer country` = ifelse(`Consumer country` == "China", "China, mainland", `Consumer country`))

#Join the datasets
Deforestation <- Deforestation %>%
  left_join(Country.Code, by = c("Producer country" = "Country")) %>%
  rename("Producer ISO" = ISO3.Code)

Deforestation <- Deforestation %>%
  left_join(Country.Code, by = c("Consumer country" = "Country")) %>%
  rename("Consumer ISO" = ISO3.Code)

#Check if the merge didn't happen for some values
Deforestation %>%
  filter(is.na(`Producer ISO`)) %>%
  distinct(`Producer country`) %>%
  slice_head(n = 22)

Deforestation %>%
  filter(is.na(`Consumer ISO`)) %>%
  distinct(`Consumer country`) %>%
  slice_head(n = 22)


#Deal with aggregated countries
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


