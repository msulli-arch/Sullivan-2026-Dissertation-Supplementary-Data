# Script 01: CONTEXT INFORMATION TO ORGANIZE CERMAIC DATA ######
library(tidyverse)
library(readxl)
library(writexl)

# 116 Broad St. (John Rutledge House) Context Work ======
# DAACS Context Data
Rutledge_context <- read_csv("data_raw/116_Broad/Rutledge_context_info.csv")
# Additional Context Information not in DAACS 
Rutledge_additional_context <- read_xlsx("data_raw/116_Broad/Rutledge_FS.xlsx")

# Select columns for provenience
print(colnames(Rutledge_context))
Rut_context_phase <- select(Rutledge_context, "F S Number", "Level Designation", "Excavator Phase", "Data Entry Notes") %>%
# Add Excavator Dates Descriptions
  mutate(Period = case_when(
    `Excavator Phase` == 1 ~ "Post 1820" ,
    `Excavator Phase` == 2 ~"1760-1820" ,
    `Excavator Phase` == 3 ~ "Pre Rutledge" ,
    is.na(`Excavator Phase`) ~ "Not Applicable" 
  ),
  # continue Mutate function to get Charleston Museum's ARL Info into one column
  ObjectID = (str_sub(`Data Entry Notes`, 1, 9)),
  # change Contexts without ARL information  
  ObjectID =ifelse(`ObjectID` %in% "This cont", "Not Applicable" , ObjectID),
  #Add ARL Information into a column, using str_locate
  ARL= (str_locate(`Data Entry Notes`, "\\.")),
  # R hates the way those data columns were named, so rework them
  ARLs= as.numeric(ARL[,"start"]),
  `Data Entry Notes` = 
             case_when(ARLs == 10 ~ (str_sub(`Data Entry Notes`, 12)), 
                       ARLs != 10 ~ `Data Entry Notes`)) %>%
  #Remove extraneous columns
  select(-`ARL`, -`ARLs`, -`Excavator Phase`)

# get additional information for future analysis from other context spreadsheet  
Rut_add_info <- Rutledge_additional_context %>%
  rename(`F S Number`= FS) %>% 
  select(c("F S Number", "Context", "CATALOGED", "FSGroup"))

# simplify DAACS data for final combination
Rut_context <- Rutledge_context %>%
  select(-c("Level Designation", "Excavator Phase", "Data Entry Notes"))

# make a combined context table for Charleston Museum
Rut_context_final <-left_join(Rut_context, Rut_context_phase, by = "F S Number") %>%
  left_join(., Rut_add_info, by = c("F S Number", "Context")) %>%
  # add associated Test Units to analyzed Feature contexts
  mutate(`Quadrat ID` = case_when(Context == "12" ~ "TU01",
                               Context == "33" ~ "TU01",
                               TRUE ~ `Quadrat ID`),
  `F S Number` = as.character(`F S Number`),
  `Feature Number` = as.character(`Feature Number`))

write_xlsx(Rut_context_final, "data_processed/spreadsheets/contexts/Rutledge_contexts_2026.xlsx")

# 87 Church Street (Heyward-Washington House) Context Work ####

# Load DAACS contexts for HWN Privy
HWN_context <- read_xlsx("data_raw/87_Church/HWN_DAACS_context.xlsx")
# Load additional context info for HWN Privy
HWN_additional_info <- read_xlsx("data_raw/87_Church/HWN_FS.xlsx")

# select columns for provenience
print(colnames(HWN_context))
HWN_context_final <- left_join(HWN_context, HWN_additional_info, by = "Context") %>%
  select(-`F S Number.x`) %>%
  rename(`F S Number` = 'F S Number.y')
write_xlsx(HWN_context_final, "data_processed/spreadsheets/contexts/HWN_contexts_2026.xlsx")

# Puente (SA 24) Context Work ####
# This site is more involved since not all contexts were analyzed

# Load DAACS context info for contexts analyzed and entered into database
Puente_context <- read_xlsx("data_raw/Puente/Puente_DAACS_context.xlsx") %>%
  mutate(`F S Number` = as.character(`F S Number`))
# Load FLMNH specific information for all contexts at site
Puente_additional_info <- read_xlsx("data_raw/Puente/Puente_FS_Contexts.xlsx", sheet = "FS DAACS Format")
# Select subset of desired columns
Puente_selected_info <- select(Puente_additional_info, c("Unit", "F S Number", "FS Name", "FSGroup", "ASSOCIATED LEVEL", "CATALOGED", "Period"))

# Combine the tables
Puente_context_final <- full_join(Puente_context, Puente_selected_info, by = "F S Number") %>%
  mutate(`Feature Number` = as.character(`Feature Number`),
          `Project Name` = "Puente"
         )
# save
write_xlsx(Puente_context_final, "data_processed/spreadsheets/contexts/Puente_Contexts_2026.xlsx")

# De la Cruz (DLC) (SA 16-23) Context Work ####
#Load DLC DAACS data
DLC_context <- read_xlsx("data_raw/DLC/DLC_DAACS_contexts.xlsx")
# Load additional info with contexts of interest, MA work, as updated by Aaron Ellrich for DAACS 
DLC_additional_info <- read_xlsx("data_raw/DLC/DLC_DAACS_PROVS_Ellrich.xlsx") 
#select columns and transform basic data
DLC_selected_info <- DLC_additional_info %>%
  select(c("F S Number", "Period", "CATALOGED", "Unit"))

# make a combined table
DLC_context_final <- full_join(DLC_context, DLC_selected_info, by = "F S Number") %>%
  mutate(`Feature Number` = as.character(`Feature Number`))

# check if Unit and Quadrat ID have different values
print(DLC_context_final %>%
        filter(!is.na(`Quadrat ID`)) %>%
        filter(!`Quadrat ID` %in% Unit))
  # all good!
write_xlsx(DLC_context_final, "data_processed/spreadsheets/contexts/DLC_Contexts_2026.xlsx")

# MA thesis site Context Work #####
# Demesa and PLII well, basic information in 1 sheet
MA_context <- read_xlsx("data_raw/MA_Data/MA_data_context.xlsx")
MA_selected_context <- MA_context %>%
  mutate(`F S Number` = as.character(`F S Number`)) %>%
  select(-c("Ceramics (n)", "Glass (n)", "MCD",))

# Combine all site contexts together #####
all_sites <- full_join(Rut_context_final, HWN_context_final) %>%
  full_join(., Puente_context_final) %>%
  full_join(., DLC_context_final) %>%
  full_join(., MA_selected_context)
# save combined context file  
write_xlsx(all_sites, "data_processed/spreadsheets/contexts/Phd_Combined_contexts.xlsx")

