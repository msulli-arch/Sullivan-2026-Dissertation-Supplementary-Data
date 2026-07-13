# ORGANIZATION OF CERAMIC DATA FOR ANALYSIS ###########################
# Load Packages for analysis
library(tidyverse)
library(readxl)
library(writexl)
library(RColorBrewer)

# Data management tool: opposite of within (%in%) from stackoverflow solution
`%ni%` <- Negate(`%in%`)

# Colors for graphs
Colors <- c("deepskyblue4", 
            "firebrick",
            "goldenrod", 
            "darkgrey",
            "darkgreen",
            "chocolate4",
            "lightblue"
            )

# LOAD DATA FOR ORGANIZATION  #######
# Load combined site context information from "Context_Organization.r" script
Site_contexts <- read_xlsx("data_processed/spreadsheets/contexts/Phd_Combined_contexts.xlsx") %>%
  rename(ARL = ObjectID) %>%
  rename(ProjectName = "Project Name")
# simplify context variables
Contexts_basic <- Site_contexts %>%
  distinct(ProjectName, Context, `Context ID`, FSGroup, `Level Designation`, CATALOGED)

## Load Ceramic data ####

# this data includes all the DAACS data: including Puente, DLC, de leon (not used in Diss), Fatio (not used in Diss) from St. Augustine, 
# in addition to 87 Church Street Privy, and 116 Broad St. in Charleston
DAACS_raw <- read_xlsx("data_raw/Ceramic_data_pull_arrange_20260710.xlsx")

# Make a new column to identify City of each site based on ProjectID
unique(DAACS_raw$ProjectID)
SitesCharleston <- c("1310", "1311")
SitesStAug <- c("5004", "5006", "5007", "5011")

# Apply City designation to Ceramic Data
DAACS_raw1 <- DAACS_raw %>% 
  mutate(City = case_when(ProjectID %in% SitesCharleston ~ "Charleston",
                          ProjectID %in% SitesStAug ~ "St. Augustine"))

# Add 87 Church Street ceramics not yet entered into DAACS as 5/21/2026
HWN_leftovers <- read_xlsx("data_raw/87_Church/HWN_not_Cat_in_DAACS.xlsx") %>%
  mutate(City = "Charleston") %>%
  select(1:12, "City")

# MA Data ceramic data translated into DAACS format for key variables (Ware, Vessel Cat, Form, Completeness)
MA_data <- read_xlsx("data_raw/MA_Data/MAdata_linked.xlsx") %>%
  mutate(City = "St. Augustine") %>%
  select(., -c("Item", "CatID", "Type", "Decoration", "Color", "Comments", "Vessel Function", "Vessel Code"))

# Merge all ceramic into one table for further transformations
all_sites <- list(DAACS_raw1, HWN_leftovers, MA_data) %>%
  reduce(full_join)
print(unique(all_sites$ProjectName))

# Function to remove columns where all rows are NA, from stackoverflow 
not_all_na <- function(x) any(!is.na(x))
# apply to ceramic data at all sites
all_sites <- all_sites %>%
  select(where(not_all_na))

## Load Object data from DAACS ==============================
# Object Data for mended vessels at Puente, de la Cruz, and Heyward Washington Necessary and John Rutledge House
DAACS_object <- read_xlsx("data_raw/Object_Query_functioning_20260710.xlsx") %>%
  mutate(City = case_when(ProjectID %in% SitesCharleston ~ "Charleston",
                          ProjectID %in% SitesStAug ~ "St. Augustine"))

# BASIC CERAMIC ORGANIZATION ####
# Vessel Form concatenation #######
# Concatenate hollow/flat to form table for unids for less general data
unids <- c("Unidentifiable", "Unid: Tableware", "Unid: Teaware", "Unid: Utilitarian")

# Objects
DAACS_object.reclass <- DAACS_object %>%
  mutate(ConcatObjectVessel = (
    case_when(
      ((`Vessel Category` == "Flat") & (Form %in% unids)) ~ 
        paste(DAACS_object$`Vessel Category`, DAACS_object$Form, sep = ", "),
      ((`Vessel Category` == "Hollow") & (Form %in% unids)) ~ 
        paste(DAACS_object$`Vessel Category`, DAACS_object$Form, sep = ", "),
      ((`Vessel Category` == "Unidentifiable") & (Form %in% unids)) ~ 
        paste(DAACS_object$Form),
      (Form %ni% unids) ~ 
        paste(DAACS_object$Form)
    )))
# select variables used below
ObjectVessel <- select(DAACS_object.reclass, c("ObjectID", "ConcatObjectVessel", "Vessel Category"))


# Sherd Level Data
all_sites.reclass <- all_sites %>%
  mutate(MergedVessel = (
    case_when(
      ((VesselCategory == "Flat") & (Form %in% unids)) ~ 
        paste(all_sites$VesselCategory, all_sites$Form, sep = ", "),
      ((VesselCategory == "Hollow") & (Form %in% unids)) ~ 
        paste(all_sites$VesselCategory, all_sites$Form, sep = ", "),
      ((VesselCategory == "Unidentifiable") & (Form %in% unids)) ~ 
        paste(all_sites$Form),
      (Form %ni% unids) ~ 
        paste(all_sites$Form)
    )))

# Add Mended Forms to new column
all_sites1 <- all_sites.reclass %>%
  mutate(Vessels = (
    case_when((MendedForm != "Not Mended") ~ paste(all_sites.reclass$MendedForm),
          (MendedForm == "Not Mended") ~ paste(all_sites.reclass$MergedVessel)
    )))

# merge Object Vessels to new column
all_sites2 <- left_join(all_sites1, ObjectVessel, by = c("ObjectID")) %>%
  mutate(ObjVessels = case_when(
    (ObjectID > 0) ~ paste(ConcatObjectVessel),
    (is.na(ObjectID)) ~ paste(Vessels)
  ))

# rewrite to reapply, concatenated vessel terms to this column
all_sites3 <- all_sites2 %>% 
  mutate(ObjVessels = case_when(
    (ObjVessels %in% unids) ~ paste(all_sites2$MergedVessel),
    (ObjVessels %ni% unids) ~ paste(all_sites2$ObjVessels)
  ))
all_sites4 <- all_sites3 %>%
  mutate(VesselCat = case_when(
    (is.na(`Vessel Category`)) ~ paste(all_sites3$VesselCategory),
    (!is.na(`Vessel Category`)) ~ paste(all_sites3$`Vessel Category`)
  ))

#further cleaning to make data more uniform
# add Gravy boat vessel forms not in DAACS
all_sites5 <- all_sites4 %>%
  mutate(ObjVessels = case_when( ObjVessels == "Serving Dish, unidentified" ~ "Serving Dish, unid.",
                                 ObjVessels == "Tea Bowl" ~ "Teabowl",
                                 ObjVessels == "Tea Cup" ~ "Teacup",
                                 ObjectID == 3283254 ~ "Gravy/Sauce Boat",
                                 ObjectID == 3296106 ~ "Gravy/Sauce Boat",
                                 ArtifactID == "5011-2000-6-391-NRD--00139" ~ "Gravy/Sauce Boat",
                                 TRUE ~ ObjVessels),
         StylisticGenre = case_when(Ware == "Cauliflower ware" & is.na(StylisticGenre) ~ "Not Applicable",
           TRUE ~ StylisticGenre),
         Ware = case_when(Ware == "Staffordshire Brown Stoneware" ~ "British Stoneware",
                          Ware == "Redware" ~ "Coarse Earthenware, unidentified",
                          Ware == "Spanish Coarse Earthenware" ~ "Iberian Coarse Earthenware",
                          TRUE ~ Ware),
         CoarseEarthenwareType = if_else(CoarseEarthenwareType == "American Redware, unid.", "American CEW, unid.", CoarseEarthenwareType)
)
VesselCat_summaries <- all_sites5 %>%
  distinct(VesselCat, ObjVessels)

Hollow <- c("Chamberpot", "Escudilla", "Drinking Pot", "Bowl", "Bowl, punch", "Teabowl", "Teacup", 
            "Hollow, Unid: Teaware", "Storage Jar", "Hollow, Unidentifiable", "Hollow, Unid: Utilitarian", "Mug/Can", "Tureen", "Drug Jar/Salve Pot")
Flat <- c("Milk Pan", "Plate", "Saucer", "Flat, Unid: Tableware", "Platter", "Brimmed Plato")
Unidentifiable <- c("Gaming Piece", "Gaming Piece, preform", "Tile, fireplace")

all_sites6 <- all_sites5 %>%
  mutate(VesselCat = case_when(ObjVessels %in% Hollow ~ "Hollow",
                               ObjVessels %in% Flat ~ "Flat",
                               ObjVessels %in% Unidentifiable ~ "Unidentifiable",
                              TRUE ~ VesselCat))

VesselCat_summaries <- all_sites6 %>%
  distinct(VesselCat, ObjVessels)

# Condense categories with few counts or Spanish names that = English names
all_sites7 <- all_sites6 %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Brimmed Plato", "Plate", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Flat, Unid: Teaware", "Saucer", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Plato/Plate (FLMNH)", "Plate", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Cup", "Drinking Vessel", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Drinking Pot", "Drinking Vessel", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Jar", "Storage Jar", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Mug/Can", "Drinking Vessel", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Pitcher/Jug", "Pitcher/Ewer", ObjVessels)) %>%
  mutate(ObjVessels = ifelse(ObjVessels == "Gaming Piece, preform", "Gaming Piece", ObjVessels))

# As accurate as possible vessel count by sherd data by vessel type based off mends!!
# Our more accurate Vessel category columns is VesselCat for Hollow, Flat, Unid
# Our more accurate vessel form is ObjVessels which accounts for object identifications of sherds 

# Further Data-Clean Up #####

# Apply Regions of Ware Types to Data
Ware_regions <- read_xlsx("data_raw/Wares_region.xlsx")
AmericanCEWs <- c("PHAB (PHL, ALX, BLT)", "American CEW, unid.", "American Redware, unid.")
BritishCEWS <- c("Coal Measures", "Slip-coated Ware (SC)", "Post-Medieval London-Area Redware")

Ware_regions1 <- left_join(all_sites7, Ware_regions) %>%
  mutate(General =case_when(CoarseEarthenwareType %in% AmericanCEWs ~ "American",
                            CoarseEarthenwareType %in% BritishCEWS ~ "British",
                            TRUE ~ General)) %>%
  mutate(Region =case_when(CoarseEarthenwareType %in% AmericanCEWs ~ "American",
                           CoarseEarthenwareType %in% BritishCEWS ~ "British",
                           TRUE ~ General))

# Remove sites in question for Dissertation and duplicate DLC entries
remove_sites <- c("Fatio", "de Leon")
PhD_ceramics <- Ware_regions1 %>%
  filter(ProjectName %ni% remove_sites) %>%
  filter(!(Context == "HWN02-L08" & Mended == "No"))
PhD_ceramics1 <- left_join(PhD_ceramics, Contexts_basic, by = c("ProjectName", "Context"))
# remove ceramic types not analyzed
PhD_ceramics2 <- PhD_ceramics1 %>%
  filter(Ware != "Indigenous") %>%
  filter(Ware != "Colonoware") %>%
  filter(Ware !="Caribbean Coarse Earthenware, unid.") %>%
  filter(Ware !="Architectural")

# Select only MA contexts at DLC site
PhD_ceramics3 <- PhD_ceramics2 %>%
  filter((ProjectName != "De la Cruz") | 
           (ProjectName == "De la Cruz" & CATALOGED == "MA") |
           (ProjectName == "De la Cruz" & `In DAACS` == "No" & `MA Data` == "Yes") )
print(unique(PhD_ceramics3$Context))

# join defined Vessel Types (Utilitarian, Tableware, etc to sherd data based on concatenated object data)
VesselType <- read_xlsx("data_raw/VesselTypes.xlsx")
PhD_ceramics4 <- left_join(PhD_ceramics3, VesselType, by = c("VesselCat", "ObjVessels")) %>%
  select(-`TotalCt`)
final_PhD <- PhD_ceramics4

print(unique(final_PhD$Context))

# Data fix that appeared during analysis
final_PhD1 <- final_PhD %>%
  mutate(Material = case_when(
    ArtifactID == "5006-92-84-72-11-DRS--00073" ~ "Coarse EW",
    TRUE ~ Material   
  ))
write_xlsx(final_PhD1, "data_processed/spreadsheets/PhD_ceramics.xlsx")

# Clean Data Set for MCD/CA and further Vessel Form Analysis!

#### Rim and Base Organization ####
## Load Rim/Base Data from sites =====
# 116 Broad Street site
Rutledge_sherd_rims <- read_excel("data_raw/116_Broad/Rut_rims_bases.xlsx", sheet = "Sherds")
Rutledge_obj_rims <- read_excel("data_raw/116_Broad/Rut_rims_bases.xlsx", sheet = "Objects")
# 87 Church Street site
Heyward_sherd_rims <- read_excel("data_raw/87_Church/HWN_Rims_Bases.xlsx", sheet = "Sherds") %>%
  mutate(`Rim Diameter` = as.character(`Rim Diameter`))
Heyward_obj_rims <- read_excel("data_raw/87_Church/HWN_Rims_Bases.xlsx", sheet = "Objects") %>%
  mutate(`Rim Diameter` = as.character(`Rim Diameter`))
# Puente
Puente_sherd_rims <- read_excel("data_raw/Puente/Puente_rims_bases.xlsx", sheet = "Sherds")
Puente_obj_rims <- read_excel("data_raw/Puente/Puente_rims_bases.xlsx", sheet = "Objects")
# MA sites (DLC, Demesa, PLII)
MA_rim <- read_excel("data_raw/MA_Data/MA_rim_data.xlsx")

# combine rim data
All_sherd_rims <- bind_rows(Rutledge_sherd_rims, Heyward_sherd_rims, Puente_sherd_rims, MA_rim)
All_obj_rims <- bind_rows(Rutledge_obj_rims, Heyward_obj_rims, Puente_obj_rims)

sherds <-All_sherd_rims %>%
  select(c("ArtifactID", "Rim Length", "Rim Diameter", "Base Length", "Base Diameter"))

sherd_type_info <- all_sites7 %>%
  select(c("ArtifactID", "Ware", "StylisticGenre", "VesselCat", "ObjVessels", "Completeness", "MaximumSherdMeasurement", "ObjectID", "ProjectName", "City", "Notes")) %>%
  right_join(sherds, sherd_type_info, by = "ArtifactID")
unique(sherd_type_info$StylisticGenre)
unique(sherd_type_info$Completeness)

Rims <- c("Body, Handle Terminal, Rim", "Body, Rim", "Neck, Rim", "Body, Handle, Rim", "Rim", "Lid")
Bases <- c("Base, Body", "Base, Body, Handle",  "Base")
Both <- c("Base, Body, Rim", "Base, Body, Handle, Rim")

sherd_type_info1 <- sherd_type_info %>%
  mutate(MeasurementType = case_when(Completeness %in% Rims ~ "Rim",
                                     Completeness %in% Bases ~ "Base",
                                     Completeness %in% Both ~ "Both",
                                     TRUE ~ "Error"))

#filter extraneous rim data
sherds_rims <- sherd_type_info1 %>%
  filter(`Rim Diameter` != "Not Applicable") %>%
  filter(`Rim Diameter` != "Did not record") %>% 
  filter(`Rim Diameter` != "Too Small") %>%
  filter(`Rim Diameter` != "Mended") %>%
  filter(`Rim Length` != "Object") %>%
  filter(is.na(ObjectID)) %>%
  filter(!is.na(Ware)) %>%
  mutate(`Rim Diameter` = as.numeric(`Rim Diameter`))

objs <- All_obj_rims %>%
  select(c("ObjectID", "Rim Length", "Rim Diameter", "Base Length", "Base Diameter"))

Object_merge <- full_join(objs, DAACS_object.reclass, by = "ObjectID") %>%
  mutate(MeasurementType = case_when(Completeness %in% Rims ~ "Rim",
                                   Completeness %in% Bases ~ "Base",
                                   Completeness %in% Both ~ "Both",
                                   TRUE ~ "Error"),
         VesselCat = `Vessel Category`,
         ObjVessels = ConcatObjectVessel)
Object_merge2 <- Object_merge %>%
  mutate(`Rim Diameter` = ifelse(is.na(`Rim Diameter`), Object_merge$ObjectRimDiameter, `Rim Diameter`)) %>%
  mutate(`Base Diameter` = ifelse(is.na(`Base Diameter`), Object_merge$ObjectBaseDiameter, `Base Diameter`))

Object_merge3 <- Object_merge2 %>%
  select(c("ObjectID", "ProjectName", "MeasurementType", "Ware", "VesselCat", "ObjVessels", "Completeness", "Notes", "City", "Rim Length", "Rim Diameter", "Base Length", "Base Diameter")) %>%
  mutate(`Rim Diameter` = as.numeric(`Rim Diameter`))

rim_data <- full_join(Object_merge3, sherds_rims)
write_csv(rim_data, "data_processed/spreadsheets/Rim_data.csv")
