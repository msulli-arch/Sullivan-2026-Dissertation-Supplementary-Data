# PhD CERAMIC DATA ANALYSIS ###########################
# Load Packages for analysis
library(rio)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(writexl)

`%ni%` <- Negate(`%in%`) # opposite of within (%in%) from stackoverflow solution, useful
Colors <- c("firebrick",
            "deepskyblue4",
            "goldenrod", 
            "darkgrey",
            "darkgreen",
            "chocolate4",
            "lightblue"
)

# Define the desired consistent column order for data tables
desired_order <- c("DLC", "DM", "PLII", "Puente",	"87 Church", "116 Broad")		

# LOAD DATA  #######
# load data and change project names for readibility on graphs and charts
PhD_final <- read_excel("data_processed/spreadsheets/PhD_ceramics.xlsx") %>%
  mutate(ProjectName = case_when(ProjectName == "Heyward-Washington House Necessary" ~ "87 Church",
                                 ProjectName == "John Rutledge House" ~ "116 Broad",
                                 ProjectName == "De la Cruz" ~ "DLC",
                                 ProjectName == "DeMesa" ~ "DM",
                                 TRUE ~ ProjectName),
         )
PhD_final$ProjectName <- factor(PhD_final$ProjectName, levels = desired_order)
print(unique(PhD_final$Context))

# BASIC ANALYSIS #####
# Ceramic Material Types at each site and corresponding graph ====
# Count data and graph ----
BasicSummary <- PhD_final %>%
  group_by(City, `ProjectName`, `Material` ) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
# Count data as relative frequency by site
BasicSummary_Prop <- BasicSummary %>%
  group_by(City, ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
# Count data as relative frequency by site transformed into a summary table
BasicSummary_Prop_wide <- BasicSummary_Prop %>%
  select(-c(City, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))
write_csv(BasicSummary_Prop_wide, "data_processed/tables/BasicSummary_Prop_wide.csv")
# Graph the Count data
BasicSummary_Prop_graph <- ggplot(BasicSummary_Prop, aes(x=ProjectName, y=percent, fill=Material)) +
  geom_bar(stat='identity', position='stack', width = 0.5) +
  theme(text=element_text(size=18/.pt)) +
  ylab("Percent (%)") +
  #facet_grid(City ~ .) +
  scale_fill_manual(values=Colors) +
  theme_gray(base_size = 12)

plot(BasicSummary_Prop_graph)
ggsave("data_processed/graphs/Graph_Basic_ct_Prop.png", BasicSummary_Prop_graph)

# Weight data and Graph ----
Basic_wt_summary <- PhD_final %>%
  filter(!is.na(SherdWeight)) %>%
  group_by(City, `ProjectName`, `Material` ) %>%
  summarise(`TotalCt` = sum(SherdWeight)) %>%
  ungroup()
# Weight data as relative frequency by site
Basic_wt_Prop <- Basic_wt_summary %>%
  group_by(City, ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
# Weight data as relative frequency by site transformed into a summary table
Basic_wt_Prop_wide <- Basic_wt_Prop %>%
  select(-c(City, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))
write_csv(Basic_wt_Prop_wide, "data_processed/tables/Basic_wt_Prop_wide.csv")
# Graph the Weight data
Basic_wt_Prop_graph <- ggplot(Basic_wt_Prop, aes(x=ProjectName, y=percent, fill=Material)) +
  geom_bar(stat='identity', position='stack', width = 0.5) +
  theme(text=element_text(size=18/.pt)) +
  ylab("Percent (%)") +
  #facet_grid(City ~ .) +
  scale_fill_manual(values=Colors) +
  theme_gray(base_size = 12)
plot(Basic_wt_Prop_graph)
ggsave("data_processed/graphs/Graph_Basic_wt_Prop.png", Basic_wt_Prop_graph)

# Ceramic Material Types by region at each site and corresponding graph ====
# Regional Count data and graph ----
GeneralSummary <- PhD_final %>%
  group_by(City, `ProjectName`, `General` ) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
GeneralSummary_Prop <- GeneralSummary %>%
  group_by(City, ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
GeneralSummary_Prop_wide <- GeneralSummary_Prop %>%
  select(-c(City, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))
write_csv(GeneralSummary_Prop_wide, "data_processed/tables/GeneralSummary_Prop_wide.csv")

# Graph the Regional Count data
GeneralSummary_Prop_graph <- ggplot(GeneralSummary_Prop, aes(x=ProjectName, y=percent, fill= General)) +
  geom_bar(stat='identity', position='stack', width = 0.5) +
  theme(text=element_text(size=18/.pt)) +
  ylab("Percent (%)") +
  #facet_grid(City ~ .) +
  scale_fill_manual(values=Colors) +
  theme_gray(base_size = 12)
plot(GeneralSummary_Prop_graph)
ggsave("data_processed/graphs/GeneralSummary_Prop_graph.png", GeneralSummary_Prop_graph)

# Regional weight data and graph ----
General_wt_summary <- PhD_final %>%
  filter(!is.na(SherdWeight)) %>%
  group_by(City, `ProjectName`, `General`) %>%
  summarise(`TotalCt` = sum(SherdWeight)) %>%
  ungroup()
# Weight data as relative frequency by site
General_wt_Prop <- General_wt_summary %>%
  group_by(City, ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
# Weight data as relative frequency by site transformed into a summary table
General_wt_Prop_wide <- General_wt_Prop %>%
  select(-c(City, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))
write_csv(General_wt_Prop_wide, "data_processed/tables/General_wt_Prop_wide.csv")
# Graph the Weight data
General_wt_Prop_graph <- ggplot(General_wt_Prop, aes(x=ProjectName, y=percent, fill=General)) +
  geom_bar(stat='identity', position='stack', width = 0.5) +
  theme(text=element_text(size=18/.pt)) +
  ylab("Percent (%)") +
  #facet_grid(City ~ .) +
  scale_fill_manual(values=Colors) +
  theme_gray(base_size = 12)
plot(General_wt_Prop_graph)
ggsave("data_processed/graphs/General_wt_Prop_graph.png", General_wt_Prop_graph)

# WARE/VESSEL ANALYSIS GENERAL SUMMARIES ####
# Vessel Analysis Data Constraints ====
# Graphs to show limits of vessel analysis
# Vessel Category Size ----
VesselCat_size <- PhD_final %>%
  filter(MaximumSherdMeasurement <= 150) %>% # filtering larger results for cleaner graph
  ggplot(aes(`MaximumSherdMeasurement`, y= Count, fill= VesselCategory)) +
  geom_bar(stat = "identity", width = 10) +
  scale_x_continuous(breaks=seq(0, 300, 20)) +
  theme(legend.position = "bottom", text=element_text(size=30/.pt)) +
  xlab("Maximum Sherd Size") +
  facet_grid(ProjectName ~ .)
plot(VesselCat_size)
ggsave("data_processed/graphs/VesselCat_size.png", VesselCat_size, dpi=300)
# Mended Sherds at each site ----
MendedCats <- PhD_final %>%
  select(c("ProjectName", "Mended", "Count")) %>%
  group_by(ProjectName, Mended) %>%
  summarise(`Count` = sum(Count)) %>%
  remove_missing() %>%
  ungroup()

MendedCats_percent <- MendedCats %>%
  group_by(ProjectName) %>%
  mutate(percent= round(prop.table(Count) * 100, digits = 2)) %>%
  ungroup()
MendedCats_percent1 <- MendedCats_percent %>%
  select(-c("Count")) %>%
  spread(., ProjectName, percent)
write_csv(MendedCats_percent1, "data_processed/tables/mended_percents.csv")

# Vessel Forms Represented At Each Site ####
FormSums <- PhD_final %>%
  group_by(`City`, `ProjectName`, VesselCat, `ObjVessels`, `VesselType`) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
FormSums_percent <- FormSums %>%
  group_by(`ProjectName`) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
FormSums_percent_wide <- FormSums_percent %>%
  select(-c(City, `VesselType`, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))
# Vessel Forms City to city comparison
CityFormSums <- PhD_final %>%
  group_by(`City`, VesselCat, `ObjVessels`) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
CityFormSums_percent <- CityFormSums %>%
  group_by(`City`) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
CityFormSums_percent_wide <- CityFormSums_percent %>%
  select(-c(TotalCt)) %>%
  spread(.,
         key = City, # Category to split into columns
         value = percent,
         fill= NA)

# summary table of vessel forms present at all sites and both cities
Combined_form_sums <- full_join(FormSums_percent_wide, CityFormSums_percent_wide)
write_csv(Combined_form_sums, "data_processed/tables/Combined_form_sums.csv")

# VesselType Comparisons removing unidentifiable vessels ####
FormPercentsbyVesseltype <- FormSums %>%
  filter(ObjVessels != "Unidentifiable") %>%
  group_by(`ProjectName`) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()

Graph_FormPercentsbyVesseltype <- FormPercentsbyVesseltype %>%
  ggplot(aes(x= ProjectName, y= percent, fill=VesselType)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  #theme(aspect.ratio = 1) +
  theme(legend.position = "bottom", axis.text.y = element_text(size=30/.pt)) +
  scale_fill_manual(values=Colors) +
  xlab(label = NULL) +
  ylab(label = NULL)
Graph_FormPercentsbyVesseltype
ggsave("data_processed/graphs/Graph_FormPercentsbyVesseltype_unid_filter.png", Graph_FormPercentsbyVesseltype, dpi=300)

Graph_FormPercentsbyCat <- FormPercentsbyVesseltype %>%
  ggplot(aes(x= ProjectName, y= percent, fill=VesselCat)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  #theme(aspect.ratio = 1) +
  theme(legend.position = "bottom", axis.text.y = element_text(size=30/.pt)) +
  xlab(label = NULL) +
  ylab(label = NULL)
Graph_FormPercentsbyCat
# graph of vessel category (percent) at each site
ggsave("data_processed/graphs/Graph_FormPercentsbyVesselType__unid_filter.png", Graph_FormPercentsbyCat, dpi=300)

# WARE TYPE ANALYSIS ####
# Ware Count Summaries by site ======
Ware_ct <- PhD_final %>%
  group_by(City, ProjectName, Material, General, Ware) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
# Ware Count data as relative frequency by site
Ware_ct_prop <- Ware_ct %>%
  group_by(City, ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
Ware_ct_prop_wide <- Ware_ct_prop %>%
select(-c(City, TotalCt)) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character))

CityWareCt <- Ware_ct %>%
  group_by(`City`, Ware) %>%
  summarise(`TotalCt` = sum(TotalCt)) %>%
  ungroup()
CityWare_prop <- CityWareCt %>%
  group_by(`City`) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
CityWare_prop_wide <- CityWare_prop %>%
  select(-c(TotalCt)) %>%
  spread(.,
         key = City, # Category to split into columns
         value = percent,
         fill= NA)

# summary table of ceramic ware types represented at each site and by city
Combined_ware_prop <- full_join(Ware_ct_prop_wide, CityWare_prop_wide, by = "Ware")
write_csv(Combined_ware_prop, "data_processed/tables/Combined_ware_prop.csv")

# General Summary by Material Type
Material_General_sum <- Ware_ct_prop %>%
  group_by(City, ProjectName, General, Material) %>%
  select(-c(Ware, TotalCt)) %>%
  mutate(TotalPct = sum(percent)) %>%
  ungroup()
Material_General_prop_wide <- Material_General_sum %>%
  select(-c(City, percent)) %>%
  distinct(ProjectName, General, Material, TotalPct) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = TotalPct,
         fill= NA) %>%
  relocate(desired_order, .after = where(is.character)) %>%
  arrange(General) %>%
  ungroup()
City_material_sum <- PhD_final %>%
  select(-c(Ware)) %>%
  group_by(`City`, `General`, `Material`) %>%
  summarise(`TotalCt` = sum(`Count`)) %>%
  ungroup()
City_material_prop <- City_material_sum %>%
  group_by(City) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2))
City_material_prop_wide <- City_material_prop %>%
  select(-c("TotalCt")) %>%
  spread(.,
         key = City, # Category to split into columns
         value = percent,
         fill= NA)
# summary table of Regional ware type and ceramic material categories at all sites and both cities
Combined_material_prop <- full_join(Material_General_prop_wide, City_material_prop_wide, by = c("General", "Material"))
write_csv(Combined_material_prop, "data_processed/tables/Combined_material_prop.csv")

# Summary of Vessel Forms by Ware Type/General/Material
FormSums <- PhD_final %>%
  group_by(`City`, `ProjectName`, General, `Material`, VesselCat, `ObjVessels`, `VesselType`, Ware) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
FormSums_prop <- FormSums %>%
  group_by(ProjectName) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
FormSums_prop_wide <- FormSums_prop %>%
  select(-c("City", "TotalCt")) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA)
CityFormSums <- PhD_final %>%
  group_by(`City`, General, `Material`, VesselCat, `ObjVessels`, `VesselType`, Ware) %>%
  summarise(`TotalCt` = sum(Count)) %>%
  ungroup()
CityFormSums_prop <- CityFormSums %>%
  group_by(City) %>%
  mutate(percent= round(prop.table(TotalCt) * 100, digits = 2)) %>%
  ungroup()
CityFormSums_prop_wide <- CityFormSums_prop %>%
  select(-c("TotalCt")) %>%
  spread(.,
         key = City, # Category to split into columns
         value = percent,
         fill= NA)
# KEY Summary table: all vessel forms by ceramic ware types at each site and city
# Master table used to delineate various vessel form tables in Chapter 6
Combined_formsums_prop <- full_join(FormSums_prop_wide, CityFormSums_prop_wide)
write_csv(Combined_formsums_prop, "data_processed/tables/Combined_formsums_prop.csv")

# Majolica Decorative Types Summary ####
MajolicaDec <- PhD_final %>%
  filter(Ware == "Majolica") %>%
  # combine plate Unid:table ware for better comprehension #
  mutate(ObjVessels= if_else(ObjVessels == "Flat, Unid: Tableware", "Plate", ObjVessels)) %>%
  group_by(ProjectName, Ware, VesselCat, ObjVessels, DecorationYN, TinEnamelType) %>%
  summarise(`TotalCt` = sum(Count))
MajDec_sum <- MajolicaDec %>%
  group_by(ProjectName, DecorationYN, TinEnamelType) %>%
  summarise(`TotalCt` = sum(TotalCt)) %>%
  ungroup() %>%
  group_by(ProjectName) %>%
  mutate(percent = round(prop.table(TotalCt) * 100, digits = 2))
MajDec_prop_wide <- MajDec_sum %>%
  select(-c("TotalCt")) %>%
  spread(.,
         key = ProjectName, # Category to split into columns
         value = percent,
         fill= NA)
# summary of majolica types at all sites, not used directly in dissertation
write_csv(MajDec_prop_wide, "data_processed/spreadsheets/MajDec_prop_wide.csv")

# Parsing Hollowares by rim data ####
rim_data <- read_csv("data_processed/spreadsheets/Rim_data.csv")

# comparison of creamware in both cities
Charleston_flat_compare <- rim_data %>%
  filter(
    !is.na(`Rim Diameter`) &
    City == "Charleston" &
    (Ware == "Creamware" | Ware == "Porcelain, Chinese") &
    VesselCat == "Flat") %>%
  ggplot(., aes(x=`Rim Diameter`, fill=ObjVessels)) +
  geom_histogram() +
  theme(legend.title = element_text(size=20/.pt),  text = element_text(size = 20/.pt), axis.text.x = element_text(size = 20/.pt),
        axis.text.y = element_text(size =20/.pt),
        legend.position = "bottom",
        axis.title = element_text(size=20/.pt)) +
  labs(
    title = "Charleston Rim Diameter Comparison") +
  scale_x_continuous(breaks=seq(0,350, 20)) +
  facet_grid(Ware ~ .) +
  scale_fill_brewer(palette = "Dark2")
plot(Charleston_flat_compare)
ggsave("data_processed/graphs/Charleston_flat_compare.png", Charleston_flat_compare, dpi=300)

Charleston_hollow_compare <- rim_data %>%
  filter(
    !is.na(`Rim Diameter`) &
      City == "Charleston" &
      (Ware == "Creamware" | Ware == "Porcelain, Chinese") &
      VesselCat == "Hollow") %>%
  ggplot(., aes(x=`Rim Diameter`, fill=ObjVessels)) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,300, 20)) +
  scale_y_continuous(breaks=seq(0,10, 2)) +
  theme(legend.title = element_text(size=20/.pt),  text = element_text(size = 20/.pt), axis.text.x = element_text(size = 20/.pt),
        axis.text.y = element_text(size =24/.pt),
        legend.position = "bottom",
        axis.title = element_text(size=20/.pt)) +
  labs(
    title = "Charleston Rim Diameter Comparison") +
  scale_fill_brewer(palette = "Paired") +
  facet_grid(Ware ~ .)
plot(Charleston_hollow_compare)
ggsave("data_processed/graphs/Charleston_hollow_compare.png", Charleston_hollow_compare, dpi=300)

StAug_flat_compare <- rim_data %>%
  filter(
    !is.na(`Rim Diameter`) &
      City == "St. Augustine" &
      (Ware == "Creamware" | Ware == "Porcelain, Chinese") &
      VesselCat == "Flat") %>%
  ggplot(., aes(x=`Rim Diameter`, fill=ObjVessels)) +
  geom_histogram() +
  theme(legend.title = element_text(size=20/.pt),  text = element_text(size = 20/.pt), axis.text.x = element_text(size = 20/.pt),
        axis.text.y = element_text(size =24/.pt),
        legend.position = "bottom",
        axis.title = element_text(size=20/.pt)) +
  scale_x_continuous(breaks=seq(0,350, 20)) +
  labs(
    title = "St. Augustine Rim Diameter Comparison") +
  facet_grid(Ware ~ .) +
  scale_fill_brewer(palette = "Dark2")
plot(StAug_flat_compare)
ggsave("data_processed/graphs/StAug_flat_compare.png", StAug_flat_compare, dpi=300)

StAug_hollow_compare <- rim_data %>%
  filter(
    !is.na(`Rim Diameter`) &
      City == "St. Augustine" &
      (Ware == "Creamware" | Ware == "Porcelain, Chinese") &
      VesselCat == "Hollow") %>%
  ggplot(., aes(x=`Rim Diameter`, fill=ObjVessels)) +
  geom_histogram() +
  scale_x_continuous(breaks=seq(0,300, 20)) +
  scale_y_continuous(breaks=seq(0,10, 2)) +
  theme(legend.title = element_text(size=20/.pt),  text = element_text(size = 20/.pt), axis.text.x = element_text(size = 20/.pt),
        axis.text.y = element_text(size =28/.pt),
        legend.position = "bottom",
        axis.title = element_text(size=20/.pt)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "St. Augustine Rim Diameter Comparison") +
  facet_grid(Ware ~ .)
plot(StAug_hollow_compare)
ggsave("data_processed/graphs/StAug_hollow_compare.png", StAug_hollow_compare, dpi=300)
