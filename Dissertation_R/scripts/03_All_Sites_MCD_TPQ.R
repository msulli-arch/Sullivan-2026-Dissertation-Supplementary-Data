# MCD/TPQ AND CA ANALYSIS OF SELECTED CONTEXTS IN SITE ASSEMBLAGE FOR DISSERTATION ####

# Created by:  FDN  8.5.2014
# Last update: FDN 8.5.2014  
# Edited by:   LAB 1.17.2017 for Morne Patate
# Edited by:   LC 12.4.2017 Hermitage phases
# Edited by:   FDN 12.21.2018 more tidy; fixed MCD function to handle phases.
# Edited by:   CP and EB 5.28.2019 Comments and Section editing for UR code
# Edited by:   Myles Sullivan 7.10.2026 adapted for dissertation research

# load the libraries
library(tidyverse)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)
library(readxl)
library(writexl)
library(ggrepel)

#### 1. get the table with the ware type date ranges ####

# this MCD type table has been modified for this project with date ranges
MCDTypeTable<- read.csv(file = "data_raw/DAACS_MCDTypeTable_mod.csv", 
                        fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

Regional_data <- read_excel("data_raw/Wares_region.xlsx") %>%
  select(-`Material`)

#### 2. load dissertation ceramics ####
PhD_ceramics <- read_excel("data_processed/spreadsheets/PhD_ceramics.xlsx")
Context_basic <- PhD_ceramics %>%
  mutate(FSGroup = if_else(ProjectName == "De la Cruz", Context, FSGroup)) %>%
  distinct(ProjectName, Context, `Context ID`, FSGroup, QuadratID, `Level Designation`)

# Make Majolica Types 
wareTypeData <- PhD_ceramics

Genre_Distinct <- wareTypeData %>%
  distinct(Ware, StylisticGenre)
write.csv(Genre_Distinct, "data_processed/spreadsheets/Genre_Distinct_PhD_ceramics.csv")

Majolicas <- wareTypeData %>%
  filter(Ware == "Majolica") %>%
  select(c("Ware", "TinEnamelType", "Count")) %>%
  group_by(TinEnamelType) %>% 
  summarise(count = sum(Count))

# do a summary
summary1 <- wareTypeData %>%
  group_by(ProjectName, Ware) %>% 
  summarise(count = sum(Count))
options(tibble.print_min=100)
summary1

#compute the total count of ceramics
AllCeramicCount <- summary1 %>% summarise(Count=sum(count))

#### 3. Customizations to the Ware Type dates or names####
# Replace Majolica with more specific Tin Enamel Types 
wareTypeData <-mutate(wareTypeData, Ware= ifelse((Ware == "Majolica"), TinEnamelType, Ware))
# Replace Majolica with more specific Tin Enamel Types 
wareTypeData <-mutate(wareTypeData, Ware= ifelse((Ware == "Mexican Coarse Earthenware"),`CoarseEarthenwareType`, Ware))
wareTypeData <-mutate(wareTypeData, Ware= ifelse((`CoarseEarthenwareType` == "Orange Micaeous"),`CoarseEarthenwareType`, Ware))

# Do a quick summary of the new ware type totals
summary2 <- wareTypeData %>% 
  group_by(ProjectName, Ware) %>% 
  summarise(count = sum(Count))
summary2

#### 4. Compute new numeric date variables from original ones #### 
# Compute midpoint, manufacturing span, and inverse variance for each ware type
# and add new columns to house variables.  These are needed to calculate the MCD.
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
  )

#### 5. Here you have the option to remove contexts with deposit type Cleanup and Surface Collection ####
#wareTypeData <- subset(wareTypeData, ! wareTypeData$DepositType  %in%  c('Clean-Up/Out-of-Stratigraphic Context',
#                                                                         'Surface Collection'))


#### 6. Create the UNIT Variable ####
# The UNIT variable contains the level at which assemblages are aggregated in 
# the analysis. You will need to cutomize this logic for YOUR site. There
# are circumstances where it will make the most sense to group by SGs or Features, but in others
# you may need to use contexts. Note, you should only use ONE option, either 6.1, 6.2, 6.3, or 6.4.  The rest
# should be commented out.
# Note that we create a new dataframe: wareTypeData_Unit.

# First some housekeeping so we do not stumble on the confusion between R's NA 
# (SQL NULLs) and blanks. 
wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- '' 
wareTypeData$FSGroup[is.na(wareTypeData$QuadratID)] <- ''
# 6.1 
# Use case_when to cycle through Feature, SG, and Context to assign 
# aggregration unit. 
# wareTypeData_Unit <- wareTypeData %>%  
#  mutate( unit = case_when(
#    FeatureNumber == "" & StratigraphicGroup == "" 
#    ~ paste(Context),
#    FeatureNumber == "" & StratigraphicGroup != "" 
#    ~ paste(StratigraphicGroup),
#    FeatureNumber != "" & StratigraphicGroup == ""
#    ~ paste(FeatureNumber, Context, sep= '.'),
#    FeatureNumber != "" & StratigraphicGroup != "" 
#    ~ paste(FeatureNumber,StratigraphicGroup, sep='.')
#  )) 


# BASIC MCD AT SITE LEVEL ####
## 6.2 Use this to assign ContextID to the unit. This is the Site level 
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$ProjectName)

table(wareTypeData_Unit$unit)

#### 7. Transpose the data for the MCD and CA ####
wareByUnitT <- wareTypeData_Unit %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Count)) %>%
  spread(Ware, value=count , fill=0 )


#### 8. Remove specific ware types (if you must) and set sample size cut off  ####
# 8.1 It is possible at this point to drop types you do not want in the MCD computations
# We do this because some types are historical types and some aren’t
#if a type isn’t helpful in providing a chronological signal then you should take it out
#Ex: American SW -- it has a very different function than a pearlware plate
# and therefore its pattern of occurrence is likely to be affected, can get temporal gradiant for Dim1 (REW) vs Dim 2 that capturing is more utilitarian  
# One thing -- we are doing this before we calculate MCDs NOT the seriation
# Two approaches: 1) leave them in the MCD dataframe and only take them out of the CA and then when you compare CA to the MCDs (i.e. don't
# remove them here,
#2) If you take them out here you will be doing the MCD CA comparison on the same dataset without the ware types

# Here we name the types we do NOT
# want included (NOTE: You will need to add the types that are particular to your site to the select function):
wareByUnitT1 <- wareByUnitT 
# not going to take these out for now
#%>% select( 
#  - 'American Stoneware',
#  - 'Refined Earthenware, unidentifiable',
#  - 'Iberian Coarse Earthenware')


# 8.2  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT1[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT1[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]



#### 9. Define functions to Remove Types w/o Dates and then compute MCDs ####
# 9.1 We build a function that removes types with no dates, either because they 
# have no dates in the MCDTypeTable or because they are not MCD Ware 
# Types (e.g. they are CEW Types). This approach is useful because it returns a 
# dataframe that contains ONLY types that went into the MCDs, which you may 
# want to analyze using using other methods (e.g. CA).
# Two arguments: 
#   unitData: dataframe with counts of ware types in units
#   the left variable IDs the units, while the rest of the varaibles are types
#   typeData: a dataframe with at least three variables named 'Ware', 'midPoint'
#   and 'inversevar' containing the manufacturing midpoints and inverse 
#   variances for the types.
# Returns a list comprised of two dataframes:
#   unitDataWithDates has units with types with dates
#   typeDataWithDates has the types with dates
RemoveTypesNoDates <- function(unitData,typeData){
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTable
  typesWithNoDates <- typeData$Ware[(is.na(typeData$midPoint))] 
  # types in the MCD table with no dates.
  moreTypesWithNoDates <- colnames(unitData)[-1][! colnames(unitData)[-1] %in% 
                                                   typeData$Ware] 
  # types in the data that are NOT in the MCD Type table.
  typesWithNoDates <- c(typesWithNoDates, moreTypesWithNoDates)
  unitDataWithDates <- unitData[, ! colnames(unitData) %in%  typesWithNoDates]
  typeDataWithDates <- typeData[! typeData$Ware %in%  typesWithNoDates, ]
  unitDataWithDates <- filter(unitDataWithDates, 
                              rowSums(unitDataWithDates[,2:ncol(unitDataWithDates)])>0)
  return(list(unitData = unitDataWithDates, 
              typeData = typeDataWithDates))
}

# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)


# Define a function that computes MCDs
# Two arguments: 
#   unitData: a dataframe with the counts of ware types in units. 
#   We assume the first column IDs the units, while the rest of the columns 
#   are counts of types.
#   typeData: a dataframe with at least two variables named 'midPoint' and 
#   'inversevar' containing the manufacturing midpoints and inverse variances 
#   for the types.
# Returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they 
#     appeared in the input unitData dataframe.  
EstimateMCD<- function(unitData,typeData){
  countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
  originalUnitName <-  colnames(unitData)[1]
  colnames(unitData)[1] <- 'unitID'
  unitID <- (unitData[,1])
  unitID[is.na(unitID)] <-'Unassigned'
  unitData[,1] <- unitID
  nUnits <- nrow(unitData)   
  nTypes<- nrow(typeData)
  nTypesFnd <-ncol(countMatrix)
  typeNames<- colnames(countMatrix)
  # create two col vectors to hold inverse variances and midpoints
  # _in the order in which the type variables occur in the data_.
  invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
  mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
    for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
    }
  }
  # compute the blue MCDs
  # get a unit by type matrix of inverse variances
  invVarMat<-matrix(t(invVar),nUnits,nTypesFnd, byrow=T)
  # a matrix of weights
  blueWtMat<- countMatrix * invVarMat
  # sums of the weight
  sumBlueWts <- rowSums(blueWtMat)
  # the BLUE MCDs
  blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
  # compute the vanilla MCDs
  sumWts <- rowSums(countMatrix)
  # the vanilla MCDs
  MCD<-(countMatrix %*% mPoint) / sumWts
  # now for the TPQs
  meltedUnitData <- gather(unitData, key = Ware, value=count,- unitID)
  meltedUnitData <- filter(meltedUnitData, count > 0) 
  mergedUnitData <- inner_join(meltedUnitData, typeData, by='Ware')
  # the trick is that to figure out the tpq. it's best to have each record (row) 
  # represent an individual sherd  but in its current state, each record has 
  # a count c:(c > 1). We must generate c records for each original record.
  # Use rep and rownames - rowname is a unique number for each row, kind of 
  # like an index. rep() goes through dataframe mergedUnitData and replicates 
  # based on the count column, i.e. if count is 5 it will create 5 records or 
  # rows and for columns 1 and 6 (col 1 is unit name and 6 is begin date.
  repUnitData <- mergedUnitData[rep(rownames(mergedUnitData ),mergedUnitData$count),c(1,6)]
  # once all the rows have a count of one, we run the quantile function
  ?quantile
  TPQ <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(unitID, MCD, blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c(originalUnitName,'MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 
# end of function EstimateMCD

# apply the function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit
MCDs <- as_tibble(MCDByUnit[["MCDs"]])
MCDs1 <- MCDs %>%
  mutate(MCD = round(MCD, 2),
         blueMCD = round(blueMCD, 2))
write.csv(MCDs1,"data_processed/MCDs/MCDS.TPQS.Sites.csv", row.names = FALSE)

# BASIC MCD BY CONTEXT ####
## 6.2 Use this to assign ContextID to the unit. 
# This time by Context
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$Context)

table(wareTypeData_Unit$unit)

#### 7. Transpose the data for the MCD and CA ####
wareByUnitT <- wareTypeData_Unit %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Count)) %>%
  spread(Ware, value=count , fill=0 )


#### 8. Remove specific ware types (if you must) and set sample size cut off  ####

# None Removed
# Here we name the types we do NOT
# want included (NOTE: You will need to add the types that are particular to your site to the select function):
wareByUnitT1 <- wareByUnitT

# 8.2  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT1[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT1[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]
# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)

# apply the MCD function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit
MCDs <- as_tibble(MCDByUnit[["MCDs"]]) 
MCDs1 <- merge(Context_basic, MCDs, by.x=c("Context"), by.y=c("unit")) %>%
  mutate(MCD = round(MCD, 2),
         blueMCD = round(blueMCD, 2))
write.csv(MCDs1,"data_processed/MCDs/MCDS.TPQS.Sites.Contexts.csv")

# BASIC MCD BY FS Group ####
## 6.2 Use this to assign ContextID to the unit. # Aggregate 
wareTypeData_Unit <- wareTypeData %>%
  mutate(unit = if_else(ProjectName == "De la Cruz", Context, FSGroup))
table(wareTypeData_Unit$unit)

#### 7. Transpose the data for the MCD and CA ####
wareByUnitT <- wareTypeData_Unit %>% group_by(Ware,unit) %>% 
  summarise(count = sum(Count)) %>%
  spread(Ware, value=count , fill=0 )

#### 8. Remove specific ware types (if you must) and set sample size cut off  ####
# 8.1 It is possible at this point to drop types you do not want in the MCD computations
wareByUnitT1 <- wareByUnitT

# 8.2  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT1[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT1[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]
# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)

# apply the function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit
MCDs <- as_tibble(MCDByUnit[["MCDs"]])
MCDs1 <- merge(Context_basic, MCDs, by.x=c("FSGroup"), by.y=c("unit")) %>%
  mutate(MCD = round(MCD, 2),
         blueMCD = round(blueMCD, 2)) %>%
  select(-c("Context", "Context ID", "Level Designation")) %>%
  distinct()
write.csv(MCDs1,"data_processed/MCDs/MCDS.TPQS.Sites.FSGroup.csv")
