# MCD/TPQ AND CA ANALYSIS OF PHD CONTEXTS FOR JRH ####
# MCD FOR DISSERTATION ####
# MCD-CA Code.R
# Created by:  FDN  8.5.2014
# Last update: FDN 8.5.2014  
# Edited by:   LAB 1.17.2017 for Morne Patate
# Edited by:   LC 12.4.2017 Hermitage phases
# Edited by:   FDN 12.21.2018 more tidy; fixed MCD function to handle phases.
# Edited by:   CP and EB 5.28.2019 Comments and Section editing for UR code
# Edited by:   Myles Sullivan for Dissertation research 

# load the libraries
library(RPostgreSQL)
library(tidyverse)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)
library(readxl)
library(ggrepel)

#### 1. get the table with the ware type date ranges =====
# get the table with the ware type date ranges
MCDTypeTable<- read.csv(file = "data_raw/DAACS_MCDTypeTable_mod.csv", 
                        fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#### 2. load dissertation ceramics ####
PhD_ceramics <- read_excel("data_processed/spreadsheets/PhD_ceramics.xlsx")

# Load non-Dissertation contexts at 116 Broad Street
JR_post1820 <- read_excel("data_raw/116_Broad/JR_post_rutledge_ceramics.xlsx") %>%
  rename(ProjectName = `Project Name`) %>%
  select(-`Period`) %>%
  mutate(ProjectID = as.character(ProjectID))

# Load Context data
PhD_contexts <- read_excel("data_processed/spreadsheets/contexts/Phd_Combined_contexts.xlsx")
Rutledge_contexts <- PhD_contexts %>%
  filter(`Project Name` == "John Rutledge House")

# Load regional data that will be added to CA data later
Regional_data <- read_excel("data_raw/Wares_region.xlsx") %>%
  select(-`Material`)

# select data for site
Periods <- Rutledge_contexts %>%
  distinct(Context, Period)

Rutledge_ceramics <- PhD_ceramics %>%
  filter(ProjectName == "John Rutledge House") %>%
  # combine with not analyzed contexts
  full_join(., JR_post1820) %>%
  # add Excavator Period to Ceramic data
  left_join(., Periods, by = c("Context"))

# combine them
Context_basic <- Rutledge_ceramics %>%
  distinct(ProjectName, Context, `Context ID`, FSGroup, QuadratID, `Level Designation`, DepositType, Period)

# do a summary
summary1 <- Rutledge_ceramics %>%
  group_by(ProjectName, Ware) %>% 
  summarise(count = sum(Count))
options(tibble.print_min=100)
summary1

#compute the total count of ceramics
AllCeramicCount <- summary1 %>% summarise(Count=sum(count))

#### 3. Customizations to the Ware Type dates or names####
# Remove Clean-Up Contexts
wareTypeData <- Rutledge_ceramics %>%
  filter(DepositType != "Clean-Up/Out-of-Stratigraphic Context")
  
summary2 <- wareTypeData %>%
  group_by(Context, Ware) %>% 
  summarise(count = sum(Count))
options(tibble.print_min=100)
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
# wareTypeData$StratigraphicGroup[is.na(wareTypeData$StratigraphicGroup)] <- ''
wareTypeData$FeatureNumber[is.na(wareTypeData$FeatureNumber)] <- ''
wareTypeData$QuadratID[is.na(wareTypeData$QuadratID)] <- '' 
wareTypeData$FSGroup[is.na(wareTypeData$QuadratID)] <- ''
# 6.1 

# BASIC MCD BY CONTEXT ####
## 6.2 Use this to assign ContextID to the unit. 
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$Context)

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
#and therefore its pattern of occurrence is likely to be affected, can get temporal gradiant for Dim1 (REW) vs Dim 2 that capturing is more utilitarian  
#One thing -- we are doing this before we calculate MCDs NOT the seriation
#Two approaches: 1) leave them in the MCD dataframe and only take them out of the CA and then when you compare CA to the MCDs (i.e. don't
# remove them here,
#2) If you take them out here you will be doing the MCD CA comparison on the same dataset without the ware types

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
MCDs1 <- merge(Context_basic, MCDs, by.x=c("Context"), by.y=c("unit")) %>%
  # Round the MCDs Columns to 2 decimal places
  mutate_if(is.numeric, ~ round(., 2))

write.csv(MCDs1,"data_processed/MCDs/116_Broad/MCDS.TPQS.CONTEXTS.116.Broad.csv")

# MCD BY FS Group ####
## 6.2 Use this to assign ContextID to the unit. 
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$FSGroup)

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
#and therefore its pattern of occurrence is likely to be affected, can get temporal gradiant for Dim1 (REW) vs Dim 2 that capturing is more utilitarian  
#One thing -- we are doing this before we calculate MCDs NOT the seriation
#Two approaches: 1) leave them in the MCD dataframe and only take them out of the CA and then when you compare CA to the MCDs (i.e. don't
# remove them here,
#2) If you take them out here you will be doing the MCD CA comparison on the same dataset without the ware types

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
#### 9. Define functions to Remove Types w/o Dates and then compute MCDs ####
# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)
# apply the function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit
MCDs <- as_tibble(MCDByUnit[["MCDs"]])
MCDs1 <- merge(Context_basic, MCDs, by.x=c("FSGroup"), by.y=c("unit")) %>%
  select(-c("Context", "Context ID", "Level Designation", "DepositType")) %>%
  # Round the MCDs Columns to 2 decimal places
  mutate_if(is.numeric, ~ round(., 2)) %>%
  distinct()
write.csv(MCDs1,"data_processed/MCDs/116_Broad/MCDS.TPQS.FSGroup.116.Broad.csv")

# 116 Broad Street CA ANALYSIS #####
wareTypeAndSizeData <- wareTypeData
#### 3.0 Do some recoding of Ware and Genre names ####
wareTypeAndSizeData1 <- wareTypeAndSizeData %>% 
  mutate(FeatureNumber = ifelse(is.na(FeatureNumber), '', FeatureNumber),
         FeatureType = ifelse(is.na(FeatureType), '', FeatureType),
         FSGroup = ifelse(is.na(FSGroup), '', FSGroup),
         QuadratID = ifelse(is.na(QuadratID), '', QuadratID )) %>% 
  filter (DepositType != 'Clean-Up/Out-of-Stratigraphic Context',
          Context != '',
          ! is.na(Context))

#### 5.2 Summarize by Ware type and Genre, preview counts ####
wareTotals <- wareTypeAndSizeData1 %>% 
  group_by(Ware) %>% 
  summarize(totalCount = sum(Count))
wareTotals

#### 5.3 Set minimum total counts for Contexts ####
# Set the minimum Context total
#### 6.0 Remove unid. types ####
wareTypeAndSizeData2 <- wareTypeAndSizeData1 %>% 
  filter (! Ware %in% c('Unidentifiable',
                        'Refined Earthenware, unidentifiable',
                        'Native American',
                        'Caribbean Coarse Earthenware, unid.',
                        'Colonoware'))

ContextCount <- wareTypeAndSizeData2 %>% 
  group_by(Context) %>% 
  summarise(ContextCount=sum(Count))

wareTypeAndSizeData3 <- inner_join(wareTypeAndSizeData2, ContextCount) %>% 
  filter(ContextCount >= 5)


# Different Configurations to run the analysis, like in MCDs, to select further below
wareTypeAndSize_FSGroup <- wareTypeAndSizeData3 %>%
  mutate(StratigraphicZone = FSGroup)

wareTypeAndSize_Context <- wareTypeAndSizeData3 %>%
  mutate(StratigraphicZone = Context)

# Load Context MCDs for future visualizations
MCD_contexts <- read_csv("data_processed/MCDs/116_Broad/MCDS.TPQS.CONTEXTS.116.Broad.csv") %>%
  select(-c("...1", "Count", "QuadratID", "Period"))

# Load FS Group MCDs  for future visualizations
MCD_FS_Group <- read_csv("data_processed/MCDs/116_Broad/MCDS.TPQS.FSGroup.116.Broad.csv") %>% 
  select(-c("...1", "Count", "QuadratID", "Period"))

##### 9.0 Randomized in inertias for plotting #####
##### 9.1 Define function: get_Randomized_Inertias() #####
# Define a function to compute the actual proportion of inertia
# accounted for by CA dimensions and the expected proportion, based on 
# random permutation of the elements within each row of the data matrix.
# Both the mean and 95% confidence interval are computed for the randomizations.
get_Randomized_Inertias <- function(n_times, data){
  # Arguments: n_times: The number of randomizations.
  #            data:    A data frame or matrix that contains ONLY counts.
  # Function to permute elements within a row
  permute_Row <- function(row) {return(sample(row))}
  # Function to permute elements within all rows 
  permute_Matrix_Rows <-function(data){
    repeat{permuted_matrix <- t(apply(data, 1, permute_Row))
    # Make sure no colsums are zeros
    if(sum(colSums(permuted_matrix)==0)==0){break}}
    return(permuted_matrix)}
  # Function to compute % inertias
  get_Inertias <- function(data){
    p <- data/sum(data)
    # row and column marginal sums
    row_masses <- rowSums(p)
    col_masses <- colSums(p)
    # expected values
    e <- row_masses %o% col_masses
    # residual matrix
    i <- (p-e)/e
    z <- i*sqrt(e) 
    # SVD
    svd_result <- svd(z)
    # Extract the singular values as % inertias
    round(svd_result$d^2/sum(svd_result$d^2),3)
  }
  # call the functions
  random_inertias <- replicate(n = n_times, 
                               expr = get_Inertias(data 
                                                   = permute_Matrix_Rows(
                                                     data=data)))
  # get the means and CLs
  means_and_cls <- data.frame(t(apply(random_inertias, 1, function(x) 
    c(mean = mean(x), quantile(x, c(0.025, 0.975))))))
  colnames(means_and_cls) <- c('mean', 'lcl', 'ucl')
  means_and_cls$actual <- get_Inertias(data=data)
  means_and_cls$Dimension <- 1:nrow(means_and_cls)
  return(means_and_cls[-nrow(means_and_cls),])
}
##### End of function definition #### 

# Select the context grouping you want (from defined options above, by Context or FS)
# CA by Context (FS) ####
wareGenreCountsByContextT <- wareTypeAndSize_Context %>% 
  group_by(ProjectName, Period, StratigraphicZone, 
           Ware) %>% 
  summarize(Count= sum(Count)) %>% 
  pivot_wider(id_cols = c(ProjectName, Period, StratigraphicZone), 
              names_from = Ware, values_from= Count, values_fill = 0) 

# run the function
means_and_cls <- get_Randomized_Inertias(n_times=1000,wareGenreCountsByContextT [,-1:-4]  )

# plot the results
theme_set(theme_classic(base_size = 18))  
p2 <- ggplot(data=means_and_cls, aes(x = Dimension, y=actual)) +
  scale_x_continuous(limits = c(1, nrow(means_and_cls)), 
                     breaks = seq(0, nrow(means_and_cls),5)) +
  geom_line(aes(y=mean), lty=2, col='black', linewidth=1) +
  geom_ribbon(aes(x= Dimension, ymin=lcl, ymax=ucl), 
              col='gray', alpha=.1) +
  geom_line(aes(y=actual), col= 'grey', linewidth=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="CA: Ware",
        subtitle = '116 Broad St',
        x="Dimension", y='Proportion of Inertia')
p2

ggsave( p2, file = 'data_processed/CAs/116_Broad/CA_by_context/p2_Scree_Randomized_Inertias_JR.png',  
        dpi=600, width=10, height=6, scale=1) 

#####  10.0 Do the CA  #####
Matx<-as.data.frame(wareGenreCountsByContextT [,-1:-4])
rownames(Matx)<-wareGenreCountsByContextT$StratigraphicZone
ca1 <- ca(Matx)

# Put the results in data frames 
# We convert inertia to percent inertia 
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))
# We only take the first five CA dimensions for row and col scores
rowScores <- data.frame(ca1$rowcoord[,1:4], 
                        unit =ca1$rownames)
colScores <- data.frame(ca1$colcoord[,1:4], 
                        type =ca1$colnames)
colScores <- left_join(colScores, Regional_data, by = c("type" = "Ware")) %>%
  unique()


##### 10.1  scale rowcoords by SVs  #####
scaledRowScores <-  ca1$rowcoord[,1:4] * matrix(ca1$sv[1:4], 
                                                nrow= nrow(ca1$rowcoord), 
                                                ncol =4, byrow=T  )
scaledRowScores <- data.frame (unit=rownames(scaledRowScores), scaledRowScores)

scaledRowScores <- inner_join(scaledRowScores, wareGenreCountsByContextT[,1:4],
                              by=c('unit' = 'StratigraphicZone')) %>% 
  select(unit, Dim1, Dim2, Dim3, Dim4, unit, Period)

write.csv(scaledRowScores, "data_processed/CAs/116_Broad/CA_by_context/Scaled_Rows_CA_context.csv")

# add MCDs to Scaled Rows for future visualizations
MCD_scaledrows <- left_join(scaledRowScores, MCD_contexts, by=c("unit"="Context"))

#### 10.2 Broken Stick #####
# Create a function to compute the broken stick model inertia
broken.stick <- function(p)
  # Compute the expected values of the broken-stick distribution for 'p' pieces.
{
  result = matrix(0,p,2)
  colnames(result) = c("Dim","Expected.Inertia")
  for(j in 1:p) {
    E = 0
    for(x in j:p) E = E+(1/x)
    result[j,1] = j
    result[j,2] = E/p
  }
  result <- result
  return(data.frame(result))
}

# Apply the broken.stick function to the inertia dataframe
bs <- broken.stick(nrow(inertia))

# Plot the proportion of inertia

theme_set(theme_classic(base_size = 18))
p3 <- ggplot(data=inertia , aes(x= 1:length(Inertia), y=Inertia)) +
  scale_x_continuous(n.breaks = 8) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", linewidth=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            linewidth=1) +
  labs( title='CA: Ware',
        subtitle = '116 Broad Street',
        x="Dimension", y='Proportion of Inertia' ) 
p3

ggsave(p3, file = 'data_processed/CAs/116_Broad/CA_by_context/p3_BrokenStickScree_JR.png',  
       dpi=600, width=10, height=6, scale=1) 

##### 10.3  Now we plot the row scores on CA Dim 1 and Dim2 ####
set.seed(42)

theme_my <- function(base_size = 10, base_family = "sans")
{
  txt <- element_text(size = 10, colour = "black", face = "plain")
  bold_txt <- element_text(size = 12, colour = "black", face = "bold")
  
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_blank(),
      legend.position = "bottom",
      strip.background = element_blank(), 
      
      text = txt, 
      plot.title = bold_txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = txt, 
      legend.text = txt ) 
}

p4 <- ggplot(MCD_scaledrows, aes(x=Dim1,y=Dim2, fill=Period)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d(option='') +
  geom_text_repel(aes(label=FSGroup), size = 2, cex= 4, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Context Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()
p4
ggsave(p4, file = 'data_processed/CAs/116_Broad/CA_by_context/p4_ContextDim1Dim2_JR.png',  
       dpi=600)

# blue MCD and Dim1 as Plot ####
set.seed(42)
p4a <- ggplot(MCD_scaledrows, aes(x=Dim1,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=FSGroup), size=3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim1)',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep='')) +
  theme_my()
p4a
ggsave(p4a, file = 'data_processed/CAs/116_Broad/CA_by_context/p4a_ContextMCDDim1_JR.png',  
       dpi=600) 

# blue MCD and Dim2 as Plot
set.seed(42)
p4b <- ggplot(MCD_scaledrows, aes(x=Dim2,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=FSGroup), size=3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim2)',
       x = paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()

p4b
ggsave(p4b, file = 'data_processed/CAs/116_Broad/CA_by_context/p4b_ContextMCDDim2_JR.png',  
       dpi=600)

# blue MCD and Dim3 as Plot
set.seed(42)
p4c <- ggplot(MCD_scaledrows, aes(x=Dim3,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=FSGroup), size=3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim3)',
       x = paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep='')) +
  theme_my()
p4c
ggsave(p4c, file = 'data_processed/CAs/116_Broad/CA_by_context/p4c_ContextMCDDim3_JR.png',  
       dpi=600)

#### 10.4 Plot the row scores on CA Dim 1 and Dim3 #####
set.seed(42)
p5 <- ggplot(MCD_scaledrows, aes(x=Dim1,y=Dim3, fill=Period)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  geom_text_repel(aes(label=FSGroup), size = 2, cex= 4, force=2) +
  scale_fill_viridis_d() +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = "'116 Broad St. Context Scores'",
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep='')) +
  theme_my()
p5
ggsave(p5, file = 'data_processed/CAs/116_Broad/CA_by_context/p5_ContextDim1Dim3_JR.png',  
       dpi=600) 

##### 10.5  Plot the column scores on CA Dim 1 and Dim 2 ####
p6 <- ggplot(colScores, aes(x = Dim1,y = Dim2, fill=General)) +
  coord_fixed() +
  geom_point(shape=21, size=2, colour="black", alpha=.75) +
  geom_text_repel(aes(label=type), size = 2, cex= 4, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Ware Type Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()
p6
ggsave(p6, file = 'data_processed/CAs/116_Broad/CA_by_context/p6_TypeDim1Dim2_JR.png',  
       dpi=600)

##### 10.6 Plot the column  scores on CA Dim 1 and Dim 3 #####
p7 <- ggplot(colScores, aes(x = Dim1,y = Dim3, fill=General)) +
  theme_my() +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  geom_text_repel(aes(label= type), size = 2, cex = 4, max.overlaps=20) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Ware Type Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep=''))
p7
ggsave(p7, file = 'data_processed/CAs/116_Broad/CA_by_context/p7_TypeDim1Dim3_JR.png',  
       dpi=600)

# Select the context grouping you want (from defined options above, by Context or FS)
# CA by FS Group ####
wareGenreCountsByContextT <- wareTypeAndSize_FSGroup %>% 
  group_by(ProjectName, Period, StratigraphicZone, 
           Ware) %>% 
  summarize(Count= sum(Count)) %>% 
  pivot_wider(id_cols = c(ProjectName, Period, StratigraphicZone), 
              names_from = Ware, values_from= Count, values_fill = 0) 

# run the function
means_and_cls <- get_Randomized_Inertias(n_times=1000,wareGenreCountsByContextT [,-1:-4]  )

# plot the results
theme_set(theme_classic(base_size = 18))  
p2 <- ggplot(data=means_and_cls, aes(x = Dimension, y=actual)) +
  scale_x_continuous(limits = c(1, nrow(means_and_cls)), 
                     breaks = seq(0, nrow(means_and_cls),5)) +
  geom_line(aes(y=mean), lty=2, col='black', linewidth=1) +
  geom_ribbon(aes(x= Dimension, ymin=lcl, ymax=ucl), 
              col='gray', alpha=.1) +
  geom_line(aes(y=actual), col= 'grey', linewidth=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  labs( title="CA: Ware",
        subtitle = '116 Broad St',
        x="Dimension", y='Proportion of Inertia')
p2

ggsave( p2, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p2_Scree_Randomized_Inertias_JR.png',  
        dpi=600, width=10, height=6, scale=1) 

#####  10.0 Do the CA  #####
Matx<-as.data.frame(wareGenreCountsByContextT [,-1:-4])
rownames(Matx)<-wareGenreCountsByContextT$StratigraphicZone
ca1 <- ca(Matx)

# Put the results in data frames 
# We convert inertia to percent inertia 
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))
# We only take the first five CA dimensions for row and col scores
rowScores <- data.frame(ca1$rowcoord[,1:4], 
                        unit =ca1$rownames)
colScores <- data.frame(ca1$colcoord[,1:4], 
                        type =ca1$colnames)
colScores <- left_join(colScores, Regional_data, by = c("type" = "Ware")) %>%
  unique()


##### 10.1  scale rowcoords by SVs  #####
scaledRowScores <-  ca1$rowcoord[,1:4] * matrix(ca1$sv[1:4], 
                                                nrow= nrow(ca1$rowcoord), 
                                                ncol =4, byrow=T  )
scaledRowScores <- data.frame (unit=rownames(scaledRowScores), scaledRowScores)

scaledRowScores <- inner_join(scaledRowScores, wareGenreCountsByContextT[,1:4],
                              by=c('unit' = 'StratigraphicZone')) %>% 
  select(unit, Dim1, Dim2, Dim3, Dim4, unit, Period)

write.csv(scaledRowScores, "data_processed/CAs/116_Broad/CA_by_FSGroup/Scaled_Rows_CA_FSGroup.csv")

# add MCDs to Scaled Rows for future visualizations
MCD_scaledrows <- left_join(scaledRowScores, MCD_FS_Group, by=c("unit"="FSGroup"))

#### 10.2 Broken Stick #####
# Apply the broken.stick function to the inertia dataframe
bs <- broken.stick(nrow(inertia))

# Plot the proportion of inertia
theme_set(theme_classic(base_size = 18))
p3 <- ggplot(data=inertia , aes(x= 1:length(Inertia), y=Inertia)) +
  scale_x_continuous(n.breaks = 8) +
  # geom_bar(stat="identity", fill="grey") +
  geom_line(col= "grey", linewidth=2) +
  geom_point(shape=21, size=5, colour="black", fill="grey") +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            linewidth=1) +
  labs( title='CA: Ware',
        subtitle = '116 Broad Street',
        x="Dimension", y='Proportion of Inertia' ) 
p3

ggsave(p3, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p3_BrokenStickScree_JR.png',  
       dpi=600, width=10, height=6, scale=1) 

##### 10.3  Now we plot the row scores on CA Dim 1 and Dim2 ####
set.seed(42)
p4 <- ggplot(MCD_scaledrows, aes(x=Dim1,y=Dim2, fill=Period)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d(option='') +
  geom_text_repel(aes(label=unit), size =3, cex= 4, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Context Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()
p4
ggsave(p4, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p4_ContextDim1Dim2_JR.png',  
       dpi=600)

# blue MCD and Dim1 as Histogram ####
set.seed(42)
p4a <- ggplot(MCD_scaledrows, aes(x=Dim1,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=unit), size = 3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim1)',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep='')) +
  theme_my()
p4a
ggsave(p4a, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p4a_ContextMCDDim1_JR.png',  
       dpi=600) 

# blue MCD and Dim2 as Histogram
set.seed(42)
p4b <- ggplot(MCD_scaledrows, aes(x=Dim2,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=unit), size= 3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim2)',
       x = paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()

p4b
ggsave(p4b, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p4b_ContextMCDDim2_JR.png',  
       dpi=600)

set.seed(42)
p4c <- ggplot(MCD_scaledrows, aes(x=Dim3,y=blueMCD,fill=Period)) +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  scale_fill_viridis_d() +
  geom_text_repel(aes(label=unit), size = 3, cex= 4, force=2) +
  labs(title = '116 Broad St. MCD & CA (Dim3)',
       x = paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep='')) +
  theme_my()
p4c
ggsave(p4c, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p4c_ContextMCDDim3_JR.png',  
       dpi=600)

#### 10.4 Plot the row scores on CA Dim 1 and Dim3 #####
set.seed(42)
p5 <- ggplot(MCD_scaledrows, aes(x=Dim1,y=Dim3, fill=Period)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  geom_text_repel(aes(label=unit), size=3, cex= 4, force=2) +
  scale_fill_viridis_d() +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = "'116 Broad St. Context Scores'",
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep='')) +
  theme_my()
p5
ggsave(p5, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p5_ContextDim1Dim3_JR.png',  
       dpi=600) 

##### 10.5  Plot the column scores on CA Dim 1 and Dim 2 ####
p6 <- ggplot(colScores, aes(x = Dim1,y = Dim2, fill=General)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  geom_text_repel(aes(label=type), size = 3, cex= 4, force=2) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Ware Type Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')) +
  theme_my()
p6
ggsave(p6, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p6_TypeDim1Dim2_JR.png',  
       dpi=600)

##### 10.6 Plot the column  scores on CA Dim 1 and Dim 3 #####
p7 <- ggplot(colScores, aes(x = Dim1,y = Dim3, fill=General)) +
  coord_fixed() +
  geom_point(shape=21, size=3, colour="black", alpha=.75) +
  geom_text_repel(aes(label= type), size = 3, cex = 4, max.overlaps=20) +
  geom_hline(yintercept = 0, linetype='dashed', color='grey') +
  geom_vline(xintercept = 0, linetype='dashed', color='grey') +
  labs(title = '116 Broad St. Ware Type Scores',
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 3",":  ", round(inertia[3,]*100),'%', sep='')) +
  theme_my()  
p7
ggsave(p7, file = 'data_processed/CAs/116_Broad/CA_by_FSGroup/p7_TypeDim1Dim3_JR.png',  
       dpi=600)

