#######################################################
#######################################################

#### Clean and filter raw meter data to get monthly total water volume 

#######################################################
#######################################################

#### Load Required packages
library(tidyverse)
library(readxl) ########### STOP #################
library(sf)

#### Import Raw meter data
#   11,757 records
meterDataRaw <- read_excel("Water_Meter_Data/Meter_Data_Raw.xlsx")
# view(meterDataRaw)
glimpse(meterDataRaw)

#### Count unique meter IDs
#   127 Unique meter IDs
meterDataRaw %>%
  summarise(count = n_distinct(MTR_Station_ID))

#### Select relevant properties, remove years before 2011, 
#   remove annual records, standardize unit labels
#    10,943 records
meterDataFiltered <- meterDataRaw %>% 
  select(MTR_Station_ID, MTR_Read_Start_DT, MTR_Read_End_DT, 
         MTR_Read_Start_Measurement_QT, MTR_Read_End_Measurement_QT, 
         MTR_Read_Total_Flow_QT, MTR_Read_Measurement_UOM_CD, MTR_Read_UOM_CD, 
         MTR_Read_Is_Annual_Read) %>% 
  filter(MTR_Read_Start_DT >= as.Date("2011-01-01"),
         is.na(MTR_Read_Is_Annual_Read)) %>% 
  mutate(Merged_UOM = coalesce(MTR_Read_Measurement_UOM_CD, MTR_Read_UOM_CD),
         Merged_UOM = case_when(
           tolower(trimws(Merged_UOM)) == "gal" ~ "Gal",
           tolower(trimws(Merged_UOM)) == "cf" ~ "CF",
           tolower(trimws(Merged_UOM)) == "acre-ft" ~ "Acre-ft",
           tolower(trimws(Merged_UOM)) == "100gal" ~ "100Gal",
           tolower(trimws(Merged_UOM)) == "1000gal" ~ "1,000Gal",
           tolower(trimws(Merged_UOM)) == "kgal" ~ "1,000Gal",
           tolower(trimws(Merged_UOM)) == "10gal" ~ "10Gal",
           tolower(trimws(Merged_UOM)) == "10kgal" ~ "10,000Gal",
           tolower(trimws(Merged_UOM)) == "100cf" ~ "100CF"
         )) %>%
  select(MTR_Station_ID, MTR_Read_Start_DT, MTR_Read_End_DT, 
         MTR_Read_Start_Measurement_QT, MTR_Read_End_Measurement_QT,
         MTR_Read_Total_Flow_QT, Merged_UOM) %>% 
  filter(!is.na(MTR_Read_Total_Flow_QT))

glimpse(meterDataFiltered)
# view(meterDataFiltered)

#### Confirm unit labels are consistent
unique(meterDataFiltered $Merged_UOM)

#### Add column to show the number of days between start and end meter readings
addDaysBetween <- meterDataFiltered %>%
   mutate(
     daysBetween = as.numeric(difftime(MTR_Read_End_DT, MTR_Read_Start_DT, units = "days"))
  )

glimpse(addDaysBetween)
#view(addDaysBetween)

#### Filter based on days between column to 
#   keep only Days between 0 and 94
#   Exclude days between 10 and 26
#   Exclude days between 34 and 56 - readings need to be at monthly intervals 
#   Exclude days between 64 and 86 - readings need to be at monthly intervals
#   9,674 records
limitDaysBetween <- addDaysBetween %>% 
  filter(daysBetween > 0 &
           daysBetween <= 94 & 
           !(daysBetween > 34 & daysBetween < 56) &
           !(daysBetween > 64 & daysBetween < 86) &
           !(daysBetween > 10 & daysBetween < 26) 
        )

glimpse(limitDaysBetween)
# view(limitDaysBetween)


#### View all units still included in the data
unique(limitDaysBetween $Merged_UOM)

#### Convert all units to cubic meters
convertUnits <- limitDaysBetween %>% 
  mutate(
    unit = "m³",  # Assign "m³" to all rows
    totalVolume = case_when(
      Merged_UOM == "Gal" ~ MTR_Read_Total_Flow_QT * 0.00378541,
      Merged_UOM == "CF" ~ MTR_Read_Total_Flow_QT * 0.0283168,
      Merged_UOM == "Acre-ft" ~ MTR_Read_Total_Flow_QT * 1233.48,
      Merged_UOM == "1,000Gal" ~ MTR_Read_Total_Flow_QT * 3.78541,
      Merged_UOM == "100Gal" ~ MTR_Read_Total_Flow_QT * 0.378541,
      Merged_UOM == "10Gal" ~ MTR_Read_Total_Flow_QT * 0.0378541,
      Merged_UOM == "10,000Gal" ~ MTR_Read_Total_Flow_QT * 37.85412,
      TRUE ~ NA_real_),
    startVolume = case_when(
      Merged_UOM == "Gal" ~ MTR_Read_Start_Measurement_QT * 0.00378541,
      Merged_UOM == "CF" ~ MTR_Read_Start_Measurement_QT * 0.0283168,
      Merged_UOM == "Acre-ft" ~ MTR_Read_Start_Measurement_QT * 1233.48,
      Merged_UOM == "1,000Gal" ~ MTR_Read_Start_Measurement_QT * 3.78541,
      Merged_UOM == "100Gal" ~ MTR_Read_Start_Measurement_QT * 0.378541,
      Merged_UOM == "10Gal" ~ MTR_Read_Start_Measurement_QT * 0.0378541,
      Merged_UOM == "10,000Gal" ~ MTR_Read_Start_Measurement_QT * 37.85412,
      TRUE ~ NA_real_),
    endVolume = case_when(
      Merged_UOM == "Gal" ~ MTR_Read_End_Measurement_QT * 0.00378541,
      Merged_UOM == "CF" ~ MTR_Read_End_Measurement_QT * 0.0283168,
      Merged_UOM == "Acre-ft" ~ MTR_Read_End_Measurement_QT * 1233.48,
      Merged_UOM == "1,000Gal" ~ MTR_Read_End_Measurement_QT * 3.78541,
      Merged_UOM == "100Gal" ~ MTR_Read_End_Measurement_QT * 0.378541,
      Merged_UOM == "10Gal" ~ MTR_Read_End_Measurement_QT * 0.0378541,
      Merged_UOM == "10,000Gal" ~ MTR_Read_End_Measurement_QT * 37.85412,
      TRUE ~ NA_real_)) %>% 
    select(MTR_Station_ID, MTR_Read_Start_DT, MTR_Read_End_DT, daysBetween,
           startVolume, endVolume, totalVolume, unit) %>% 
    rename(meterID = MTR_Station_ID, 
           startDate = MTR_Read_Start_DT, 
           endDate = MTR_Read_End_DT)

head(convertUnits)
glimpse(convertUnits)
#view(convertUnits)

#### Confirm all units are the same
unique(convertUnits $unit)


#### For records with single day to 4 day observations without a startVolume or 
#   endVolume, none account for more than 24 days in a single month.
#   Thus all records where daysBetween = 1 and startVolume = na need to 
#   be filtered out

#### Filter out records where daysBetween = 1,2,3, or 4 and startVolume = na
#    8,982 records
filterOutStartNa <- convertUnits %>%
  filter((!(daysBetween %in% c(1,2,3,4) & is.na(startVolume))))

glimpse(filterOutStartNa)
# view(filterOutStartNa)


#### For records with more than 26 filter out any that do not have start and end dates within the 
#   first 4 days on the beginning of the month
#   8,630 records
filterDateStart <- filterOutStartNa %>% 
  filter(
    (daysBetween < 26) |
    (((daysBetween >= 26 & day(startDate) <= 4) |
         (daysBetween >= 26 & abs(day(startDate) - days_in_month(startDate)) <= 4)) &
        
           ((daysBetween >= 26 & day(endDate) <= 4) |
              (daysBetween >= 26 & abs(day(endDate) - days_in_month(endDate)) <= 4)))
        )

# view(filterDateStart)

#####################################################
#####################################################

#### Split data by records with more and less than 26 days to handle cases differently

#####################################################

#### Greater than 26 days

#### Rename and select relevant columns and set start and end dates to the first of the month
#   513 records
greaterThan <- filterDateStart %>% 
  filter(daysBetween >= 26) %>% 
  rename(monthlyVolume = totalVolume) %>% 
  select(meterID, startDate, endDate, daysBetween, monthlyVolume) %>% 
  mutate(
    startDate = floor_date(startDate + days(10), "month"),
    endDate = floor_date(endDate + days(10), "month")
  ) %>% 
  rename(totalDays = daysBetween,
         totalVolume = monthlyVolume)

head(greaterThan)
# view(greaterThan)

#####################################################

#### Less than 26 days

#### Incomplete records only including total volume
#   470 records
lessThanNull <- filterDateStart %>% 
  filter(daysBetween <26 & is.na(startVolume))

head(lessThanNull)
# view(lessThanNull)

#### Complete records with start and end date volumes
#   7,647 records
lessThanComplete <- filterDateStart %>% 
  filter(daysBetween < 26 & !is.na(startVolume))
head(lessThanComplete)
# view(lessThanComplete)


#### Define a function to assign the majority month to records that overlap the 1st of the month
majorityMonthFunction <- function(start, end) {
  # Generate a sequence of dates covering the full interval
  fullInterval <- seq.Date(as_date(start), as_date(end), by = "day")
  
  # Count the number of days in each month
  monthCounts <- table(floor_date(fullInterval, "month"))
  
  # Assign the month with the most days, for ties (ie. 2 days in each month) it will select the first month
  majorityMonth <- as_date(names(which.max(monthCounts)))
  
  return(majorityMonth)
}
#####################################################

#### Separate workflows for complete and incomplete records with less than 26 days between

#####################################################

#### Calculate monthly total for INCOMPLETE records

#### Apply majority month function to each record
addMajorityMonthNull <- lessThanNull %>%
  rowwise() %>%
  mutate(majorityMonth = majorityMonthFunction(startDate, endDate)) %>%
  ungroup()

head(addMajorityMonthNull)
# view(addMajorityMonthNull)

#### Group by meter ID and majority month to calculate total monthly volume for months with enough records
#   45 Records
totalVolumeNull <- addMajorityMonthNull %>% 
  group_by(meterID, majorityMonth) %>% 
  summarise(
    # Sum the daysBetween
    totalDays = sum(daysBetween, na.rm = TRUE), 
    # Only calculate monthly volume for months with sufficient data
    monthlyVolume = ifelse(totalDays >= 27 & totalDays <=34, sum(totalVolume, na.rm = TRUE), NA_real_),  
    .groups = "drop"
  ) %>% 
    # Remove months with NA monthlyVolume
  filter(!is.na(monthlyVolume)) %>% 
  mutate(
    startDate = majorityMonth,
    endDate = ceiling_date(startDate, "month")
  ) %>% 
  select(meterID, startDate, endDate, totalDays, monthlyVolume) %>% 
  rename(totalVolume = monthlyVolume)

# view(totalVolumeNull)

###########################################

#### Calculate monthly total for COMPLETE records with less than 26 days between

#### Filter for only consecutive records
#   7,565 records
consecutiveDates <- lessThanComplete %>% 
  arrange(meterID, startDate) %>%  # Ensure chronological order
  group_by(meterID) %>%  # Check within each meterID
  mutate(prevEndDate = lag(endDate)) %>%  # Get the previous row's endDate
  filter(!is.na(prevEndDate) | startDate > prevEndDate + days(1)) %>%  # Identify gaps
  ungroup()

# view(consecutiveDates)

#### Apply majority month function to each row
addMajorityMonthCompleteConsecutive <- consecutiveDates %>%
  rowwise() %>%
  mutate(majorityMonth = majorityMonthFunction(startDate, endDate)) %>%
  ungroup()

head(addMajorityMonthCompleteConsecutive)
# view(addMajorityMonthCompleteConsecutive)

#### Group by meter ID and majority month to calculate total monthly volume for months with enough records
#    1,002 records
totalVolumeCompleteConsecutive <- addMajorityMonthCompleteConsecutive %>% 
  group_by(meterID, majorityMonth) %>% 
  summarise(
    # Sum the daysBetween
    totalDays = sum(daysBetween, na.rm = TRUE), 
    # Only calculate monthly volume for months with sufficient data
    monthlyVolume = ifelse(totalDays >= 26 & totalDays <=34, sum(totalVolume, na.rm = TRUE), NA_real_),  
    .groups = "drop"
  ) %>% 
  #### Remove months with NA monthlyVolume
  filter(!is.na(monthlyVolume)) %>% 
  mutate(
    startDate = majorityMonth,
    endDate = ceiling_date(startDate, "month")
  ) %>% 
  select(meterID, startDate, endDate, totalDays, monthlyVolume) %>% 
  rename(totalVolume = monthlyVolume)

# view(totalVolumeCompleteConsecutive)

###########################################

#### Filter for non-consecutive records (none are usable)
#   126 records
nonConsecutiveDates <- lessThanComplete %>% 
  arrange(meterID, startDate) %>%  # Ensure chronological order
  group_by(meterID) %>%  # Check within each meterID
  mutate(prevEndDate = lag(endDate)) %>%  # Get the previous row's endDate
  filter(is.na(prevEndDate) | startDate > prevEndDate + days(1)) %>%  # Identify gaps
  ungroup() %>% 
  filter(
    abs(day(startDate) - days_in_month(startDate)) <= 4 |
    abs(day(endDate) - days_in_month(endDate)) <= 4
  )
# view(nonConsecutiveDates)

#### Apply function to each row
addMajorityMonthNonConsecutive <- nonConsecutiveDates %>%
  rowwise() %>%
  mutate(majorityMonth = majorityMonthFunction(startDate, endDate)) %>%
  ungroup()

head(addMajorityMonthNonConsecutive)
# view(addMajorityMonthNonConsecutive)

#### None of these records are usable, not enough days to create monthly totals

#######################################################
#######################################################

#### Combine workflows

##### Combine all three cases, greater than 26 days, less than 26 days with null start and end dates, 
#   and less than 26 days with start and end dates
#   1,560 records
monthlyMeterData <- bind_rows(greaterThan, totalVolumeNull, totalVolumeCompleteConsecutive) %>%
  arrange(meterID, startDate)  # Sort by meterID and startDate

head(monthlyMeterData)
# view(monthlyMeterData)

#### Count unique meter IDs
#   89 unique ids
unique(monthlyMeterData$meterID)
monthlyMeterData %>%
  summarise(count = n_distinct(meterID))

#### Write cleaned monthly meter data to csv
# write.csv(monthlyMeterData, "C:/WRIA1_Water_Use_Analysis/R_Analysis/Water_Meter_Data/monthlyMeterData.csv", row.names = FALSE)


#######################################################
#######################################################
#######################################################
#######################################################

#### Clean and filter water right record number and meter ids table

#######################################################
#######################################################

#### Load record data

recordDataRaw <- read_excel("Water_Meter_Data/record_data_clean.xlsx")
head(recordDataRaw)
# view(recordDataRaw)

#### Count number of unique meter IDs in record table
#   106 unique meter IDs
recordDataRaw %>%
  summarise(count = n_distinct(meterID))

#### Keep only records with meter IDs that match monthlyMeterData
filteredRecordData <- recordDataRaw %>%
  semi_join(monthlyMeterData, by = "meterID")

head(filteredRecordData)
# view(filteredRecordData)

#### Count number of shared meter Ids
#   76 shared meter Ids
filteredRecordData %>%
  summarise(count = n_distinct(meterID))
unique(filteredRecordData$recordNumber)

#### Write filtered record data to csv
# write.csv(filteredRecordData, "C:/WRIA1_Water_Use_Analysis/R_Analysis/Water_Meter_Data/filteredRecordData.csv", row.names = FALSE)

#######################################################

#### There is a many to many relationship between record numbers and Meter Ids
#   Create a table with group numbers as rows with all associated record numbers and meter Ids

#### Create record number and meter ID groups
#   Step 1: Group by recordNumber and collect meterIDs
recordGroups <- filteredRecordData %>%
  group_by(recordNumber) %>%
  summarise(meters = list(sort(unique(meterID))), .groups = "drop")

#   Step 2: Identify unique groups based on shared meter sets
uniqueGroups <- recordGroups %>%
  group_by(meters) %>%
  summarise(records = list(sort(unique(recordNumber))), .groups = "drop") %>%
  mutate(groupID = row_number())

#   Step 3: Expand into wide format
expandGroups <- uniqueGroups %>%
  mutate(
    records = map(records, ~ set_names(.x, paste0("record", seq_along(.x)))),
    meters = map(meters, ~ set_names(.x, paste0("meter", seq_along(.x))))
  ) %>%
  unnest_wider(records, names_sep = "") %>%
  unnest_wider(meters, names_sep = "")

#   Step 4: Rename and select Group Ids (44 record groups with up to 9 record numbers and 8 meter Ids)
groupedRecordData <- expandGroups %>% 
  mutate(groupID = groupID, record1 = recordsrecord1, record2 = recordsrecord2, record3 = recordsrecord3, 
         record4 = recordsrecord4, record5 = recordsrecord5, record6 = recordsrecord6,
         record7 = recordsrecord7, record8 = recordsrecord8, record9 = recordsrecord9,
         meter1 = metersmeter1, meter2 = metersmeter2, meter3 = metersmeter3, 
         meter4 = metersmeter4, meter5 = metersmeter5, meter6 = metersmeter6, 
         meter7 = metersmeter7, meter8 = metersmeter8) %>% 
  select(groupID, record1, record2, record3, record4, record5, record6, record7, 
         record8, record9, meter1, meter2, meter3, meter4, meter5, meter6, 
         meter7, meter8)

glimpse(groupedRecordData)
# view(groupedRecordData)

#### Write Grouped Record Data to CSV
# write.csv(groupedRecordData, "C:/WRIA1_Water_Use_Analysis/R_Analysis/Water_Meter_Data/groupedRecordData.csv", row.names = FALSE)

#### Show missing IDs
missingMeterIDs <- recordDataRaw %>%
  anti_join(monthlyMeterData, by = "meterID") %>%
  distinct(meterID)

# view(missingMeterIDs)

#### Join to monthly meter data
#   3,945 records

monthlyRecords <- monthlyMeterData %>%
  left_join(recordDataRaw, by = "meterID") %>% 
  select(recordNumber, meterID, startDate, endDate, totalDays, totalVolume, purpose, notes) %>% 
  arrange(recordNumber, meterID, startDate)

head(monthlyRecords)
# view(monthlyRecords)

#### 89 unique meter IDs
unique(monthlyRecords$meterID)

monthlyRecords %>%
  summarise(count = n_distinct(meterID))

#######################################################
#######################################################

#### Bring in shapefiles of testFields features as determined in ArcGIS Pro
#   by water right Place of use polygons and water right records numbers and clipped to WSDA fields

#### Set directory containing the shapefiles
shapefileFolder <- "C:/WRIA1_Water_Use_Analysis/R_Analysis/Water_Meter_Data/recordGroupBoundaries"

#### Get a list of all shapefiles
shapefiles <- list.files(shapefileFolder, pattern = ".shp$", full.names = TRUE)
print(shapefiles)

#### Read all shapefiles and ensure all columns are retained
shapefileList <- lapply(shapefiles, function(x) {
  sf_obj <- st_read(x, quiet = TRUE)  # Read shapefile
  sf_obj <- mutate(sf_obj, source_file = basename(x))  # Add a column to track source
  return(sf_obj)
})

#### Combine all shapefiles while retaining all columns
testFieldsRough <- bind_rows(shapefileList)
glimpse(testFieldsRough)
# view(testFieldsRough)

#### Fix column group number column name and select columns
testFieldsClean <- testFieldsRough %>% 
  mutate(groupNumber = groupNumbe) %>% 
  select(groupNumber, startDate, endDate, geometry, notes_1)
head(testFieldsClean)
glimpse(testFieldsClean)
# view(testFieldsClean)

#### Group by groupNumber and startDate, then merge geometries and sum areas
testFieldsMerged <- testFieldsClean %>%
  group_by(groupNumber, startDate, endDate) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

head(testFieldsMerged)
# view(testFieldsMerged)

#######################################################
#######################################################

#### Test Fields are yearly based on the WSDA fields layer.
#   Need to create monthly fields to match with water metr data

#### Function to create monthly intervals
addMonthsFunction <- function(testFieldsMerged) {
  testFieldsMerged %>%
    rowwise() %>%
    mutate(
      month_seq = list(seq(from = startDate, to = endDate, by = "month"))
    ) %>%
    unnest(month_seq) %>%
    mutate(
      startDate = month_seq,
      endDate = startDate %m+% months(1) # Add one month to get the next endDate
    ) %>%
    select(-month_seq) %>%
    ungroup() # Remove rowwise grouping
}

#### Apply function to shapefile field boundaries
testFieldsMonthly <- addMonthsFunction(testFieldsMerged)
head(testFieldsMonthly)
# view(testFieldsMonthly)


#######################################################
#######################################################

#### Bring in grouped record data Final master key after manual cleaning in Excel

groupedRecordDataFinal <- read_csv("Water_Meter_Data/groupedRecordDataFinal.csv")
head(groupedRecordDataFinal)
# view(groupedRecordDataFinal)

#######################################################
#######################################################

#### Add total water volume to testFieldsMonthly using records dataset as key

#### Step 1: Link meterID to groupNumber using groupedRecordDataFinal
meterGroupSummary <- groupedRecordDataFinal %>%
  pivot_longer(cols = starts_with("meter"), names_to = "meterCol", values_to = "meterID") %>%  # Unnest meter columns
  select(groupID, meterID) %>%
  inner_join(monthlyMeterData, by = c("meterID" = "meterID")) %>%  # Join with monthlyMeterData
  group_by(groupID, startDate) %>%
  summarise(waterVolume = sum(totalVolume, na.rm = TRUE), .groups = "drop")  # Sum totalVolume

#### Step 2: Join aggregated waterVolume back to testFieldsMonthly and filter out special cases based on record notes from testFieldsClean
testFieldsMeterData <- testFieldsMonthly %>%
  left_join(meterGroupSummary, by = c("groupNumber" = "groupID", "startDate" = "startDate")) %>% 
  filter(!is.na(waterVolume))
#   869 records
head(testFieldsMeterData)
# view(testFieldsMeterData)

#### Filter out unreliable record based on notes from testFieldsClean
#   780 Records
testFieldsMeterDataEdit<- testFieldsMeterData %>% 
  filter(!(groupNumber == 40 & lubridate::year(startDate) == 2018),
         !(groupNumber == 19 & lubridate::year(startDate) == 2019),
         !(groupNumber == 11),
         !(groupNumber == 21),
         !(groupNumber == 22),
         !(groupNumber == 24)) 
count(testFieldsMeterDataEdit)
head(testFieldsMeterDataEdit)

testFieldsMeterDataEdit %>%
  count(startDate, groupNumber, waterVolume) %>%
  filter(n > 1)

#### Remove duplicate records from Test fields dataframe
#    763 records
testFieldsMeterDataFinal <- testFieldsMeterDataEdit %>%
  group_by(groupNumber, startDate) %>%
  slice(1) %>%  # Keeps only the first row per group
  ungroup()
# view(testFieldsMeterDataFinal)
count(testFieldsMeterDataFinal)

#### Count number of unique groups
#   30 groups
testFieldsMeterDataFinal %>%
  summarise(count = n_distinct(groupNumber))


#### Export shapefiles with water meter data attached
# st_write(testFieldsMeterDataFinal, "testFieldsMeterData.shp", delete_dsn = TRUE)
