#######################################################
#######################################################

#### Clean and filter water right record number and meter ids table

#######################################################
#######################################################

#### Load Required packages
library(tidyverse)
library(readxl)

#### Load record data and cleaned monthly meter data from csv
recordDataRaw <- read_excel("Water_Meter_Data/record_data_clean.xlsx")
monthlyMeterData <- read_csv("Water_Meter_Data/monthlyMeterData.csv")
head(recordDataRaw)
head(monthlyMeterData)
# view(recordDataRaw)
# view(monthlyMeterData)

#### Count number of unique meter IDs in record table
#   106 unique meter IDs
recordDataRaw %>%
  summarise(count = n_distinct(meterID))

#### Keep only records with meter IDs that match monthlyMeterData
filteredRecordData <- recordDataRaw %>%
  semi_join(monthlyMeterData, by = "meterID")

head(filteredRecordData)
# view(filteredRecordData
     
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
# Step 1: Group by recordNumber and collect meterIDs
recordGroups <- filteredRecordData %>%
  group_by(recordNumber) %>%
  summarise(meters = list(sort(unique(meterID))), .groups = "drop")

# Step 2: Identify unique groups based on shared meter sets
uniqueGroups <- recordGroups %>%
  group_by(meters) %>%
  summarise(records = list(sort(unique(recordNumber))), .groups = "drop") %>%
  mutate(groupID = row_number())

# Step 3: Expand into wide format
expandGroups <- uniqueGroups %>%
  mutate(
    records = map(records, ~ set_names(.x, paste0("record", seq_along(.x)))),
    meters = map(meters, ~ set_names(.x, paste0("meter", seq_along(.x))))
  ) %>%
  unnest_wider(records, names_sep = "") %>%
  unnest_wider(meters, names_sep = "")

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

#### write grouped record and meter ID data to csv
#write.csv(groupedRecordData, "C:/WRIA1_Water_Use_Analysis/R_Analysis/Water_Meter_Data/groupedRecordData.csv", row.names = FALSE)

#######################################################

#### Add record numbers column to monthly meter data

#### Show missing IDs
#   30 IDs
missingMeterIDs <- recordDataRaw %>%
  anti_join(monthlyMeterData, by = "meterID") %>%
  distinct(meterID)
# view(missingMeterIDs)

#### Join raw record data to monthly meter data
monthlyRecords <- monthlyMeterData %>%
  left_join(recordDataRaw, by = "meterID") %>% 
  select(recordNumber, meterID, startDate, endDate, totalDays, totalVolume, purpose, notes) %>% 
  arrange(recordNumber, meterID, startDate)

head(monthlyRecords)
# view(monthlyRecords)

#### count unique meter IDs in monthly meter data
# 89 unique meter IDs
unique(monthlyRecords$meterID)
monthlyRecords %>%
  summarise(count = n_distinct(meterID))

#######################################################


