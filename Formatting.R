library(lubridate)

#plates refer to large tables of patient values
#this formats FLOWSHEETROWVALUEFACT
format_plate <- function(plate){
        
        plate$FlowDate <- as.character(plate$FlowDate)
        plate$FlowTime <- as.character(plate$FlowTime)
        list <- list(DateTime = paste(plate$FlowDate,plate$FlowTime, sep = ' '))
        plate <- cbind(plate, list)
        
        plate$DateTime <- str_replace(plate$DateTime, " (?=[:digit:]{1}:[:digit:]{2}:[:digit:]{2})", " 0")
        
        time <- "[:digit:]{2}[/][:digit:]{2}[/][:digit:]{4} {1,2}[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"
        plate$DateTime <- str_extract(plate$DateTime, time)
        
        plate$DateTime <- parse_date_time(plate$DateTime, "m/d/Y H:M:S", tz = "UTC")        
        
        
        
        plate$patient_ID <- as.character(plate$patient_ID)
        unique_pt <- unique(plate$patient_ID)
        
        data = list(plate = plate, pt_list = unique_pt)
        
        data$plate <- tbl_df(data$plate)
        
        return(data)
}

#same thing for lab values.
format_lab_plate <- function(plate){
        
        plate$Lab_Collection_Date <- as.character(plate$Lab_Collection_Date)
        plate$Lab_Collection_Time <- as.character(plate$Lab_Collection_Time)
        list <- list(DateTime = paste(plate$Lab_Collection_Date,plate$Lab_Collection_Time, sep = ' '))
        plate <- cbind(plate, list)
        
        plate$DateTime <- str_replace(plate$DateTime, " (?=[:digit:]{1}:[:digit:]{2}:[:digit:]{2})", " 0")
        
        time <- "[:digit:]{2}[/][:digit:]{2}[/][:digit:]{4} {1,2}[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"
        plate$DateTime <- str_extract(plate$DateTime, time)
        
        plate$DateTime <- parse_date_time(plate$DateTime, "m/d/Y H:M:S", tz = "UTC")        
        
        plate$Patient_ID <- as.character(plate$Patient_ID)
        unique_pt <- unique(plate$Patient_ID)
        
        data = list(plate = plate, pt_list = unique_pt)
        
        return(data$plate)
}

#use AFTER formatting
#eliminate the rows you probably don't want
prune_lab_plates <- function(plate){
        plate <- tbl_df(plate)
        plate <- select(plate,LOINC_Code, DateTime, Patient_ID, Lab_Value, Lab_Result_Abnormal, Lab_Common_Name, LOINC_Name)
        return(plate)
}

prune_lab_abbreviated <- function(l_plate){
        plate <- tbl_df(l_plate)
        plate %>%
                select(Patient_ID, Lab_Collection_Date, Lab_Collection_Time, LOINC_Code, Lab_Value)
        return(plate)
}

#ENCOUNTERS formatting

#read.csv with encounters
encounters <- tbl_df(encounters)

encounter_types <- unique(encounters$Encounter_Type)
#I am sure the following encounters are wrong. But this index only works if the encounters file is never changed
#not_right_index = the numbers for the encounters that you don't want.
encounter1_types <- encounter_types[!(1:length(encounter_types) %in% not_right_index)]
encounters1 <- filter(encounters, Encounter_Type %in% encounter1_types)


format_encounters <- function(encounters){
        encounters <- tbl_df(encounters)
        encounters$X_EncounterDate <- paste("0:00:01", encounters$X_EncounterDate, sep = ' ')
        encounters$X_EncounterDate <- parse_date_time(encounters$X_EncounterDate,"H:M:S m/d/Y", tz = "UTC")
        encounters <- arrange(encounters, X_EncounterDate)
        return(encounters)
}