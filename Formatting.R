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

#use after formatting both of these non-loinc values and labs
add_vitals_labs <- function(plate, lab_plate){
        
        print("WARNING: GETS RID OF *Deleted IDS")
        
        #trim down the vitals
        plate %>%
                select(FlowsheetRowKey, Value, DateTime, patient_ID) -> plate
        
        #trim down the lab values
        lab_plate %>%
                select(LOINC_Code, Lab_Value, DateTime, Patient_ID) %>%
                filter(LOINC_Code != "*Deleted") %>%
                filter(str_detect(LOINC_Code,"[:digit:]")) %>%
                rename(FlowsheetRowKey = LOINC_Code,Value = Lab_Value, DateTime = DateTime,patient_ID = Patient_ID) -> lab_plate
        
        
        
        lab_plate$FlowsheetRowKey <- as.numeric(str_replace(lab_plate$FlowsheetRowKey,"-","0")) + 48441
        
        total_df <- rbind(plate,lab_plate)
        return(total_df)
}


dist_from_date <- function(date, plate){
        plate <- tbl_df(plate)
        plate <- filter(plate, DateTime >= date)
        dists <- plate$DateTime - date
        units(dists) <- "secs"
        dists <- dists[order(dists)]
        return(dists)
}

dist_unordered <- function(date, plate){
        plate <- tbl_df(plate)
        plate <- filter(plate, DateTime >= date)
        dists <- plate$DateTime - date
        units(dists) <- "secs"
        return(dists)
}

#returns the values of a plate that are ahead of the
#date (or, realistically, on that date)
plate_ahead <- function(date, plate){
        plate <- tbl_df(plate)
        plate <- filter(plate, DateTime >= date)
        return(plate)
}

#doesn't take plates
#goes from seconds to hours
#orders the distances(fuck)
dist_from_date2 <- function(date, times){
        times <- times[times >= date]
        dists <- times - date
        units(dists) <- "secs"
        dists <- dists[order(dists)]
        return(dists)
}

#takes in ordered times, finds interval between
#them
dist_dist <- function(times){
        ahead <- c(times[2:length(times)],times[length(times)])
        dists <- ahead - times
        units(dists) <- "secs"
        return(dists)
}

#assumes we're working in seconds
#returns an index of the breaks.
find_breaks <- function(times, break_size){
        differences <- dist_dist(times)
        break_after <- differences > break_size
        return(which(break_after))
        
}

#returns the part of a plate after
# the encounter break until the break.
#this fucker actually works!!
plate_until_break <- function(date, plate, break_size = hours_24){
        plate_ahead <- plate_ahead(date,plate)
        #check that this is the right ordering.
        
        #return if it is zero
        if(dim(plate_ahead)[1] == 0){
                return(plate_ahead)
        }
        
        plate_ahead <- arrange(plate_ahead, DateTime)
        dists_ahead <- dist_unordered(date, plate_ahead)
        
        #so dists_ahead and plate_ahead should match. In other words,
        #dists_ahead is just the column of the times ahead of the date.
        
        #now we have to just get the next break.
        breaks <- find_breaks(dists_ahead, break_size = break_size)
        
        if(length(breaks) == 0){
                return(plate_ahead)
        }
        next_break <- min(breaks)
        encounter_plate <- plate_ahead[1:next_break,]
        return(encounter_plate)
}

strip_time <- function(datetime){
        datetime <- as.character(datetime)
        date <- str_extract(datetime, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")
        return(date)
}

#try to implement without building a new data frame, but instead just filling one in
#that is already the right size.
#The purpose of this addition is to minimize the calls to rbind
#which takes a lot of time.
parse_as_different_patients <- function(plate, encounters, break_size = hours_24){
        
        patient_ids <- as.character(unique(plate$patient_ID))
        
        if(!all(patient_ids %in% as.character(as.factor((encounters$Patient_ID))))){
                print("Inadequate coverage by encounters, please return.")
                return(patient_ids[!(patient_ids %in% encounters$Patient_ID)])
        }
        
        #create a dataframe that is the right size to begin with.
        #we know that the new dataframe cannot be bigger than the original dataframe
        df <- NULL
        #this will index where to start filling in and where to stop
        row_start <- 1
        row_end <- NA
        
        for(pt in patient_ids){
                pt_plate <- filter(plate, patient_ID == pt)
                pt_encounters <- filter(encounters, Patient_ID == pt)
                encounter_dates <- pt_encounters$X_EncounterDate
                enc_index <- 1
                
                while(enc_index <= length(encounter_dates)){
                        new_df <- plate_until_break(encounter_dates[enc_index],
                                                    pt_plate,
                                                    break_size = break_size)
                        
                        #if the addition is empty, then don't worry about it!
                        #go to the next addition
                        if(dim(new_df)[1] == 0){
                                enc_index <- enc_index + 1
                                print(enc_index)
                                print("breaking!")
                                break
                        }
                        
                        new_df %>%
                                #this is going to need to be fixed to get rid of the time.
                                mutate(patient_ID = paste(patient_ID[1], strip_time(encounter_dates[enc_index]), sep = '!')) %>%
                                select(patient_ID, Value, FlowsheetRowKey, DateTime) -> new_df
                        
                        #if the addition is the first one,
                        #create the resultant df
                        if(is.null(df)){
                                df <- new_df
                                #print(new_df[1:10,])
                                #print(df[1:10,])
                                
                                
                                #I realize this could be more efficient,
                                #but I am having difficulty transferring the date
                                #class, and formatting is difficult at this point.
                                plate_length <- dim(plate)[1]
                                Date <- rep(new_df$DateTime[1], plate_length)
                                FlowRow <- as.numeric(rep(NA, plate_length))
                                Val <- as.factor(rep(NA, plate_length))
                                pat_ID <- as.character(rep(NA, plate_length))
                                append_df <- data.frame(FlowsheetRowKey = FlowRow,
                                                        Value = Val,
                                                        DateTime = Date,
                                                        patient_ID = pat_ID)
                                df <- rbind(df,append_df)
                        }
                        
                        #add the new_df to the df.
                        row_end <- row_start + dim(new_df)[1] - 1
                        df[row_start:row_end,] <- new_df
                        
                        #update the rows
                        row_start <- row_end + 1
                        
                        #print(enc_index)
                        
                        #find the next encounter
                        later_encounter <- encounter_dates > max(new_df$DateTime)
                        #make sure break is what you're looking for.
                        #if there aren't any later encounters, break out of
                        #this while loop.
                        if(!any(later_encounter)){
                                break
                        }
                        #update encounter so that the next encounter
                        #is one that occurs after the last encounter
                        #ended.
                        enc_index <- min(which(later_encounter))
                }
        }
        
        #find the rows that are still empty
        full_rows <- !is.na(df$patient_ID) & !is.na(df$FlowsheetRowKey)
        df <- df[full_rows,]
        return(df)
}