####THESE FUNCTIONS ARE DESIGNED TO DETERMINE WHICH VALUE
####IS THE MOST "KEY-LIKE". It's "LOINC_Code", But I will let
####you determine that for yourself.

####################ARE THE LABELS INJECTIVE?!?################
plate <- lab_plate

plate %>%
        group_by(LOINC_Code) %>%
        dpylr::summarize(values = length(unique(LOINC_Name))) -> plate2

plate %>%
        group_by(LOINC_Name) %>%
        dplyr::summarize(values = length(unique(LOINC_Code))) -> plate3

plate %>%
        group_by(LOINC_Name) %>%
        dpylr::summarize(values = length(unique(Lab_Common_Name))) -> plate4

plate %>%
        group_by(Lab_Common_Name) %>%
        dpylr::summarize(values = length(unique(LOINC_Name))) -> plate5

plate %>%
        group_by(Lab_Common_Name) %>%
        dplyr::summarize(values = length(unique(LOINC_Code))) -> plate6

plate %>%
        group_by(LOINC_Code) %>%
        dplyr::summarize(values = length(unique(Lab_Common_Name))) -> plate7

lab_name_to_key <- function(plate){
        plate <- tbl_df(plate)
        plate %>%
                group_by(Lab_Common_Name) %>%
                dplyr::summarize(key = LOINC_Code[1]) -> plate
        plate$Lab_Common_Name <- as.character(plate$Lab_Common_Name)
        return(plate)
}

###########

#these functions are similar to the
#functions for non-LOINC values.

lab_name_key <- lab_name_to_key(lab_plate)

lab_search <- function(lab_name_key, search_term){
        rows <- str_detect(lab_name_key$Lab_Common_Name, search_term)
        return(lab_name_key[rows,])
        
}

lab_var_count <- function(plate){
        print("Keys that are tied for counts retain their original order")
        print("in the slice.")
        print("Returns df with col 1 = keys, col 2 = counts.")
        keys <- as.character(unique(plate$Lab_Common_Name))
        unique_count <- length(keys)
        counts <- NULL
        for(i in 1:length(keys)){
                counts[i] <- length(plate$Lab_Common_Name[plate$Lab_Common_Name == keys[i]])
        }
        ordered_keys <- NULL
        ordered_counts <- NULL
        for(i in 1:unique_count){
                
                #put the key in the right order
                ordered_keys[i] <- keys[which.max(counts)]
                
                ordered_counts[i] <- max(counts)
                #delete the key from the key vector
                keys <- keys[-which.max(counts)]
                counts <- counts[-which.max(counts)]
        }
        df <- data.frame(keys = ordered_keys, counts = ordered_counts)
        return(df)
}

find_common_names <- function(plate, cutoff = .05){
        key_list <- lab_var_count(plate)
        counts <- key_list$counts
        common <- counts > quantile(counts,cutoff)
        return(key_list$keys[common])
}

#