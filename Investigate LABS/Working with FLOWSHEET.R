
########NAME, VALUE FORM, AND DISTRIBUTION FILTERS#######

#name filter filters only variables with a name that 
#table must have:
#1 column listing variable names
#1 column listing variable keys (the "id number" of the variable)
#keys are the keys you have already filtered in. If left NULL,
#keys will not filter the table at all.
#NOTE: column names must UNABIGUOUSLY identify columns.
name_form_filter <- function(table, name_column, key_column, keys = NULL, variable_name){
        table <- tbl_df(table)
        table %>%
                select_(name_column, key_column) -> table
        str <- str_detect(as.vector(t(table[colnames(table) == name_column])), variable_name)
        table <- table[str,]
        
        if(!is.null(keys)){
                table <- table[as.vector(t(table[colnames(table) == key_column])) %in% keys,]
        }
        
        keys <- unique(as.vector(t(select(table, matches(key_column)))))
        return(keys)
}



#this is a simpler search function, but specific to the
#FLOWSHEET
key_search <- function(flow_sheet_row, search_term){
        flow_sheet_row$Name <- as.character(flow_sheet_row$Name)
        search_result <- str_detect(flow_sheet_row$Name, search_term)
        return(flow_sheet_row$FlowsheetRowKey[search_result])
}

value_form_filter <- function(table,value_column, key_column, keys = NULL, value_form, cutoff = .75){
        
        #simply rename the column names to make this whole thing easier.
        names <- names(table)
        names[names == value_column] <- "Value"
        names[names == key_column] <- "Keys"
        names(table) <- names
        
        table <- tbl_df(table)
        
        if(!is.null(keys)){
                table <- filter(table, Keys %in% keys)
        }
        
        table %>%
                select(Value, Keys) %>%
                group_by(Keys) %>%
                #is_similar(vec = Value, form = value_form, cutoff = cutoff)
                dplyr::summarise(correct_form = is_similar(vec = Value, form = value_form, cutoff = cutoff)) %>%
                filter(correct_form == TRUE) -> table
        return(table$Keys)
}

folder_value_form_filter <- function(value_column, key_column, keys = NULL, value_form, cutoff = .75){
        key_vec <- NULL
        for(file in dir()){
                table <- read.csv(file)
                key_vec <- c(key_vec, value_form_filter(table,value_column, key_column, keys = NULL, value_form, cutoff = .75))
                key_vec <- unique(key_vec)
        }
        return(key_vec)
}


#this filter is a bit tricky because it requires that you pass it functions
#these functions that test a vector of values to determine if those values have certain properties.
#the function must at least take a vector, and should probably take other parameters.
#the functions must return a boolean value.
#keys with distributions that have that property are selected. Those that don't are left out.
#when passing variables through ..., you cannot signify them in the line. They must
#be given values in the global environment
#this means, just define them before you call this function.
value_dist_filter <- function(table, value_column,
                              key_column, keys = NULL, value_func, ...){
        
        func <- match.fun(value_func)
        
        #simply rename the column names to make this whole thing easier.
        names <- names(table)
        names[names == value_column] <- "Value"
        names[names == key_column] <- "Keys"
        names(table) <- names
        
        table <- tbl_df(table)
        
        if(!is.null(keys)){
                table <- filter(table, Keys %in% keys)
        }
        
        table %>%
                select(Value, Keys) %>%
                group_by(Keys) %>%
                dplyr::summarise(correct_dist = func(Value, ...)) %>%
                filter(correct_dist == TRUE) -> table
        return(table$Keys)
}

#########DISTRIBUTION FUNCTIONS###########

#Note, all distribution functions, for whatever reason, must take ... as their other parameters
#I'm not a good enough coder to allow them to take specific parameters but be called generally.

mean_sys_range <- function(vec, ...){
        sys <- parse_sys(vec)
        m_sys <- mean(sys, na.rm = TRUE)
        return(((m_sys < max) && (m_sys > min)))
}

#checks if the mean of the numeric terms is within the
#range outlined by min and max.
mean_range <- function(vec,...){
        vec1 <- as.numeric(as.character(vec))
        #gets rid of the NAs
        m <- mean(vec1, na.rm = TRUE)
        return(((m < max) && (m > min)))
        
}

median_range <- function(vec,...){
        vec1 <- as.numeric(as.character(vec))
        #gets rid of the NAs
        m <- median(vec1, na.rm = TRUE)
        return(((m < max) && (m > min)))
        
}

parse_sys <- function(block_vec){
        block_vec <- as.character(block_vec)
        block_vec <- block_vec[!is.na(str_match(block_vec,"[:digit:]{2,3}[/][:digit:]{2,3}"))]
        sys <- (str_extract(block_vec, "[:digit:]{2,3}"))
        sys <- as.numeric(sys)
        return(sys)
}

#looks to see if the mean systolic bp is within a range

parse_dia <- function(block_vec,func = "mean"){
        block_vec <- as.character(block_vec)
        block_vec <- block_vec[!is.na(str_match(block_vec,"[:digit:]{2,3}[/][:digit:]{2,3}"))]
        print(block_vec)
        sys <- (str_extract(block_vec, "[:digit:]{2,3}"))
        sys <- as.numeric(sys)
        print(sys)
        dia <- as.numeric(str_extract(block_vec, "(?<=[/])[:digit:]{2,3}"))
        return(dia)
}

###########PERFORMING STATISTICS ON KEYS###########

#Takes a search term
#finds the keys that match the search term
#gets counts for all instances of those keys
#and prints out that list in most common to least.
key_list <- function(flow_sheet_row, plate, search_term){
        keys <- key_search(flow_sheet_row, search_term)
        #get just the relevant keys
        
        slice <- plate[plate$FlowsheetRowKey %in% keys,]
        
        if(length(keys) == 0 || nrow(slice) == 0){
                print("No keys and/or values !!!!!")
                matrix <- matrix(NA, ncol = 2, nrow = 20)
                matrix <- as.data.frame(matrix)
                names(matrix) <- c("FlowsheetRowKey","counts")
                return(matrix)
        }
        #look at the prevalence of the rows for those keys
        count <- var_count(slice)
        if(nrow(count) >= 20){
                count <- count[1:20,]
        }
        
        if(nrow(count) < 20){
                additional <- 20 - nrow(count)
                matrix <- matrix(NA, ncol = 2, nrow = additional)
                matrix <- as.data.frame(matrix)
                names(matrix) <- c("FlowsheetRowKey","counts")
                count <- rbind(count, matrix)
        }
        return(count)
}

#basically, do key_list for a bunch of search terms
key_table <- function(flow_sheet_row, plate, search_terms, var_names = NULL){
        if(is.null(var_names)){
                var_names <- search_terms
        }
        
        df <- NULL
        
        for(i in 1:length(search_terms)){
                var_name <- var_names[i]
                key_list <- key_list(flow_sheet_row, plate, search_terms[i])
                print(key_list)
                #this should just add the variable to the df
                col_names <- names(key_list)
                names(key_list) <- paste(var_name,col_names)
                print(key_list)
                if(!is.null(df)){
                        df <- cbind(df,key_list)
                }
                if(is.null(df)){
                        df <- key_list
                }
        }
        return(df)
}

#does key_list for the whole table, which in the original analysis was called
#a slice.
var_count <- function(slice){
        print("Keys that are tied for counts retain their original order")
        print("in the slice.")
        print("Returns df with col 1 = keys, col 2 = counts.")
        keys <- unique(slice$FlowsheetRowKey)
        unique_count <- length(keys)
        counts <- NULL
        for(i in 1:length(keys)){
                counts[i] <- length(slice$FlowsheetRowKey[slice$FlowsheetRowKey == keys[i]])
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
        df <- data.frame(FlowsheetRowKey = ordered_keys, counts = ordered_counts)
        return(df)
}

#AN EXTREMELY USEFUL FUNCTION. It returns 10 values from each key in the key
#list.
var_examples <- function(plate, keys, examples = 10){
        for(key in keys){
                print(which(keys == key))
                example <- as.vector(plate$Value[plate$FlowsheetRowKey == key])[1:examples]
                print(example)
        }
}