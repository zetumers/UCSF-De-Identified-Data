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


