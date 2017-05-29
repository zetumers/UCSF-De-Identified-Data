library(stringr)
library(xlsx)
library(gtools)
library(stringdist)
library(randomForest)
library(rpart)
library(ROCR)
library(pscl)
library(neuralnet)
library(lubridate)
library(dplyr)
library(mi)
detach("package:plyr", unload = TRUE)


get_rows <- function(file_name, chunk_size){
        con = file(file_name, "r")
        lines <- 0
        while((new_lines <- length(readLines(con,chunk_size)))>0){
                lines <- lines + new_lines
        }
        close(con)
        #minus 1 for the header
        return(lines - 1)
}

#for all future pulls, put the row number you find in row_num
#it allows the program to read the csv file in huge chunks.

###pulls the whole database
#not really useful
pull_db <- function(file_name, row_num, chunk_size){
        header <- colnames(read.csv(file_name, header = TRUE, nrows = 2, stringsAsFactors = TRUE))
        con = file(file_name, "r")
        new_df <- read.csv(con, nrow = chunk_size, stringsAsFactors = TRUE, header = TRUE)
        rows_left <- row_num - chunk_size
        while(rows_left > 0){
                bite <- min(chunk_size,rows_left)
                df <- read.csv(con, nrow = bite, stringsAsFactors = TRUE, header = FALSE)
                colnames(df) <- header
                new_df <- rbind(new_df, df)
                rows_left <- rows_left - bite
                print(c(bite,rows_left, nrow(new_df)))
        }
        close(con)
        return(new_df)
}

#combine this function with piper

piper_unique_pts <- function(output, df){
        pts <- unique(c(output,unique(df$patient_ID)))
        return(pts)
}

####piper pipes in whole database a chunk at a time and
####applies a function to that chunk.
####
piper <- function(file_name, func, row_num, chunk_size, ...){
        
        header <- colnames(read.csv(file_name, header = TRUE, nrows = 2, stringsAsFactors = TRUE))
        con = file(file_name, "r")
        
        #read the first bit outside the loop
        new_df <- read.csv(con, nrow = chunk_size, stringsAsFactors = TRUE, header = TRUE)
        rows_left <- row_num - chunk_size
        
        output <- NULL
        output <- match.fun(func)(output, new_df, ...)
        
        while(rows_left > 0){
                
                #how much to look at next
                bite <- min(chunk_size,rows_left)
                
                #pulling that much into R
                df <- read.csv(con, nrow = bite, stringsAsFactors = TRUE, header = FALSE)
                colnames(df) <- header
                
                #updating the output
                output <- match.fun(func)(output, df, ...)
                
                #how many rows left do we have?
                rows_left <- rows_left - bite
        }
        close(con)
        return(output)
}


#gets all the information in the table on pt_id's patients
#appends it to what we have so far
#doing this without rbind would be ideal.
piper_pt_extract <- function(output, df, pt_id){
        output <- rbind(output, df[df$patient_ID %in% pt_id,])
        return(output)
}

####creates a list for batches of 25 patients
pt_batch <- function(pt_list){
        print("Remember it's index up to (index+1) - 1")
        index <- seq(1, length(pt_list),25)
        index <- c(index, length(pt_list))
        return(index)
}

#note: only works on Sam's computer. Change the directories accordingly.
#goal_dir is the place you want to put all the patient files
pt_scrape <- function(file_name, pt_lt, row_num, chunk_size, goal_dir, ...){
        
        #goal directory
        directory <- getwd()
        
        #create the batches of patients
        index <- pt_batch(pt_lt)
        
        #cycle through an order of an index of patients
        for(i in 1:(length(index)-1)){
                
                #get the directory back
                setwd(directory)
                
                #create a list of current pts.
                current_pts <- pt_lt[index[i]:(index[i+1]-1)]
                new_file_name <- paste("Pts ",index[i]," to ",index[i+1]-1, ".csv",sep ='')
                
                df<- piper(file_name = file_name, func = piper_pt_extract,
                           row_num = row_num, chunk_size = chunk_size,
                           pt_id = current_pts)
                
                #now put it in the right place!
                setwd(goal_dir)
                
                write.csv(df, new_file_name)
        }
}


#Now the same functions with the labs.
#use piper but use lab scrape, etc.

lab_piper_pt_extract <- function(output, df, pt_id){
        output <- rbind(output, df[df$Patient_ID %in% pt_id,])
        return(output)
}

lab_pt_scrape <- function(file_name, pt_lt, row_num, chunk_size, goal_dir, ...){
        
        #goal directory
        directory <- getwd()
        
        #create the batches of patients
        index <- pt_batch(pt_lt)
        
        #cycle through an order of an index of patients
        for(i in 1:(length(index)-1)){
                
                #get the directory back
                setwd(directory)
                
                #create a list of current pts.
                current_pts <- pt_lt[index[i]:(index[i+1]-1)]
                new_file_name <- paste("LABS for Pts ",index[i]," to ",index[i+1]-1, ".csv",sep ='')
                
                df<- piper(file_name = file_name, func = lab_piper_pt_extract,
                           row_num = row_num, chunk_size = chunk_size,
                           pt_id = current_pts)
                
                #now put it in the right place!
                setwd(goal_dir)
                
                write.csv(df, new_file_name)
        }
}


