remove(list=ls())
library(dplyr)
library(tidyr)

F01_call <- TRUE

############################################
# prepares dataframes for winners of 2013-15

winners <- function(){
  
  # function for preparing data frames 2013-2015
  prep_df_2 <- function(df_in){
    
    df_out <- df_in %>% mutate(program_rank = as.numeric(V1),
                               st_ID = as.numeric(V2),
                               st_points = as.numeric(V3),
                               st_preference = as.numeric(V4))
    
    
    # get indeces where Nr occurrs
    index_list <- which(df_out$V1 == "Nr")
    
    #adjust index list so it accounts for the stretch from the last Nr encountered to the last entry
    index_list[length(index_list)+1] <- length(df_out$V1)+3 
    
    # initiate columns
    df_out$program <- ""
    df_out$uni <- ""
    
    # for each stretch defined by the indeces of two sequential Nr, copy the value for the program
    # and for university
    for (i in 1:(length(index_list)-1)) {
      df_out$program[(index_list[i]+1):(index_list[i+1]-3)] <- df_out$V1[index_list[i]-1]
      df_out$uni[(index_list[i]+1):(index_list[i+1]-3)] <- df_out$V1[index_list[i]-2]
    }
    
    df_out <- df_out[!is.na(df_out$st_points),]
    return(df_out)
  }
  
  files_names <- c("faza_I_2013","faza_I_2014","faza_I_2015")
  
  df_list <- list()
  df_winners <- list()
  path_in <- "./Data/Winners/"
  
  for (i in files_names){
    file_to_read <- paste0(path_in,i,".csv")
    
    df_list[[i]] <- read.csv(file_to_read, header= FALSE,
                             stringsAsFactors = FALSE)
    
    df_winners[[i]] <- df_list[[i]] %>% prep_df_2()
    df_winners[[i]][["year"]] <- i
  }
  
  return(df_winners)
}

############################################
# prepares dataframes for coefficients

coeff <- function(){
  
  files_names <- c("coeff_exams","coeff_school")
  path_in <- "./Data/Coefficients/"

  df_list <- list()
  
  for (i in files_names){
    file_to_read <- paste0(path_in,i,".csv")
    df_list[[i]] <- read.csv(file_to_read, header= TRUE,
                                 stringsAsFactors = FALSE)
    names(df_list[[i]]) <- c("uni","faculty","program_code","program_name","exam_index","exam_name","exam_coeff")
    df_list[[i]][["program"]] <- paste("Programi i studimit",df_list[[i]][["program_code"]],":",df_list[[i]][["program_name"]])
  }
  df_list[["coeff_exams"]] <- df_list[["coeff_exams"]] %>% select(one_of(c("uni","faculty","program_code",
                                                                           "program_name","exam_index","exam_name",
                                                                           "exam_coeff","program")))
  return(df_list)
}

############################################
# read data by calling functions

df_read <- list()
df_read[c("faza_I_2013","faza_I_2014","faza_I_2015")] <- winners()
df_read[c("coeff_exams","coeff_school")] <- coeff()
