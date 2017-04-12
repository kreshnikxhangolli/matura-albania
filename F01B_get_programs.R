# sole purpose of this script is to get unique values for programs and unis and
# save them to a csv file. The csv file is later manipulated by hand to get a unique
# name for programs offered during the years. This is necessary since the same program
# apperas under a different name or it has been offered only in restricted years

if (!is.element("F01_call", ls())) source("./F01_import_data.r")

#######################################
# Get programs from results files

get_programs_df <- function(df_in){
  
  progs_list <- list()
  
  length_progs <- -1

  for (i in names(df_in)){
    progs_list[[i]] <- df_in[[i]]$program %>% unique()
    length_progs <- c(length_progs,length(progs_list[[i]]))
  }

  max_length_progs <- max(length_progs)
  
  # adjust elements of list to the same length by adding empy space 
  # and converts to dataframe
  
  convert_to_df <- function(list_in, max_length_in){
    
    for (i in names(list_in)) {
      if (max_length_in > length(list_in[[i]])) {
        list_in[[i]][(length(list_in[[i]])+1):max_length_in] <- "" 
      }
    }  
    
    list_in <- as.data.frame(list_in)
    return(list_in)
  }
  
  df_programs_out <- convert_to_df(progs_list,max_length_progs)
  
  return(df_programs_out)  
}

############################################
# read data by calling functions

df_programs <- get_programs_df(df_read)
write.csv(df_programs,"./Data/Programs/programs.csv")
