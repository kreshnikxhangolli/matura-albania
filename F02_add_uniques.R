if (!is.element("F01_call", ls())) source("./F01_import_data.r")

F02_call <- TRUE

###############################################################
# adds program_unique and uni_unique column

add_unique_list <- function(list_in, prog_df_in){
  
  add_unique_df <- function(df_in, df_add_in, prog_field_in){
    
    # select index column, program_unique and uni_unique column
    temp_df <- df_add_in %>% select(one_of(c(prog_field_in,"program_unique","uni_unique")))
    
    df_in <- left_join(df_in, temp_df, by = c("program" = prog_field_in))
    
    return(df_in)
  }
  
  list_out <- list()
  for (i in names(list_in)) {
    list_out[[i]] <- add_unique_df(list_in[[i]], prog_df_in, i)
  }
  
  return(list_out)
  
}

###############################################################
# add unique columns to current d.f.

df_progs <- read.csv("./Data/Programs/programs_unique.csv",stringsAsFactors = FALSE)

df_read <- add_unique_list(df_read,df_progs)

list_through <- c("faza_I_2013","faza_I_2014","faza_I_2015")

df_ready <- list()
df_ready[["results"]] <- df_read[[list_through[1]]]

for (i in 2:length(list_through)){
  df_ready[["results"]] <- union(df_ready[["results"]],df_read[[list_through[i]]])
}

df_ready[["coeff_exams"]] <- df_read[["coeff_exams"]]
df_ready[["coeff_school"]] <- df_read[["coeff_school"]]

housekeeping_list <- c("df_read","list_through","i","df_progs")
remove(list=housekeeping_list)
