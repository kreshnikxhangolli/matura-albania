if (!is.element("F02_call", ls())) source("./F02_add_uniques.r")
library(ggplot2)

F03_call <- TRUE

#################################################################
# function for generating density graphs

gen_density_graphs <- function(df_in){
  unique_years <- df_in[["year"]] %>% unique()
  unique_years <- sort(unique_years)
  
  list_out <- list()
  list_out[["the_plots"]] <- list()
  list_out[["length_density"]] <- list()
  
  for (i in unique_years) {
     temp_df <- df_in[df_in[["year"]]==i,]
     if (length(temp_df[["st_points"]]) > 1) {
          density_calc <- temp_df[["st_points"]] %>% density()
          temp_dens_df <- density_calc %>% with(data.frame(x,y))
          list_out[["the_plots"]][[i]] <- temp_dens_df %>% ggplot(aes(x = x, y=y)) + geom_line() +
                theme_classic() + labs(title = i, x = "Score", y = "Percent")
          list_out[["length_density"]][[i]] <- length(temp_df[["st_points"]])
     } else{
       list_out[[i]] <- temp_df %>% ggplot(aes(st_points)) + geom_density() +
         theme_classic() + labs(title = i, x = "Score", y = "Percent")
       list_out[["length_density"]][[i]] <- 1
     }
  }
  
  return(list_out)
}

#### Note: it is important to get uniques for either coeff_exams or
# coeff_school d.f. since results may be available even for programs that are not
# provided in the current year, therefore no coefficients available

uni_list <- df_ready[["coeff_exams"]][["uni_unique"]] %>% unique()

programs_list <- list()

for (i in uni_list){
  temp_uni <- df_ready[["coeff_exams"]] %>% filter(uni_unique == i)
  programs_list[[i]] <- temp_uni[["program_unique"]]  %>% unique()
}

container <- list()

for (i in names(programs_list)) {
  
  container[[i]] <- list()
  
  for (j in programs_list[[i]]){
    
    container[[i]][[j]] <- list()
    container[[i]][[j]][["scores"]] <- df_ready[["results"]] %>% filter(program_unique == j) %>% select(one_of(c("st_points","year")))
    container[[i]][[j]][["coeff"]] <- list()
    container[[i]][[j]][["coeff"]][["exams"]] <- df_ready[["coeff_exams"]] %>% filter(program_unique == j) %>% select(one_of(c("exam_name","exam_coeff")))
    container[[i]][[j]][["coeff"]][["school"]] <- df_ready[["coeff_school"]] %>% filter(program_unique == j) %>% select(one_of(c("exam_name","exam_coeff")))
    container[[i]][[j]][["graph"]] <- container[[i]][[j]][["scores"]] %>% gen_density_graphs()
  }
}

saveRDS(container,"./RDS/container.rds")
saveRDS(programs_list,"./RDS/programs_list.rds")
