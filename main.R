library(tidyr)

# Code for Year 2018
year_code <- "X2018..YR2018."

# Initial columns to extract
cols_to_extract <- c("Country.Name", "Series.Name", year_code)

read_and_widen_csv <- function(filename) {
  
  develop_df <- read.csv(filename)
  
  develop_df <- develop_df[cols_to_extract]
  
  develop_df[,year_code] <- as.numeric(develop_df[,year_code])
  
  develop_df <- develop_df[!is.na(develop_df[,year_code]),]
  
  develop_df <- tidyr::pivot_wider(develop_df, id_cols = Country.Name, 
                                   names_from = Series.Name, 
                                   values_from = year_code)
  
  return(develop_df)
  
}

csv_filenames <- list.files("data", full.names = T)

df_list <- lapply(csv_filenames, read_and_widen_csv)

df <- Reduce(function(x,y) merge(x,y, by="Country.Name"), df_list)


