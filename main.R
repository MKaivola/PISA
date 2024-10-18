library(tidyr)
library(moments)
library(boot)

# Code for Year 2018
year_code <- "X2018..YR2018."

# Initial columns to extract
cols_to_extract <- c("Country.Name", "Series.Code",  year_code)

# Relevant PISA series code

PISA_code <- "LO.PISA.MAT.0"

read_and_widen_csv <- function(filename) {
  
  df <- read.csv(filename)
  
  df[,year_code] <- as.numeric(df[,year_code])
  
  df <- df[!is.na(df[,year_code]),]
  
  df_codes <- unique(df[c("Series.Name", "Series.Code")])
  
  df <- df[cols_to_extract]
  
  df <- tidyr::pivot_wider(df, id_cols = Country.Name, 
                                   names_from = Series.Code, 
                                   values_from = all_of(year_code))
  return(list(data = df, codes = df_codes))
  
}



PISA_filename <- list.files("data/PISA_results", full.names = T)

PISA_data <- read_and_widen_csv(PISA_filename)

PISA_math_below_1 <- PISA_data$data[c("Country.Name", PISA_code)]

covariate_filenames <- list.files("data/Covariate_csv", full.names = T)

covariate_data <- lapply(covariate_filenames, read_and_widen_csv)

covariate_dfs <- lapply(covariate_data, function(x) x$data)

covariate_codes <- do.call(rbind,lapply(covariate_data, function(x) x$codes))

df <- Reduce(function(x,y) merge(x,y, by="Country.Name", all.x = T), c(list(PISA_math_below_1), covariate_dfs))

rownames(df) <- df$Country.Name

df <- df[!(names(df) %in% c("Country.Name"))]


# Calculate the proportion of missing values for each variable

props_NA_var <- apply(df, 2, function(col) mean(is.na(col)))

sort(props_NA_var)

# Calculate the proportion for each country

props_NA_obs <- apply(df, 1, function(col) mean(is.na(col)))

sort(props_NA_obs)

# Drop variables for which proportion exceeds 10 %

vars_to_drop <- names(props_NA_var[props_NA_var > 0.1])

df_subset <- df[!(names(df) %in% vars_to_drop)]

# Drop obs where there are missing values

obs_to_drop <- !apply(df_subset, 1, function(row) any(is.na(row)))

df_subset <- df_subset[obs_to_drop, ]

### Univariate analysis ###

basic_sum_stats <- c(mean, median, sd, function(x) mad(x, constant = 1.0), moments::skewness)

names(basic_sum_stats) <- c("Mean", "Median", "SD", "MAD", "Skew")

df_sum_stats <- sapply(basic_sum_stats, function(func) apply(df_subset, 2, func))

var_inds <- 1:ncol(df_subset)

names(var_inds) <- colnames(df_subset)

boot_ci_func <- function(data, stat) {
  boot_obj <- boot::boot(data, function(d, ind) apply(data, 2, function(col) stat(col[ind])), 10000, parallel = "snow")
  
  boot_cis <- sapply(var_inds, function(ind) boot::boot.ci(boot_obj, index = ind, type = "basic")$basic[4:5])
  
  return(boot_cis)
}

boot_cis <- lapply(basic_sum_stats, function(stat) boot_ci_func(df_subset, stat))

PISA_sum_cis <- sapply(boot_cis, function(mat) mat[,PISA_code])

PISA_sum_stats <- rbind(df_sum_stats[PISA_code,], PISA_sum_cis)

rownames(PISA_sum_stats) <- c("Est.","Lower", "Upper")

