library(ggplot2)
library(patchwork)
library(xgboost)

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

### Missing value filtering ###

drop_missing <- function(df, 
                         props_NA, 
                         prop_thres,
                         drop_obs = F) {
  
  # Drop variables for which proportion exceeds prop_thres
  vars_to_drop <- names(props_NA[props_NA > prop_thres])
  
  df_subset <- df[!(names(df) %in% vars_to_drop)]
  
  if (!drop_obs) {
    return(df_subset)
  }
  
  # Drop obs where there are missing values
  obs_to_drop <- !apply(df_subset, 1, function(row) any(is.na(row)))
  
  df_subset[obs_to_drop, ]
}

# Use 0.1 as threshold
df_subset <- drop_missing(df, 
                          props_NA_var,
                          0.1,
                          drop_obs=T)

### Univariate analysis ###

basic_sum_stats <- c(mean, median, sd, function(x) mad(x, constant = 1.0), moments::skewness)

names(basic_sum_stats) <- c("Mean", "Median", "SD", "MAD", "Skew")

df_sum_stats <- sapply(basic_sum_stats, function(func) apply(df_subset, 2, func))

var_inds <- 1:ncol(df_subset)

names(var_inds) <- colnames(df_subset)

boot_ci_func <- function(data, stat) {
  boot_obj <- boot::boot(data, function(d, ind) apply(data, 2, function(col) stat(col[ind])), 1000, parallel = "snow")
  
  boot_cis <- sapply(var_inds, function(ind) boot::boot.ci(boot_obj, index = ind, type = "basic")$basic[4:5])
  
  return(boot_cis)
}

boot_cis <- lapply(basic_sum_stats, function(stat) boot_ci_func(df_subset, stat))

PISA_sum_cis <- sapply(boot_cis, function(mat) mat[,PISA_code])

PISA_sum_stats <- rbind(df_sum_stats[PISA_code,], PISA_sum_cis)

rownames(PISA_sum_stats) <- c("Est.","Lower", "Upper")

### Visualization of PISA Series ###

PISA_hist <- ggplot(df_subset, aes(x=LO.PISA.MAT.0)) + 
  geom_histogram(color="black", fill = "white", binwidth = 5)
PISA_hist

### Bivariate Analysis

pearson_cor_mat <- cor(df_subset,method = "pearson")

### Long representation of correlation matrix

cor_wide_to_long <- function(cor_mat) {
  cor_mat[lower.tri(cor_mat)] <- NA
  
  cor_mat <- as.data.frame(cor_mat)
  
  cor_mat$Var_1 <- factor(rownames(cor_mat), levels = rownames(cor_mat))
  
  cor_mat_long <- tidyr::pivot_longer(cor_mat, cols = !Var_1, 
                                           names_to = "Var_2", 
                                           values_to = "Corr", 
                                           values_drop_na = T,
                                           names_transform = function(x) factor(x, levels = rownames(cor_mat)))
  
  return(cor_mat_long)
}

### Heat map constructor

plot_cor_heatmap <- function(cor_mat_long) {
  
  pearson_heatm <- ggplot(cor_mat_long, aes(Var_1, Var_2)) + 
    geom_tile(aes(fill=Corr)) + 
    scale_fill_gradient(low="white", high="steelblue") +
    theme(panel.grid.major = element_blank())
    
  pearson_heatm
}

pearson_cor_long <- cor_wide_to_long(pearson_cor_mat)

plot_cor_heatmap(pearson_cor_long)

### To avoid excessive colinearity, find covariate pairs with over 0.8 correlation

get_colinear_pairs <- function(cor_long, 
                               cor_thres,
                               response) {
  covar_corrs <- cor_long[!(cor_long$Var_1 %in% response | cor_long$Var_2 %in% response),]
  
  covar_corrs <- covar_corrs[covar_corrs$Var_1 != covar_corrs$Var_2,]
  
  covar_high_corrs <- covar_corrs[abs(covar_corrs$Corr) > cor_thres,]
  
  return(covar_high_corrs)
}

covar_high_corrs <- get_colinear_pairs(pearson_cor_long,
                                       0.8,
                                       PISA_code)

### Drop one variable from each pair

df_subset_processed <- df_subset[!(colnames(df_subset) %in% covar_high_corrs$Var_1)]

### Linear Regression ###

df_subset_processed <- as.data.frame(scale(df_subset_processed))

PISA_lm <- lm(LO.PISA.MAT.0 ~ 0 + ., data = df_subset_processed)

### Diagnostics

res_fitted <- data.frame(res = resid(PISA_lm), fitted = fitted(PISA_lm))
res_fitted$Country <- rownames(res_fitted)

res_plot <- ggplot(res_fitted, aes(x = fitted, y = res)) +
  geom_point(color = "steelblue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data = subset(res_fitted, abs(res) >= 0.75), aes(label = Country), size = 2.5, nudge_y = -0.05) +
  xlab("Fitted value") + 
  ylab("Residual")

res_plot

### Estimate CI for R^2

fit_and_extract_R2 <- function(data) {
  model <- lm(LO.PISA.MAT.0 ~ ., data = data)
  
  model_sum <- summary(model)
  
  return(model_sum$r.squared)
  
}

boot_r2 <- boot::boot(df_subset_processed, function(d, i) fit_and_extract_R2(d[i,]), 1000)

r2_CI <- boot::boot.ci(boot_r2)

### Estimate CI:s for covariates

fit_and_extract_coeff <- function(data) {
  model <- lm(LO.PISA.MAT.0 ~ 0 + ., data = data)
  
  return(coef(model))
}

boot_coeff <- boot::boot(df_subset_processed, function(d,i) fit_and_extract_coeff(d[i,]), 1000)

param_inds <- 1:length(coef(PISA_lm))

names(param_inds) <- names(coef(PISA_lm))

extract_est_and_CI <- function(boot_obj, index) {
  boot_ci <- boot::boot.ci(boot_obj, index = index, type = "basic")
  
  est <- boot_ci$t0
  CI <- boot_ci$basic[4:5]
  
  data <- c(est, CI)
  
  names(data) <- c("Est.", "Lower", "Upper")
  
  return(data)
  
}

coeff_CI <- as.data.frame(t(sapply(param_inds, function(ind) extract_est_and_CI(boot_coeff, ind))))

coeff_CI$Var <- rownames(coeff_CI)

### Plot the estimates and the confidence intervals

lm_coeff_est_plot <- ggplot(coeff_CI, aes(x = Var, y = Est.)) + 
  geom_point(color = "steelblue", size = 2) + 
  geom_linerange(aes(ymax = Upper, ymin = Lower)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-1,1)

lm_coeff_est_plot

### Perform permutation tests for covariates

PISA_lm_perm <- summary(lmPerm::lmp(LO.PISA.MAT.0 ~ 0 + ., data = df_subset_processed))

PISA_lm_perm

### Variable Selection using LASSO ###

X <- as.matrix(df_subset_processed[,!(colnames(df_subset_processed) %in% PISA_code)])
y <- df_subset_processed$LO.PISA.MAT.0

lm_LASSO_cv <- glmnet::cv.glmnet(X,y, alpha = 1, standardize = F, intercept = F)

### Plot CV deviance as a function of lambda

df_lambda_cv <- data.frame(lambda = lm_LASSO_cv$lambda, 
                           mean_cv_err = lm_LASSO_cv$cvm, 
                           cv_err_up = lm_LASSO_cv$cvup,
                           cv_err_low = lm_LASSO_cv$cvlo)

LASSO_cv_plot <- ggplot(df_lambda_cv, aes(x = lambda, y = mean_cv_err)) + 
  geom_point(color = "steelblue") + 
  geom_linerange(aes(ymax = cv_err_up, ymin = cv_err_low)) + 
  scale_x_log10() + 
  geom_vline(aes(xintercept = lm_LASSO_cv$lambda.1se, color = "SE"), linetype = "dashed") +
  geom_vline(aes(xintercept = lm_LASSO_cv$lambda.min, color = "Min"), linetype = "dashed") +
  scale_color_manual(name = "Optima", values = c(SE = "red", Min = "black")) +
  xlab(expression(log(lambda))) +
  ylab("Mean MSE")

LASSO_cv_plot

### Determine which variables remain in 1se optimal fit

lambda_opt_ind <- lm_LASSO_cv$index[2]

lambda_opt_coef <- lm_LASSO_cv$glmnet.fit$beta[, lambda_opt_ind]

lambda_opt_covars <- names(lambda_opt_coef[lambda_opt_coef != 0])

df_optimal <- df_subset_processed[, colnames(df_subset_processed) %in% c(PISA_code, lambda_opt_covars)]

PISA_lm_LASSO_opt <- lm(LO.PISA.MAT.0 ~ 0 + ., data = df_optimal)

summary(PISA_lm)
summary(PISA_lm_LASSO_opt)


### Partial Dependence Plot utilities
plot_1d_pdps <- function(model, features, train_data, model_name) {
  
  pdp_data <- lapply(features,
                     function(feat) c(name = feat, data = list(pdp::partial(model, 
                                                                            pred.var = feat,
                                                                            train = train_data))))
  
  pdp_data <- lapply(pdp_data, function(l) {names(l$data)[1] <- "x";l})
  
  pdp_plot_fun <- function(var_data) {
    ggplot(as.data.frame(var_data$data), aes(x = x, y = yhat)) + 
      geom_line() +
      ggtitle(var_data$name)
  }
  
  y_values <- sapply(pdp_data, function(l) l$data$yhat)
  
  y_max <- max(y_values)
  y_min <- min(y_values)
  
  pdp_plots <- lapply(pdp_data, pdp_plot_fun)
  
  Reduce('+', pdp_plots) + plot_annotation(title = paste0("1-D PDP for ", model_name)) & 
    ylim(y_min, y_max)
  
}

### Generalized Additive Models: Check for non-linear structure

PISA_GAM <- mgcv::gam(LO.PISA.MAT.0 ~ s(SH.XPD.OOPC.PP.CD) +
                        s(NY.GNP.PCAP.PP.CD) +
                        s(SP.DYN.TFRT.IN) + 
                        s(HD.HCI.LAYS.MA) + 
                        s(VA.PER.RNK) + 
                        s(GE.PER.RNK),
                      data = df_subset_processed,
                      select = T)

mgcv::gam.check(PISA_GAM)

plot(PISA_GAM, pages = 1, seWithMean = T)

plot_1d_pdps(PISA_GAM, colnames(X), df_subset_processed, "GAM")

### Random Forest Regression Model ###

PISA_rf <- randomForest::randomForest(LO.PISA.MAT.0 ~ 0 + .,
                                      data = df_subset_processed, 
                                      importance = T,
                                      proximity = T)

rf_mse_df <- data.frame(n_trees = 1:PISA_rf$ntree, mse = PISA_rf$mse)

rf_mse_plot <- ggplot(rf_mse_df, aes(x = n_trees, y = sqrt(mse))) +
  geom_line() +
  xlab("Number of trees") +
  ylab("OOB RMSE") +
  ggtitle("Random Forest")

PISA_rf_imp <- as.data.frame(randomForest::importance(PISA_rf))

PISA_rf_imp$Var <- factor(rownames(PISA_rf_imp))

colnames(PISA_rf_imp)[1] <- "OOB_MSE"

OOB_imp_plot <- ggplot(PISA_rf_imp, aes(x = reorder(Var, -OOB_MSE), y = OOB_MSE)) + 
  geom_point() +
  xlab("Predictor") + 
  ylab("Out-of-bag MSE") + 
  scale_x_discrete(guide = guide_axis(angle = 40))

impurity_imp_plot <- ggplot(PISA_rf_imp, aes(x = reorder(Var, -IncNodePurity), y = IncNodePurity)) + 
  geom_point() +
  xlab("Predictor") + 
  ylab("Impurity decrease (MSE)") + 
  scale_x_discrete(guide = guide_axis(angle = 40))

OOB_imp_plot + impurity_imp_plot

rf_feat_imp_ord <- rownames(PISA_rf_imp)[order(PISA_rf_imp$OOB_MSE, decreasing = T)]

plot_1d_pdps(PISA_rf, rf_feat_imp_ord, df_subset_processed, "Random Forest")

rf_cv_vars <- replicate(10,randomForest::rfcv(X, y, scale = F, step = -1,
                                 mtry = function(p) max(1, floor(p/3))),
                        simplify = F)

rf_cv_vars_mse <- sapply(rf_cv_vars, '[[', 'error.cv')

rf_cv_df <- data.frame(n_vars = rf_cv_vars[[1]]$n.var, 
                       mean_mse = apply(rf_cv_vars_mse, 1, mean),
                       se_mse = apply(rf_cv_vars_mse, 1, sd))

ggplot(rf_cv_df, aes(x = n_vars, y = mean_mse)) + 
  geom_line() + 
  geom_linerange(aes(ymin = mean_mse - se_mse, ymax = mean_mse + se_mse)) + 
  scale_x_reverse() + 
  xlab("Number of predictors") + 
  ylab("CV MSE")

### Gradient Boosting using xgboost ###

plot_gbm_learn_curve <- function(eval_log) {
  ggplot(eval_log, aes(x = iter, y = test_rmse_mean)) + 
    geom_line() +
    xlab("Number of iterations") + 
    ylab("Test set RMSE") + 
    ggtitle("Gradient Boosted Trees")
}

Xy_DMatrix <- xgb.DMatrix(X, label = y)

boost_params <- list(nthread = 2,
                     eta = 0.05,
                     subsample = 0.75,
                     objective = "reg:squarederror")

PISA_gbm <- xgb.train(params = boost_params,
                      data = Xy_DMatrix,
                      nrounds = 200,
                      watchlist = list(train = Xy_DMatrix),
                      print_every_n = 10)

gbm_feat_imp <- xgb.importance(model = PISA_gbm)

xgb.ggplot.importance(gbm_feat_imp)

PISA_gbm_cv <- xgb.cv(params = boost_params,
                      data = Xy_DMatrix,
                      nrounds = 200,
                      print_every_n = 10,
                      nfold = 10)

PISA_gbm_cv_log <- PISA_gbm_cv$evaluation_log

PISA_gbm_cv_mse <-plot_gbm_learn_curve(PISA_gbm_cv_log)

xgb.ggplot.shap.summary(data = X,
                        model = PISA_gbm) +
  ylab("SHAP value") + 
  xlab("Feature") + 
  labs(color = "Value")

plot_1d_pdps(PISA_gbm, gbm_feat_imp$Feature, X, "GB Trees")

# CV evaluation for non-ensemble models

mean_rep_cv_score <- function(cv_obj) {
  mean(sapply(cv_obj, '[[', 'CV crit'))
}

PISA_lm_cv <- cv::cv(model=PISA_lm_LASSO_opt,
                     criterion = cv::rmse,
                     k = 10,
                     reps = 5)

PISA_lm_cv_mean <- mean_rep_cv_score(PISA_lm_cv)

GAM_cv <- cv::cv(model=PISA_GAM,
                 criterion = cv::rmse,
                 k = 10,
                 rep = 5)

GAM_cv_mean <- mean_rep_cv_score(GAM_cv)

rf_mse_plot + PISA_gbm_cv_mse + plot_layout(guides = 'collect') & 
  ylim(0,1.2) & 
  geom_hline(aes(yintercept = PISA_lm_cv_mean, color = "lm"), linetype = 'dashed') &
  geom_hline(aes(yintercept = GAM_cv_mean, color = "GAM"), linetype = 'dashed') &
  scale_color_manual(name = "CV RMSE", values = c(lm = "red", GAM = "black"))


### Imputation and other missing value considerations ###

# Take all features where missing value proportion is at most 0.4

df_with_missing  <- drop_missing(df,
                                props_NA_var,
                                0.4,
                                drop_obs=F)

cor_mat_missing <- cor(df_with_missing,
                       use = "complete.obs")

cor_mat_missing_long <- cor_wide_to_long(cor_mat_missing)

plot_cor_heatmap(cor_mat_missing_long)

high_cor_pairs_mis <- get_colinear_pairs(cor_mat_missing_long,
                                         0.9,
                                         PISA_code)

df_miss_subset <- df_with_missing[!(colnames(df_with_missing) %in% high_cor_pairs_mis$Var_1)]

Xy_miss_subset_proc <- scale(df_miss_subset)

X_miss <- Xy_miss_subset_proc[,!(colnames(Xy_miss_subset_proc) %in% PISA_code)]
y_miss <- Xy_miss_subset_proc[, PISA_code]

Xy_miss_Dmat <- xgb.DMatrix(X_miss, label = y_miss)

PISA_miss_gbm <- xgb.train(params = boost_params,
                      data = Xy_miss_Dmat,
                      nrounds = 200,
                      watchlist = list(train = Xy_miss_Dmat),
                      print_every_n = 10)

gbm_miss_feat_imp <- xgb.importance(model = PISA_miss_gbm)

xgb.ggplot.importance(gbm_miss_feat_imp)

PISA_miss_gbm_cv <- xgb.cv(params = boost_params,
                      data = Xy_miss_Dmat,
                      nrounds = 200,
                      print_every_n = 10,
                      nfold = 10)

PISA_miss_gbm_cv_log <- PISA_miss_gbm_cv$evaluation_log

plot_gbm_learn_curve(PISA_gbm_cv_log)

plot_1d_pdps(PISA_miss_gbm, gbm_miss_feat_imp$Feature, X_miss, "GB Trees")

# No improvement compared to smaller model when using default missing value strategy

Xy_imputed <- randomForest::rfImpute(LO.PISA.MAT.0 ~ 0 + .,
                                     data=df_miss_subset)

Xy_imputed_proc <- scale(Xy_imputed)

X_imputed <- Xy_imputed_proc[, !(colnames(Xy_imputed_proc) %in% PISA_code)]

Xy_imp_Dmat <- xgb.DMatrix(X_imputed, label = y_miss)

PISA_imp_gbm <- xgb.train(params = boost_params,
                           data = Xy_imp_Dmat,
                           nrounds = 200,
                           watchlist = list(train = Xy_imp_Dmat),
                           print_every_n = 10)

PISA_imp_gbm_cv <- xgb.cv(params = boost_params,
                          data = Xy_imp_Dmat,
                          nrounds = 200,
                          print_every_n = 10,
                          nfold = 10)
plot_gbm_learn_curve(PISA_imp_gbm_cv$evaluation_log)

plot_1d_pdps(PISA_imp_gbm, gbm_miss_feat_imp$Feature, X_imputed, "GB Trees")

# Also no improvement when using Random Forest proximity imputation