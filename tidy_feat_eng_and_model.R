library(readr)
library(tidymodels)
library(vip)
library(pdp)
library(patchwork)

data <- read_rds("data/regression/df.rds")

set.seed(123)

# Split and check continent distribution for interest

data_init_split <- initial_split(data, prop = 3 / 4)

data_train <- training(data_init_split)
data_test <- testing(data_init_split)

extract_continent_dist <- function(data) {
  data |>
    count(continent) |>
    mutate(prop = n / sum(n)) |>
    select(!n)
}

map(list(data_train, data_test), extract_continent_dist) |>
  reduce(function(x, y)
    left_join(
      x,
      y,
      by = join_by(continent),
      suffix = c(".train", ".test")
    )) |>
  pivot_longer(
    !continent,
    names_to = "set",
    names_pattern = "prop.(.*)",
    values_to = "prop"
  ) |>
  ggplot(aes(x = continent, y = prop, fill = set)) +
  geom_col(position = "dodge")

# Evaluation using 10-fold CV using a couple of different metrics

data_train_cv_split <- vfold_cv(data_train, v = 10, repeats = 2)

eval_metrics <- metric_set(rmse, mae, rsq)

# Quick fit using a random forest model

rf_model <- rand_forest() |>
  set_mode("regression") |>
  set_engine("randomForest", importance = T)

rf_recipe <- recipe(MAT.0.Both ~ . , data = data_train) |>
  update_role(id, new_role = "ID") |>
  step_impute_median(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors())

rf_workflow <- workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_model)

rf_cv_fit <- rf_workflow |>
  fit_resamples(data_train_cv_split, metrics = eval_metrics)

collect_metrics(rf_cv_fit)

rf_fit <- rf_workflow |>
  fit(data_train)

# Mean decrease in impurity scores
vip(vi_model(rf_fit, type = 2))

rf_fit |>
  augment(data_test) |>
  eval_metrics(truth = MAT.0.Both, estimate = .pred)

# More extensive tuning with a gradient boosted model

gbm_model <- boost_tree(trees = 500,
                        learn_rate = 0.05,
                        sample_size = 0.75) |>
  set_mode("regression") |>
  set_engine(
    "xgboost",
    objective = "reg:squarederror",
    alpha = tune(),
    lambda = tune()
  )

gbm_recipe <- recipe(MAT.0.Both ~ . , data = data_train) |>
  update_role(id, new_role = "ID") |>
  step_impute_median(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors()) |>
  step_dummy(all_factor_predictors(), one_hot = T)

gbm_workflow <- workflow() |>
  add_recipe(gbm_recipe) |>
  add_model(gbm_model)

gbm_param_grid <- grid_regular(list(alpha = penalty_L1(), lambda = penalty_L2()), levels = 3)

# Tune gbm model using 10-fold CV and a grid search and evaluate on test set

gbm_tuning_results <- gbm_workflow |>
  tune_grid(data_train_cv_split, grid = gbm_param_grid)

collect_metrics(gbm_tuning_results) |>
  mutate(alpha = factor(signif(alpha, 2))) |>
  ggplot(aes(x = lambda, y = mean, color = alpha)) +
  scale_x_log10() +
  geom_line() +
  facet_wrap(vars(.metric), scales = "free")

show_best(gbm_tuning_results, metric = "rmse", n = 1)
show_best(gbm_tuning_results, metric = "rsq", n = 1)

# Fit the optimal model to training data and evaluate on test
gbm_final_workflow <- gbm_workflow |>
  finalize_workflow(select_best(gbm_tuning_results, metric = "rmse"))

gbm_final_fit <- gbm_final_workflow |>
  last_fit(data_init_split)

collect_metrics(gbm_final_fit)

# Assess feature importance and dependencies
gbm_fitted_xgboost <- extract_fit_engine(gbm_final_fit)

gbm_feat_imps <- vi_model(gbm_fitted_xgboost, type = "gain")

vip(gbm_feat_imps)

### Partial Dependence Plot utilities
plot_1d_pdps <- function(model, features, train_data, model_name) {
  pdp_data <- lapply(features, function(feat)
    c(name = feat, data = list(
      pdp::partial(model, pred.var = feat, train = train_data)
    )))
  
  pdp_data <- lapply(pdp_data, function(l) {
    names(l$data)[1] <- "x"
    l
  })
  
  pdp_plot_fun <- function(var_data) {
    ggplot(as.data.frame(var_data$data), aes(x = x, y = yhat)) +
      geom_line() +
      ggtitle(var_data$name)
  }
  
  y_values <- sapply(pdp_data, function(l)
    l$data$yhat)
  
  y_max <- max(sapply(y_values, function(y)
    max(y)))
  y_min <- min(sapply(y_values, function(y)
    min(y)))
  
  pdp_plots <- lapply(pdp_data, pdp_plot_fun)
  
  Reduce('+', pdp_plots) + plot_annotation(title = paste0("1-D PDP for ", model_name)) &
    ylim(y_min, y_max)
  
}

fitted_recipe <- extract_recipe(gbm_final_fit)

data_train_baked <- bake(fitted_recipe, data_train) |>
  select(all_of(gbm_fitted_xgboost$feature_names))

# Top 5 most important variables univariate PDP plots
plot_1d_pdps(gbm_fitted_xgboost,
             gbm_feat_imps$Variable[1:5],
             data_train_baked,
             "GB Trees")
