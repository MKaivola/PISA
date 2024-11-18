library(readr)
library(tidymodels)

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
  pivot_longer(!continent,
               names_to = "set",
               names_pattern = "prop.(.*)",
               values_to = "prop") |> 
  ggplot(aes(x = continent, y = prop, fill = set)) +
  geom_col(position = "dodge")

# Evaluation using 10-fold CV using a couple of different metrics

data_train_cv_split <- vfold_cv(data_train,
                                v = 10,
                                repeats = 5)

eval_metrics <- metric_set(rmse, mae, rsq)

# Quick fit using a random forest model

rf_model <- rand_forest() |> 
  set_mode("regression") |> 
  set_engine("randomForest")

rf_recipe <- recipe(MAT.0.Both ~ . , data = data_train) |> 
  update_role(id, new_role = "ID") |> 
  step_impute_median(all_numeric_predictors())

rf_workflow <- workflow() |> 
  add_recipe(rf_recipe) |> 
  add_model(rf_model)

rf_cv_fit <- rf_workflow |> 
  fit_resamples(data_train_cv_split,
                metrics = eval_metrics)

collect_metrics(rf_cv_fit)

rf_fit <- rf_workflow |> 
  fit(data_train)

rf_fit |> 
  augment(data_test) |> 
  eval_metrics(truth = MAT.0.Both, estimate = .pred)

# More extensive tuning with a gradient boosted model

gbm_model <- boost_tree(trees = 200,
                        learn_rate = 0.05,
                        sample_size = 0.75) |>
  set_mode("regression") |>
  set_engine("xgboost", objective = "reg:squarederror")

gbm_recipe <- recipe(MAT.0.Both ~ . , data = data_train) |>
  update_role(id, new_role = "ID") |>
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_factor_predictors(), one_hot = T)

gbm_workflow <- workflow() |>
  add_recipe(gbm_recipe) |>
  add_model(gbm_model)

# Evaluate gbm model using 10-Fold CV and test set

gbm_cv_fit <- gbm_workflow |> 
  fit_resamples(data_train_cv_split,
                metrics = eval_metrics)

collect_metrics(gbm_cv_fit)

gbm_fit <- gbm_workflow |>
  fit(data_train)

gbm_fit |> 
  augment(data_test) |> 
  eval_metrics(truth = MAT.0.Both, estimate = .pred)


