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

# Modelling using parsnip and recipes

gbm_model <- boost_tree(trees = 200) |>
  set_mode("regression") |>
  set_engine("xgboost", objective = "reg:squarederror")

gbm_recipe <- recipe(MAT.0.Both ~ . , data = data_train) |>
  update_role(id, new_role = "ID") |>
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_factor_predictors(), one_hot = T)

gbm_workflow <- workflow() |>
  add_recipe(gbm_recipe) |>
  add_model(gbm_model)

eval_metrics <- metric_set(rmse, mae, rsq)

# Evaluate gbm model using 10-Fold CV

data_train_cv_split <- vfold_cv(data_train,
                                v = 10,
                                repeats = 5)

gbm_cv_fit <- gbm_workflow |> 
  fit_resamples(data_train_cv_split,
                metrics = eval_metrics)

collect_metrics(gbm_cv_fit)

# Evaluate metrics on test set

gbm_fit <- gbm_workflow |>
  fit(data_train)

gbm_fit |> 
  augment(data_test) |> 
  eval_metrics(truth = MAT.0.Both, estimate = .pred)


