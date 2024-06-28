library(tidymodels)

# https://juliasilge.com/blog/lasso-the-office/

lgbtq_movies_sel <- lgbtq_movies %>% 
  mutate(
    adult = as.integer(adult),
    video = as.integer(video),
    year = year(release_date)
  ) %>% 
  filter(!is.na(year)) %>% 
  mutate(english = if_else(original_language == "en", 1, 0)) %>% 
  select(id, year, popularity, vote_average, vote_count, adult, video, english)

lgbtq_movies_split <- initial_split(lgbtq_movies_sel)
lgbtq_movies_train <- training(lgbtq_movies_split)
lgbtq_movies_split_test <- testing(lgbtq_movies_split)

lgbtq_movies_rec <- recipe(vote_average ~ ., data = lgbtq_movies_train) %>%
  update_role(id, new_role = "ID") %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

lgbtq_movies_prep <- lgbtq_movies_rec %>%
  prep(strings_as_factors = FALSE)

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(lgbtq_movies_rec)

lasso_fit <- wf %>%
  add_model(lasso_spec) %>%
  fit(data = lgbtq_movies_train)

lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

set.seed(1234)

lgbtq_movies_boot <- bootstraps(lgbtq_movies_train)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()

set.seed(2020)

lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = lgbtq_movies_boot,
  grid = lambda_grid
)
  
lasso_grid %>%
  collect_metrics()

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- lasso_grid %>%
  select_best()

final_lasso <- finalize_workflow(
  wf %>% add_model(tune_spec),
  lowest_rmse
)

library(vip)

final_lasso %>%
  fit(lgbtq_movies_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)
