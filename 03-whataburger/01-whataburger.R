
# setup ----
library(tidyverse)
library(tonythemes)
library(yelp) # richierocks/yelp
tonythemes::theme_set_tony()

# have to go yelp and create a token before doing this
yelp::store_access_token(
  'DW4rR-HA2A4cRL20zxefH7lADdRb7tQmimexUyW4nB65lPtYpsOcWtB7XqhxPMi3h8yzTAq-uTbRML1KosVqnPqEaHmjloZeG-O2KAvynhqe7mvzESvZP4-ge_gOYXYx'
)

dir_proj <- '03-whataburger'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data)
# manual copy-pasta from https://locations.whataburger.com/tx.html
locs <- file.path(dir_proj, 'whataburger-locations.csv') %>% read_csv()

# scrape ----
# quick inspection of functions
# yelp::business_search('Whataburger', location = 'Alvarado, Texas', limit = 50) -> x
# yelp::business_match(
#   'Whataburger',
#   city = 'Alvarado',
#   state = 'TX',
#   country = 'US',
#   '',
#   match_threshold = 'none'
# ) -> y
# y

.print <- function(...) {
  cat(glue::glue(...), sep = '\n')
}

f_safe <- possibly(yelp::business_match, otherwise = tibble(), quiet = FALSE)
do_scrape_match <- function(.term, .city) {
  path <- file.path(dir_data, sprintf('business_match-%s-%s.rds', str_replace_all(.term, '\\s+', '_'), str_replace_all(.city, '\\s+', '_')))
  # You don't actually need what I've commented out.. the printing stuff is just ancillary.
  # prefix <- glue::glue('{Sys.time()}:')
  # suffix <- glue::glue('for {.term}, {.city}.')
  # ..print <- function(x) {
  #   .print(glue::glue('{prefix} {x} {suffix}'))
  # }
  if(file.exists(path)) {
    # ..print('Returning early')
    return(read_rds(path))
  }
  
  res <- yelp::business_match(
    .term,
    city = .city,
    state = 'TX',
    country = 'US',
    '',
    match_threshold = 'none'
  )
  # ..print('Retrieved data')
  write_rds(res, path)
  res
}

do_scrape_match_safely <- possibly(do_scrape_match, otherwise = tibble(), quiet = FALSE)
matches <-
  crossing(
    term = c('Whataburger', 'McDonald\'s'),
    location = locs$location
  ) %>% 
  mutate(data = map2(term, location, do_scrape_match_safely))
matches

df_match <- matches %>% select(term, location, data) %>% unnest(data)
df_match

do_scrape_info <- function(.business_id) {
  path <- file.path(dir_data, sprintf('business_lookup-%s.rds', .business_id))
  prefix <- glue::glue('{Sys.time()}:')
  suffix <- glue::glue('for {.business_id}.')
  ..print <- function(x) {
    .print(glue::glue('{prefix} {x} {suffix}'))
  }
  if(file.exists(path)) {
    ..print('Returning early')
    return(read_rds(path))
  }
  
  res <- yelp::business_lookup(.business_id)
  ..print('Retrieved data')
  write_rds(res, path)
  res
}
do_scrape_info_safely <- possibly(do_scrape_info, otherwise = tibble(), quiet = FALSE)

df <- 
  df_match %>% 
  distinct(term, location, business_id) %>% 
  mutate(data = map(business_id, do_scrape_info_safely))
df

df %>% select(term, location, data) %>% unnest(data)
df_slim <-
  df %>% 
  select(term, location, data) %>% 
  unnest(data) %>% 
  select(term, location, name, business_id, lat = latitude, lon = longitude, rating, review_count) %>% 
  mutate(
    across(rating, list(num = ~as.numeric(as.character(.x))))
  ) %>% 
  filter(term == name)
df_slim

# ebbr ----
# Reference: http://varianceexplained.org/r/empirical_bayes_baseball/
# library(ebbr)
# prior <-
#   df_slim %>% 
#   select(term, location, rating_num, review_count) %>% 
#   filter(review_count > 10L) %>% 
#   mutate(across(rating_num, ~round(.x, 0) %>% as.integer()), total = 5L) %>% 
#   # head(3) %>% 
#   # group_by(location) %>% 
#   # uncount(review_count) %>% 
#   # ungroup() %>% 
#   ebb_fit_prior(rating_num, total)
# prior
# prior %>% augment(data = df_slim %>% mutate(total = 5L))

# Calculated these by guess and check.
alpha <- 5
beta <- alpha * 2 / 3
df_slim_adj <-
  df_slim %>% 
  # select(term, location, business_id, rating_num, review_count) %>% 
  mutate(
    rating_num_adj = 5 * (review_count * rating_num + !!alpha) / (review_count * 5 + !!alpha + !!beta),
    rating_num_diff = rating_num_adj - rating_num,
    across(term, factor)
  )
df_slim_adj %>% arrange(desc(abs(rating_num_diff)))
df_slim_adj %>% arrange(rating_num_diff)
df_slim_adj

df_slim_adj %>% 
  ggplot() +
  aes(x = rating_num, y = rating_num_adj) +
  geom_point(
    data = . %>% filter(review_count < 10L),
    color = 'red',
    show.legend = FALSE,
    aes(size = -review_count),
    alpha = 0.2
  ) +
  geom_point(
    data = . %>% filter(review_count >= 10L),
    alpha = 0.25, 
    show.legend = FALSE
  ) +
  theme(
    plot.caption = ggtext::element_markdown(size = 9),
  ) +
  labs(
    title = 'Empirical Bayes adjustment to Yelp Rating',
    caption = glue::glue('Adjustment (beta distribution): alpha = {alpha}, beta = {round(beta, 1)}<br/>Locations with less than 10 reviews played in red (stronger adjustment).'),
    x = 'Original Rating',
    y = 'Adjusted Rating'
  ) -> p_drob
ggsave(
  filename = file.path(dir_proj, 'emp_bayes.png'),
  p_drob,
  width = 6,
  height = 6,
  type = 'cairo'
)

# eda ----
# df_slim %>% filter(location == 'Alvarado')
# df_slim %>% count(name, rating) %>% ggplot() + aes(x = rating, y = n) + geom_col() + facet_wrap(~name)
# df_slim %>% count(name, sort = TRUE)
# df_slim %>% skimr::skim()
tx <- map_data('state', region = 'texas')
tx

# df_slim %>% skimr::skim(review_count)

pal <- c(
  'McDonald\'s' = 'black', #DA291C',
  # 'McDonald\'s' = '#FFC72C',
  'Whataburger' = '#ff770f'
)

df_slim_adj %>% 
  arrange(desc(rating_num)) %>% 
  ggplot() +
  aes(x = lon, y = lat) +
  geom_polygon(inherit.aes = FALSE, data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  geom_point(aes(color = term, size = rating_num, alpha = review_count)) +
  guides(size = 'none', alpha = 'none', color = guide_legend(title = '', override.aes = list(size = 3))) +
  scale_color_manual(
    values = pal
  ) +
  theme(
    legend.position = 'top',
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  ) +
  coord_map() +
  labs(
    title = 'Whataburger vs. McDonald\'s, per Yelp',
    x = NULL, y = NULL
  )

df_slim %>% 
  filter(term == 'Whataburger') %>% 
  ggplot() +
  aes(x = lon, y = lat, z = rating_num) +
  geom_polygon(inherit.aes = FALSE, data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  stat_summary_hex() +
  facet_wrap(~term)

# matching model ----
# Reference: https://github.com/tonyelhabr/bdb2021
# bi is for binomial
library(tidymodels)
set.seed(42)
split_bi <- df_slim_adj %>% initial_split(strata = term)
df_trn_bi <- split_bi %>% training()
df_tst_bi <- split_bi %>% testing()
folds_bi <- df_trn_bi %>% vfold_cv(strata = term, v = 5)
folds_bi

ctrl <- control_grid(verbose = TRUE)
metset_bi <- metric_set(accuracy)

rec_bi <-
  df_slim_adj %>% 
  recipe(formula(term ~ lon + lat), data = .) %>% 
  step_interact(terms = ~lon:lat) %>% 
  step_ns(lon, deg_free = tune('deg_free_lon')) %>% 
  step_ns(lat, deg_free = tune('deg_free_lat'))
rec_bi

wf_bi <-
  rec_bi %>% 
  workflow(logistic_reg())
wf_bi

deg_frees <- seq.int(1, 4) # c(2, 4, 8) %>% as.integer()
grid_bi <- crossing(
  # mixture = c(0, 0.25, 0.5, 0.75, 1),
  # mixture = 0.5,
  # penalty = 10 ^ seq(-3, 0, .1),
  deg_free_lon = deg_frees,
  deg_free_lat = deg_frees
)
grid_bi

tune_bi <-
  wf_bi %>% 
  tune_grid(
    grid = grid_bi,
    resamples = folds_bi,
    metrics = metset_bi,
    control = ctrl
  )
tune_bi
autoplot(tune_bi)

params_best_bi <- tune_bi %>% select_best('accuracy')
params_best_bi
wf_best_bi <- wf_bi %>% finalize_workflow(params_best_bi)
fit_best_bi <- wf_best_bi %>% fit(df_slim_adj)
fit_best_bi
fit_best_bi %>% augment(df_slim_adj) %>% relocate(matches('pred_'))
# fit_best_bi2 <- glm(formula(term ~ lon + lat), data = df_slim_adj, family = 'binomial')
# fit_best_bi2 %>% fitted()

col_trt <- 'term'
res_match <-
  Matching::Match(
    # caliper = 0.01,
    # exact = TRUE,
    ties = FALSE,
    X = fit_best_bi %>% augment(df_slim_adj) %>% pull(.pred_Whataburger), # fitted(),
    Y = df_slim_adj[['rating_num']],
    Tr = df_slim_adj[[col_trt]] %>% as.integer() %>% {. - 1L}
  )
res_match

features_match <- 
  bind_rows(
    df_slim_adj[res_match[['index.control']], ] %>% mutate(grp = 'control'), 
    df_slim_adj[res_match[['index.treated']], ] %>% mutate(grp = 'treatment')
  )
features_match

f_predict <- function(data) {
  # idk why this isn't working.
  # fit_best_bi %>% extract_fit_parsnip() %>% broom::augment(new_data = data, type.predict = 'response')
  predict(fit_best_bi, data, type = 'prob') %>% 
    select(.pred_Whataburger) %>% 
    rename(.fitted = 1) %>% 
    bind_cols(data)
}

fit_best_bi %>% predict(df_slim_adj, type = 'prob')

preds <-
  bind_rows(
    df_slim_adj %>% f_predict() %>% mutate(grp_adj = 'Un-adjusted'),
    features_match %>% f_predict() %>% mutate(grp_adj = 'Adjusted')
  ) %>% 
  mutate(
    across(grp, str_to_title)
  )
preds
preds %>% filter(!is.na(grp))
preds %>% filter(is.na(grp))

bw <- 0.001
viz_prop_probs <-
  preds %>% 
  ggplot() + 
  aes(x = .fitted, fill = grp) + 
  geom_histogram(data = . %>% filter(grp_adj == 'Un-adjusted'), fill = 'grey80', binwidth = bw, aes(y = -..count..)) +
  geom_histogram(data = . %>% filter(grp == 'Control', grp_adj == 'Adjusted'), binwidth = bw, aes(y = -..count..)) +
  geom_histogram(data = . %>% filter(grp == 'Treatment', grp_adj == 'Adjusted'), binwidth = bw) +
  scale_fill_manual(values = c(`Control` = 'dodgerblue', `Treatment` = 'darkorange')) +
  scale_x_continuous(labels = scales::percent) +
  # scale_y_continuous(breaks = c(75, ))
  guides(fill = guide_legend('')) +
  theme(
    plot.title = element_text(size = 16, face = 'bold'),
    legend.position = 'top'
  ) +
  labs(
    title = sprintf('Probabilites of Whatabuger vs. McDonald\'s for matching model'),
    caption = 'Gray illustrates un-adjusted samples, while colors illustrate adjusted samples.',
    y = '# of Observations',
    x = 'P(Whataburger)'
  )
viz_prop_probs

ggsave(
  filename = file.path(dir_proj, 'matching.png'),
  viz_prop_probs,
  width = 8,
  height = 8,
  type = 'cairo'
)

# model fit ----
# wb is for whataburger
metset_wb <- metric_set(rmse)
set.seed(42)
split_wb <- features_match %>% initial_split(strata = rating_num_adj)
df_trn_wb <- split_wb %>% training()
df_tst_wb <- split_wb %>% testing()
folds_wb <- df_trn_wb %>% vfold_cv(strata = term, v = 5)
folds_wb

rec_wb <-
  features_match %>% 
  filter(term == 'Whataburger') %>% 
  recipe(formula(rating_num_adj ~ lon + lat), data = .) %>% 
  step_interact(terms = ~lon:lat) %>% 
  step_ns(lon, deg_free = tune('deg_free_lon')) %>% 
  step_ns(lat, deg_free = tune('deg_free_lat'))
rec_wb

wf_wb <-
  rec_wb %>% 
  workflow(linear_reg())
wf_wb

deg_frees_wb <- c(2, 4, 8) %>% as.integer()
grid_wb <- crossing(
  # mixture = c(0, 0.25, 0.5, 0.75, 1),
  # mixture = 0.5,
  # penalty = 10 ^ seq(-3, 0, .1),
  deg_free_lon = deg_frees_wb,
  deg_free_lat = deg_frees_wb
)
grid_wb

tune_wb <-
  wf_wb %>% 
  tune_grid(
    grid = grid_wb,
    resamples = folds_wb,
    metrics = metset_wb,
    control = ctrl
  )
tune_wb
autoplot(tune_wb)

params_best_wb <- tune_wb %>% select_best()
params_best_wb
wf_best_wb <- wf_wb %>% finalize_workflow(params_best_wb)
features_match_filt <- features_match %>% filter(term == 'Whataburger')
fit_best_wb <- wf_best_wb %>% fit(features_match_filt)
fit_best_wb
preds_wb <- fit_best_wb %>% augment(features_match_filt) %>% relocate(.pred)
p <-
  preds_wb %>% 
  ggplot() +
  aes(x = lon, y = lat, z = .pred) +
  geom_polygon(inherit.aes = FALSE, data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  stat_summary_hex() +
  scale_fill_viridis_c(option = 'H') +
  guides(fill = guide_legend('Rating (Low to High)', title.hjust = 0.5, title.position = 'top', label = FALSE)) +
  theme(
    legend.position = 'top',
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 11),
    plot.title = element_text(hjust = 0),
    plot.subtitle = ggtext::element_markdown(hjust = 0, size = 12, face = 'plain'),
    plot.caption = ggtext::element_markdown(size = 11, face = 'italic'),
    plot.tag = ggtext::element_markdown(),
    plot.tag.position = c(.01, 0.01)
  ) +
  # coord_map() +
  # coord_cartesian() +
  coord_equal(ratio = 1.2) +
  labs(
    title = 'Where are the best Whataburgers in Texas?',
    subtitle = 'Whataburgers on the borders have the highest ratings (out-of-state visitors?)',
    tag = '**Viz**: Tony ElHabr | **Data**: Yelp',
    caption = 'Emperical Bayes adjustment applied to adjust for<br/>low review counts. Whataburger reviews matched with McDonald\'s<br/>reviews to account for confounding due to regional bias.<br/>Model predicts Yelp rating based on latitude and longitude.',
    x = NULL, y = NULL
  )
p

ggsave(
  filename = file.path(dir_proj, 'whataburger.png'),
  p,
  width = 9,
  height = 9,
  type = 'cairo'
)
