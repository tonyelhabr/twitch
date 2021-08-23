
library(tidyverse)
library(nbastatR)
library(rpart)

dir_proj <- '02-unique_stat_combos'
dir_data <- file.path(dir_proj, 'players')
fs::dir_create(dir_data)
# players_tables()
# Sys.setenv("VROOM_CONNECTION_SIZE")
# getOption("VROOM_CONNECTION_SIZE")
df_dict_nba_players <- nbastatR::nba_players()

players <- 
  df_dict_nba_players %>% 
  janitor::clean_names() %>%
  filter(count_seasons >= 7 | (is_active)) # %>% filter(is_active)
players

fs::dir_create(dir_data)

path_data <- fs::path(dir_proj, 'careers.rds')


if(!fs::file_exists(path_data)) {
  
  # Have to do all this extra stuff (and restart the RStudio session) after around ~300 scrapes. Can't tell if it's an NBA web site limitation or something else. Oh well, do this ugly stuff and restart the session (which seems to re-set the possible rate limitation) until iterating through all players. Probably should 
  players_vec <- players_todo %>% pull(name_player)
  i_init <- 1
  n_player <- min(290, length(players_vec) - i_init)
  players_vec_filt <- players_vec[i_init:(i_init + n_player)]
  i <- 1
  scrape_player <- function(player) {
    cat(glue::glue('Pulling data for {player} at {Sys.time()} ({i} of {n_player}).'), sep = '\n')
    i <<- i + 1
    res <- player %>% players_careers(modes = c('Totals', 'PerGame'))
    write_rds(res, fs::path(dir_data_sub, sprintf('%s.rds', player)))
    res
  }
  
  scrape_player_possibly <- possibly(scrape_player, otherwise = tibble())
  careers <- players_vec_filt %>% map_dfr(scrape_player_possibly)
  
  careers
  write_rds(df, path_data)
} else {
  careers <- read_rds(path_data)
}

careers

careers_proc <-
  full_join(
    careers %>% 
      janitor::clean_names() %>% 
      filter(name_table == 'CareerTotalsRegularSeason') %>% 
      filter(mode_search == 'Totals') %>% 
      select(name_table, mode_search, name_player, data_table) %>% 
      unnest(data_table) %>% 
      janitor::clean_names() %>% 
      select(-c(name_table, mode_search)) %>% 
      rename_with(~sprintf('%s_%s', .x, 'total'), -c(name_player)),
    careers %>% 
      janitor::clean_names() %>% 
      filter(name_table == 'CareerTotalsRegularSeason') %>% 
      filter(mode_search == 'PerGame') %>% 
      select(name_table, mode_search, name_player, data_table) %>% 
      unnest(data_table) %>% 
      janitor::clean_names() %>% 
      select(-c(name_table, mode_search)) %>% 
      rename_with(~sprintf('%s_%s', .x, 'pg'), -c(name_player))
  ) %>% 
  rename(player = name_player)

player_y <- 'Will Barton'
player_y <- 'Jeremy Pargo'
# careers_proc %>% skimr::skim()
df_trn <-
  careers_proc %>%
  mutate(
    is_player = case_when(player == player_y ~ 1, TRUE ~ 0),
    across(where(is.numeric), ~as.double(.x) %>% coalesce(0)),
    idx = row_number()
  ) %>% 
  relocate(idx, is_player) %>% 
  arrange(desc(is_player))
df_trn
# df_trn %>% filter(minutes_total > 1000) %>% arrange(minutes_total)
df_trn %>% arrange(desc(fg3m_pg))
df_trn %>% arrange(pct_fg_total) %>% relocate(pct_fg_total) %>% slice(c(100:119))

df_trn_long <-
  careers_proc %>%
  mutate(
    is_player = case_when(player == player_y ~ 1, TRUE ~ 0),
    across(where(is.numeric), ~as.double(.x) %>% coalesce(0)),
    idx = row_number()
  ) %>% 
  pivot_longer(
    -c(player, is_player, idx)
  )
df_trn_long

df_trn_long_fp <- df_trn_long %>% filter(is_player == 0)
df_trn_long_tp <- df_trn_long %>% filter(is_player == 1)

df_trn <-
  df_trn_long %>% 
  left_join(df_trn_long_tp %>% select(name, value_y = value))

df_trn <-
  bind_rows(
    df_trn_long_tp,
    df_trn_long_fp
  ) %>%
  pivot_wider(names_from = 'name', values_from = 'value') %>%
  mutate(across(is_player, factor))
df_trn

fit <- 
  df_trn %>% 
  select(-matches('_pg$')) %>% 
  as.data.frame() %>% 
  rpart::rpart(formula(is_player ~ . - player - idx), data = ., minbucket = 1)
fit

plot(fit)
text(fit, use.n = TRUE)

fit %>% summary()
fit$variable.importance

# fit %>% broomstick::glance()
# fit %>% broomstick::augment()

viz <- fit %>% rpart.plot::rpart.plot(roundint = FALSE)
viz
viz$snipped.nodes
viz$branch.x
viz$obj$splits

df_trn %>% 
  filter(fg3a_pg >= 3.4 & pf_pg >= 1.7)

?rpart.plot::rpart.plot
df_trn %>% arrange(-pts_pg)
df_trn %>% arrange(desc(treb_total))
df_trn %>% arrange(desc(gp_pg))

# ----
library(rpart)
fit0 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit0
rpart.plot::rpart.plot(fit0)
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(0.65, 0.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,1))
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)
