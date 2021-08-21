
library(tidyverse)

yelp::store_access_token(
  'DW4rR-HA2A4cRL20zxefH7lADdRb7tQmimexUyW4nB65lPtYpsOcWtB7XqhxPMi3h8yzTAq-uTbRML1KosVqnPqEaHmjloZeG-O2KAvynhqe7mvzESvZP4-ge_gOYXYx'
)

dir_proj <- '03-whataburger'
dir_data <- file.path(dir_proj, 'data')
dir.create(dir_data)

yelp::business_search('Whataburger', location = 'Alvarado, Texas', limit = 50) -> x
yelp::business_match(
  'Whataburger',
  city = 'Alvarado',
  state = 'TX',
  country = 'US',
  '',
  match_threshold = 'none'
) -> y
y
walmart <- yelp::business_match("Walmart", "Austin", "TX", 'US', '', match_threshold = 'none')
walmart
x %>% count(name)

# yelp::business_match(
#   'Whataburger',
#   city = 'Austin',
#   country = 'US',
#   state = 'TX' # ,
#   # yelp_business_id = 'PC2i4U_VdMScYb1VvWqvtQ'
# ) -> x
.print <- function(...) {
  cat(glue::glue(...), sep = '\n')
}

f_safe <- possibly(yelp::business_search, otherwise = tibble(), quiet = FALSE)
do_scrape <- function(.term, .location, .offset) {
  path <- file.path(dir_data, .term, .location, sprintf('%04d.rds', .offset))
  fs::dir_create(dirname(path), recurse = TRUE)
  prefix <- glue::glue('{Sys.time()}:')
  suffix <- glue::glue('for {.term}, {.location}, {.offset}.')
  .pprint <- function(x) {
    .print(glue::glue('{prefix} {x} {suffix}'))
  }
  if(file.exists(path)) {
    # cat(glue::glue('{prefix} Returning early {suffix}'), sep = '\n')
    ..print('Returning early}')
    return(read_rds(path))
  }
  res <- f_safe(.term, location = .location, limit = 50, offset = .offset)
  cat(glue::glue('{prefix} Retrieved {suffix}'), sep = '\n')
  write_rds(res, path)
  res
}

res <-
  crossing(
    term = c('Whataburger', 'McDonald\'s'),
    location = sprintf('%s, Texas', c('Houston', 'Dallas', 'Austin', 'San Antonio')),
    offset = seq(0, 1000, by = 50)
  ) %>% 
  # head(1) %>% 
  mutate(data = pmap(list(term, location, offset), do_scrape))
res

df <- res %>% select(term, data) %>% unnest(data)
df

df_slim <-
  df %>% 
  select(term, name, business_id, rating, review_count, lat = latitude, lon = longitude) %>% 
  mutate(
    across(rating, list(num = ~as.numeric(as.character(.x))))
  ) %>% 
  filter(term == name)
df_slim
# df_slim %>% count(name, sort = TRUE)
# df_slim %>% skimr::skim()

df_slim %>% 
  ggplot() +
  aes(x = lon, y = lat) +
  geom_polygon(inherit.aes = FALSE, data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # stat_summary_hex() +
  # ggforce::geom_circle(aes(fill = rating_num)) +
  geom_point(aes(fill = rating_num), shape = 21, color = 'white') +
  facet_wrap(~term)

df_slim %>% 
  ggplot() +
  aes(x = lon, y = lat, z = rating_num) +
  geom_polygon(inherit.aes = FALSE, data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  stat_summary_hex() +
  facet_wrap(~term)

tx <- map_data('state', region = 'texas')
tx
dfr <- map_data('world') %>% select(long, lat, region)
sfr <- sf::st_as_sf(dfr, coords=c('long', 'lat'))
sfc <- rnaturalearth::ne_countries(returnclass='sf')
sf_usa <- sfc %>% filter(adm0_a3 == 'USA')
