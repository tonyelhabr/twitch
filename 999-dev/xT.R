library(tidyverse)
dir_proj <- '03-whataburger'

token <- xengagement::get_twitter_token()
token

?rtweet::search_tweets
res <- rtweet::search_tweets2(q = c('xT', 'expected threat', 'PV', 'VAEP'), n = 3200)
res
res %>% select(text)
rtweet::search_tweets
res <- rtweet::search_tweets(c('xT', 'expected threat', 'PV', 'VAEP'))
f_get <- function(user, overwrite = FALSE, n = 3200, ...) {
  path <- file.path(dir_data, sprintf('%s.rds', user))
  if(file.exists(path) & !overwrite) {
    return(read_rds(path))
  }
  res <- rtweet::get_timeline(user, n = n, ...)
  write_rds(res, path)
  res
}

tls_wide <- users %>% map_dfr(f_get)
tls_wide
