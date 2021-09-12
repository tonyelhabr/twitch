
# react?

+ [Attack on UMAP/t-sne](https://twitter.com/lpachter/status/1431325969411821572?s=19)
+ [Counter-argument](https://twitter.com/tim_sainburg/status/1431497387504144394?s=19)

# xEngagement

+ [R package](https://github.com/tonyelhabr/xengagement)
+ [flask app](https://xengagement.herokuapp.com)
+ [Twitter thread](https://twitter.com/TonyElHabr/status/1373277253572960258?s=20)

## Notes

+ direct xgboost model because [case weights](https://github.com/tidymodels/planning/tree/master/case-weights) are important
+ [`{xgbh}` package](https://github.com/tonyelhabr/xgbh) inspired by this project

## TODO

+ Add Brentford and add info for Norwitch and Watford
+ Possibly use [RMSLE](https://juliasilge.com/blog/nyc-airbnb/)? ([`reg:squaredlogerror` objective](https://xgboost.readthedocs.io/en/latest/parameter.html))
+ Fix Github action... broken `{remotes}`?
+ Change `grid_latin_hypercube()` to just `grid_regular()`

# xG Overperformance

+ Who has consistently overperformed their xG (i.e. scored more goals than xG)?
+ Can we model it? How should we model it? Binary classification, or possibly as a regression task where goals or xG is the outcome, and the result is converted to a 0 or 1 depending on the emperical goals/xG?
+ What features should we include for modeling? What is our intuition about what features are most important? Do the models reflect this?
+ Are mixed models helpful here?
+ Is their censoring here? I.e. are players who underperform xG end up having shorter careers, and, thus, are less frequent in the data?

+ Data sources to consider: understat, fbref, or maybe DAVIES (sourced from fbref and transfer market, and possibly others)?
  + understat has 6 leagues (big 5 + Russia), and goes back to 2014-15, but does not separate out penalties. has player ids.
  + fbref goes back to 2017-18 and has 6 leagues(?) (big 5 + MLS). separates out penalties. has much more additional data than can be used. does not have explicit player ids, but urls have ids, so those can be extracted.
  + DAVIES has goals added, which is possibly the most reliable, well-founded metric. it doesn't have player ids, which can make things a little messy.
