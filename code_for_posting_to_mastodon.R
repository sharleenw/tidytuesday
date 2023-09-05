# code for posting to Mastodon

library(rtoot)
library(emo)


post_toot(status = paste(
  "my first rtoot :rstats:",
  emo::ji("smile"),
  "testttt",
  emo::ji("sparkle"),
  "https://github.com/sharleenw/tidytuesday/tree/master/2020-08-25_chopped"),
  alt_text = "description of media"
)
