# stahne tweety a uloží dataset do složky /data (není 100% potřeba, /data jsou naplněna)

library(tidyverse) # protože tidyverse
library(rtweet) # knihovna na twitter
library(caret) # pro vynucení vybalancovaného rozdělení pravého / falešného Okamury

# soubor je gitignorován coby nerelevantní; postup získání a uložení tokenu
# je popsán na dokumentaci package rtweet: https://rtweet.info/#create-an-app
twitter_token <- readRDS("token.rds")

raw_tweets <- get_timelines(c("tomio_cz", "Tomio_Okamura"), # tomio_cz = pravý, Tomio_Okamura = falešný
                        n = 10000,
                        token = twitter_token) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""), # neužitečné znaky
         name = as.factor(screen_name),
         original = !is_retweet)  %>% # originál znamená, že nejde o retweet
  mutate(text = str_replace_all(text, '"', "")) %>% # uvozovky dělají jenom neplechu
  select(id = status_id,
         name,
         created = created_at,
         text,
         source,
         original,
         lajku = favorite_count,
         retweetu = retweet_count)


balanced_tweets <- downSample(raw_tweets, raw_tweets$name, list = F) # vybalancování datasetu

writeLines(paste("Staženo",  # podat zprávu co se chytlo a co se vyvážilo
                 nrow(raw_tweets),
                 "tweetů; vyváženo na",
                 nrow(balanced_tweets),
                 "tweetů."))

print(table(balanced_tweets$name)) # kolik se chytilo textu?

write_csv(balanced_tweets, "./data/tweety.csv")