# načte dataset tweetů a rozbije ho do datasetu slov, který uloží do složky /data

library(tidyverse)
library(udpipe)

posledni_udpipe <- "czech-pdt-ud-2.3-181115.udpipe" # (zatím) poslední verze na netu

if (!file.exists(posledni_udpipe)) udpipe_download_model(language = "czech") # stačí jednou, má to přes 50 mega..

udmodel <- udpipe_load_model(file = posledni_udpipe) # načtení modelu pro udpipe

tweets <- read_csv("./data/tweety.csv") # tweety uložené z prvního kroku

words <- udpipe_annotate(udmodel,
                         x = tweets$text,
                         doc_id = tweets$id) %>% # UDPIPE provede svojí magii...
  as.data.frame() %>%
  mutate(lemma = ifelse(is.na(lemma), token, lemma)) # prázdné lemma nahradit defaultem


writeLines(paste0("Zpracováno ", nrow(words), " slov, což odpovídá ", ceiling(nrow(words) / 250), " normostranám,\n",
                 "a tedy ~", round(nrow(words) / (250 * 70), 1), " diplomkám na VŠE :)"))

write_csv(words, "./data/slova.csv")

# slovník - lemmata dle frekvence (tj. ne slova!)

slovnik <- words %>% # slova z tweetů
  count(lemma) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>% # menší frekvence není vypovídající...
  mutate(id_slovo = row_number()) %>%
  select(lemma, id_slovo)

write_csv(slovnik, "./data/slovnik.csv") # pro budoucí použití...