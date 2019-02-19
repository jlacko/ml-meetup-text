# napočte nejsložitější verzi (funkční API + boční vstup), o výsledku podá zprávu

library(tidyverse)
library(lubridate) # kvůli práci s datumy
library(scales) # kvůli procentům v ose grafu na závěr
library(keras)

set.seed(42) # nic jiného by se myším nelíbilo, a když to platěj...

# připravený slovník
slovnik <- read_csv("./data/slovnik.csv")

# pomocný data frame - pro doplnění slov na 150 sloupců
vata <- expand.grid(id = unique(read_csv("./data/tweety.csv")$id),
                    no_slovo = 1:150,
                    id_slovo = 0)

# data frame, co bude vstupní maticí
word_matrix <- read_csv("./data/slova.csv") %>% # slova z tweetů...
  inner_join(slovnik, by = c("lemma" = "lemma")) %>% # filtrovací join = slova, co nejsou ve slovníku vypadnou
  select(id = doc_id, lemma, id_slovo) %>%
  group_by(id) %>%
  mutate(no_slovo = row_number()) %>% # pořadí slova ve větě - bude sloupec matice
  ungroup() %>%
  select(id, no_slovo, id_slovo) %>% # relevantní sloupce
  rbind(vata) %>% # připravená vata
  group_by(id, no_slovo) %>%
  mutate(id_slovo = max(id_slovo)) %>% # vytvoří duplicity; potřeba unique()
  ungroup() %>%
  unique() %>% # protože v mutate byly duplicity...
  spread(no_slovo, id_slovo) # do široka...

keras_input <- readr::read_csv("./data/tweety.csv") %>%
  select(id, name, text, hodina = created, original) %>%
  mutate(hodina = hour(hodina) / 24) %>% # hodina jako zlomek dne
  inner_join(word_matrix, by = c("id" = "id"))

text_data <- keras_input %>%
  select(-id, -name, -text, -hodina, - original)

metadata <- keras_input %>%
  select(hodina, original)

x_train_text <- data.matrix(text_data) # všechno kromě targetu jako matice
x_train_meta <- data.matrix(metadata) # metadata o tweetech samostatně, jako matice

y_train <- keras_input %>%
  mutate(pravy_tomio = ifelse(name == "tomio_cz", 1, 0)) %>%
  select(pravy_tomio) %>% # jenom target
  data.matrix()

vocab_size <- slovnik %>% # = vybrat unikátní idčka slov a spočítat je
  pull(id_slovo) %>%
  unique() %>%
  length() + 1 # navíc za nulu na začátku...

# deklarovat model
text_input <- layer_input(shape = c(150), dtype = "int32", name = "textovy_vstup")

lstm_out <- text_input %>%
  layer_embedding(input_dim = vocab_size, output_dim = 32) %>%
  bidirectional(layer_lstm(units = 16)) %>%
  layer_dropout(rate = 0.2)

metadata_output <- lstm_out %>%
  layer_dense(units = 1, activation = "sigmoid", name = "vedlejsi_vystup")

metadata_input <- layer_input(shape = c(2), name = "metadatovy_vstup")

main_output <- layer_concatenate(c(lstm_out, metadata_input)) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid", name = "hlavni_vystup")

model <- keras_model(
  inputs = c(text_input, metadata_input),
  outputs = c(main_output, metadata_output)
)

model %>% compile(
  optimizer = "rmsprop",
  loss = list("binary_crossentropy", "binary_crossentropy"),
  loss_weights = list(hlavni_vystup = 1.0, vedlejsi_vystup = 0.3),
  metrics = c("accuracy")
)

# natrénovat model
history <- model %>%
  fit(
    x = list(x_train_text, x_train_meta),
    y = list(y_train, y_train),
    epochs = 15,
    shuffle = T,
    batch_size = nrow(keras_input) / 5,
    validation_split = 1 / 5
  )

# podat o všem zprávu
writeLines(paste0("Validation Accuracy after ", history$params$epochs, " epochs: ",
                  as.character(formatC(100 * last(history$metrics$hlavni_vystup_acc), digits = 2, format = "f")), "%"))

graf <- ggplot() +
  geom_line(aes(x = 1:history$params$epochs, y = history$metrics$hlavni_vystup_acc), col = "red") +
  labs(x = "epoch", y = "accuracy") +
  theme(axis.title = element_blank()) +
  scale_y_continuous(labels = percent, limits = c(0, 1))

print(graf)