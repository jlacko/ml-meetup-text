# napočte jednoduchou verzi, o výsledku podá zprávu

library(tidyverse)
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
  select(id, name, text) %>%
  inner_join(word_matrix, by = c("id" = "id"))

train_data <- keras_input %>%
  mutate(pravy_tomio = ifelse(name == "tomio_cz", 1, 0)) %>% # klasifikace = potřebuju binární výstup
  select(-id, -name, -text)

x_train <- data.matrix(train_data %>% select(-pravy_tomio)) # všechno kromě targetu jako matice
y_train <- data.matrix(train_data %>% select(pravy_tomio)) # jenom target

vocab_size <- slovnik %>% # = vybrat unikátní idčka slov a spočítat je
  pull(id_slovo) %>%
  unique() %>%
  length() + 1 # navíc za nulu na začátku...

# deklarovat model

model <- keras_model_sequential()

model %>%
  layer_embedding(input_dim = vocab_size, output_dim = 32) %>%
  bidirectional(layer_lstm(units = 16)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "sigmoid") # jeden výstup (pravděpodobnost, že Tomio je pravý)

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)


# natrénovat model
history <- model %>%
  fit(
    x_train, y_train,
    epochs = 15,
    shuffle = T,
    batch_size = nrow(train_data) / 5,
    validation_split = 1 / 5
  )

# podat o všem zprávu
writeLines(paste0("Validation Accuracy after ", history$params$epochs, " epochs: ",
                  as.character(formatC(100 * last(history$metrics$val_acc), digits = 2, format = "f")), "%"))
  
graf <- ggplot() +
  geom_line(aes(x = 1:history$params$epochs,
                y = history$metrics$val_acc), col = "red") +
  labs(x = "epoch", y = "accuracy") +
  theme(axis.title = element_blank()) +
  scale_y_continuous(labels = percent, limits = c(0, 1))

print(graf)