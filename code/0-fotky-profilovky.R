# stahne profilovky, spojí do jednoho obrázku a uloží do složky /img

library(tidyverse)
library(magick)

falesny <- image_read("https://pbs.twimg.com/profile_images/685595149242609664/BU3gxo38_400x400.jpg")
pravy   <- image_read("https://pbs.twimg.com/profile_images/808994525385519104/2MBupdXy_400x400.jpg")

img <- image_append(c(pravy, falesny), stack = F) %>%
  image_scale("x400") # size 400px × 800px je celkem cajk...

image_write(img, path = "./img/mugshots.jpg", format = "jpg")