library(tidyverse)

amazon.books <- amazon_books_metadata_sample_20k %>% 
  slice_sample(n=200)

