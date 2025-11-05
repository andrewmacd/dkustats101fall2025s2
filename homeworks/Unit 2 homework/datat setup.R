library(tidyverse)

amazon.books <- amazon_books_metadata_sample_20k %>% 
  slice_sample(n=300)

amazon.books <- amazon.books %>%
  mutate(
    # extract numeric value
    weight_value = as.numeric(str_extract(item_weight, "\\d*\\.?\\d+")),
    
    # identify unit (ounce, pound, gram, etc.)
    weight_unit = str_to_lower(str_extract(item_weight, "ounce|ounces|pound|pounds|gram|grams|kg|kilogram|kilograms")),
    
    # convert all to grams
    weight_grams = case_when(
      weight_unit %in% c("ounce", "ounces") ~ weight_value * 28.3495,
      weight_unit %in% c("pound", "pounds") ~ weight_value * 453.592,
      weight_unit %in% c("gram", "grams") ~ weight_value,
      weight_unit %in% c("kg", "kilogram", "kilograms") ~ weight_value * 1000,
      TRUE ~ NA_real_
    ),
    
    weight_grams = round(weight_grams, 1)
  )

amazon.books <- amazon.books %>%
  mutate(
    # extract a 4-digit year from the text
    publisher_year = as.numeric(str_extract(publisher_date, "\\b(19|20)\\d{2}\\b"))
  )

amazon.books <- amazon.books %>% 
  mutate(category = category_level_2_sub,
         sub_category = category_level_3_detail)

amazon.books <- amazon.books %>% 
  select(c(title, subtitle, author_name, author_about, publisher, 
           publisher_date, publisher_year, format, page_count, language,
           isbn_10, isbn_13, category, sub_category,
           average_rating, rating_number, price, features_text, 
           dimensions, item_weight, weight_grams)) 

write.csv(amazon.books, "amazon.books.csv")