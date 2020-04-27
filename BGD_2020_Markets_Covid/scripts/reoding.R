rm(list=ls())
# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(stringr)
library(AMR)
library(naniar)


# data_read ---------------------------------------------------------------

main_df <- read.csv("inputs/02_cleaned_data/raw_data_with_round/raw_data_with_round.csv", stringsAsFactors = FALSE,
                    na.strings = c("", " ", NA))


# recoding ----------------------------------------------------------------

colnams <- c("days_of_stock_of_rice", "restocking_time_of_rice", "days_of_stock_of_cooking_oil",
             "restocking_time_of_cooking_oil","days_of_stock_of_lentils", "restocking_time_of_lentils",
             "days_of_stock_of_leafy_greens","restocking_time_of_leafy_greens","days_of_stock_of_bananas",
             "restocking_time_of_bananas","days_of_stock_of_eggs","restocking_time_of_eggs",
             "days_of_stock_of_fish", "restocking_time_of_fish", "days_of_stock_of_soap",
             "restocking_time_of_soap", "days_of_stock_of_washing_powder", "restocking_time_of_washing_powder")


for(i in colnams){
    print(i)
    col <- paste0("i.",i)
    main_df[[col]] <- if_else(main_df[[i]] %in% 0:3,"0-3 days",
                              if_else(main_df[[i]] %in% 4:7, "4-7 days",
                                      if_else(main_df[[i]] > 7,"7+","error",NULL)))
  }

colnames_proportion <- c("rice_sale_in_past_week","oil_sale_in_past_week",
                         "lentils_sale_in_past_week","leafy_greens_sale_in_past_week","bananas_sale_in_past_week",
                         "eggs_sale_in_past_week","fish_sale_in_past_week","soap_sale_in_past_week",
                         "washing_powder_sale_in_past_week","paracetamol_sale_in_past_week","tarpaulin_sale_in_past_week")

for(i in colnames_proportion){
  print(i)
  col <- paste0("i.",i)
  main_df[[col]] <- main_df[[i]]/main_df$vendors_operational
}


# write_csv ---------------------------------------------------------------

write.csv(main_df,"inputs/02_cleaned_data/market_assessment_recoding.csv")

