rm(list = ls())

# library -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)

items<- c("food_item","non_food_item")[2]

# path --------------------------------------------------------------------

outputfolder_box <-"outputs/03_charts/01_box/"

# data_preparation ---------------------------------------------------------------

cleaned_df <- read.csv("inputs/02_cleaned_data/market_assessment_recoding.csv", stringsAsFactors = FALSE,
                       na.strings = c("", " ", NA))
date_log <- read.csv("outputs/01_data_logger/date_log.csv", stringsAsFactors = FALSE,
                     na.strings = c("", " ", NA)) %>% select(-"reported_date")

data_with_round<- cleaned_df %>%  left_join(date_log,"X_uuid") #add_round


# food_item -------------------------------------------------------------

if (items  =="food_item"){

cols_needed <- c("price_of_1kg","cheapest_price_for_cooking_oil", "cheapest_price_for_1kg_of_lentils",
                    "cheapest_price_for_0.5kg_of_leafy_greens", "cheapest_price_for_1kg_of_bananas",
                    "cheapest_price_for_12__of_eggs", "cheapest_price_for_1kg__of_fish",
                    "cheapest_price_for_12_of_chicken")

data_with_cols <- data_with_round[cols_needed]

final <- data_with_cols %>% gather()

final <- final %>% mutate(
  round_1 = final$value,
  round_2 = if_else( !is.na(final$value), final$value + 5,10)
)

final_group <- final %>% group_by(key) %>% summarise(
  Round1= median(round_1,na.rm = T),
  Round2 = median(round_2,na.rm = T)
)

final_group_gather <- gather(final_group,c(2:ncol(final_group)),key="round",value="value")


final_data_for_chart <- final_group_gather %>% dplyr::mutate(
  name = if_else(grepl("price_of_1kg",key),"Rice",
                 if_else(grepl("cooking_oil",key),"Oil",
                         if_else(grepl("lentils",key),"Lentils",
                                 if_else(grepl("leafy_greens",key),"Leafy greens",
                                         if_else(grepl("eggs",key),"Egg",
                                                 if_else(grepl("bananas",key),"Banana",
                                                         if_else(grepl("fish",key),"Fish",
                                                                 if_else(grepl("chicken",key),"Chicken","error",NULL
                                                                 )))))))),
  color = if_else(grepl("price_of_1kg",key),"#d0cdbc",
                 if_else(grepl("cooking_oil",key),"#a1c5a0",
                         if_else(grepl("lentils",key),"#595959",
                                 if_else(grepl("leafy_greens",key),"#c0c0bf",
                                         if_else(grepl("eggs",key),"#d3e5d4",
                                                 if_else(grepl("bananas",key),"#53b5cd",
                                                         if_else(grepl("fish",key),"#eeadad",
                                                                 if_else(grepl("chicken",key),"#ee5658","error",NULL
                                                                 )))))))))
final_data_for_chart$color <-factor(final_data_for_chart$color,unique(final_data_for_chart$color))
final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value)

ggplot(final_data_for_chart, aes(x = round, y = value,group =name)) +
  ylim (0,ymax)+
  geom_path(aes(color=name),size=1)+
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
       legend.title=element_blank(),
       legend.text = element_text(size = 8,color="#58585A"),
       legend.position = "bottom",
       legend.justification = 0,
       legend.key.width =  unit(1,"cm"),
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(.5, "cm"),
       legend.key.size = unit(1, 'lines'),
       legend.key = element_rect(fill = NA),
       legend.text.align = 0)+ ylab("Price (BDT)")+
  scale_color_manual(values = as.character(final_data_for_chart$color) %>% dput)


ggsave(path = outputfolder_box,filename ="line_food_item.jpg" ,width=12.9213,height=7,units="cm",scale = 1.8)
}
# Non food item -----------------------------------------------------------
if (items  =="food_item"){

cols_needed <- c("cheapest_price_for_100g_soap_bar_of_soap", "cheapest_price_for_0_5l_of_bleachwashing_powder",
                      "cheapest_price_for_12_of_paracetamol")

data_with_cols <- data_with_round[cols_needed]

final <- data_with_cols %>% gather()

final <- final %>% mutate(
  round_1 = final$value,
  round_2 = if_else( !is.na(final$value), final$value + 5,10)
)

final_group <- final %>% group_by(key) %>% summarise(
  Round1= median(round_1,na.rm = T),
  Round2 = median(round_2,na.rm = T)
)

final_group_gather <- gather(final_group,c(2:ncol(final_group)),key="round",value="value")


final_data_for_chart <- final_group_gather %>% dplyr::mutate(
  name = if_else(grepl("soap_bar",key),"Soap",
                 if_else(grepl("bleachwashing",key),"Bleach/washing powder",
                         if_else(grepl("paracetamol",key),"Paracetamol", "error",NULL))),

 color = if_else(grepl("soap_bar",key),"#d0cdbc",
               if_else(grepl("bleachwashing",key),"#a1c5a0",
                       if_else(grepl("paracetamol",key),"#595959", "error",NULL))))

final_data_for_chart$color <-factor(final_data_for_chart$color,unique(final_data_for_chart$color))
final_data_for_chart$name <-factor(final_data_for_chart$name,unique(final_data_for_chart$name))

ymax <- max(final_data_for_chart$value)

ggplot(final_data_for_chart, aes(x = round, y = value,group =name)) +
  ylim (0,ymax)+
  geom_path(aes(color=name),size=1)+
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, linetype = "dashed",
                                          colour = "#c1c1c1"),
        legend.title=element_blank(),
        legend.text = element_text(size = 8,color="#58585A"),
        legend.position = "bottom",
        legend.justification = 0,
        legend.key.width =  unit(1,"cm"),
        legend.spacing.x = unit(1, "cm"),
        legend.spacing.y = unit(.5, "cm"),
        legend.key.size = unit(1, 'lines'),
        legend.key = element_rect(fill = NA),
        legend.text.align = 0)+ ylab("Price (BDT)")+
  scale_color_manual(values = as.character(final_data_for_chart$color) %>% dput)


ggsave(path = outputfolder_box,filename ="line_non_food_item.jpg" ,width=12.9213,height=7,units="cm",scale = 1.8)

}


