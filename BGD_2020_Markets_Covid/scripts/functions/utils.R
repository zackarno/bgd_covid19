
facted_histogram<-function(data){
  data_num<- data %>% select_if(is.numeric)
  data_num_long<- data %>% pivot_longer(names_to="question", values_to= "response")
  vlines<-data_num_long %>% group_by(question) %>%
    summarise(mean= mean(response, na.rm=T),
              upper_ci= mean+(1.96*sd(response, na.rm=T)),
              lower_ci=mean-(1.96*sd(response, na.rm=T))
           )


  output_plot<-ggplot(data_num_long_wstats, aes(x=response))+
    geom_histogram(bins=100)+
    facet_wrap(~question)+
    geom_vline(data=vlines,aes(xintercept= lower_ci))+
    geom_vline(data=vlines, aes(xintercept=upper_ci))
  return(output_plot)

}

make_xlsform_lookup_table<- function(kobo_survey, kobo_choices, label_column){
  question_label_col<- paste0("question_",label_column)
  choice_label_col<- paste0("choice_",label_column)
  kobo_survey %>%
    select(type, name,all_of(label_column)) %>%
    rename_all(,.funs = function(x){paste0("question_",x)}) %>%
    filter(str_detect(question_type, "select")) %>%
    select(1:3) %>%
    mutate(question_list_name=str_replace_all(question_type,
                                              c("select_one"="", "select_multiple"="")) %>% trimws()) %>%
    right_join(kobo_choices %>% select(1:3) %>%
                 rename_all(,.funs = function(x){paste0("choice_",x)}),
               by=c("question_list_name"= "choice_list_name")) %>%
    mutate(xml_format=paste0(question_name,".",choice_name))
}


refactor_to_xlsform<-function(data,kobo_survey,kobo_choices ,label_column = "label::english" ){
  xls_lt<-make_xlsform_lookup_table(kobo_survey ,kobo_choices,label_column )
  xls_lt_select_questions<-xls_lt %>%
    filter(str_detect(question_type,"select"))
  for(i in 1: length(unique(xls_lt_select_questions$question_name))){
    col_temp <- unique(xls_lt_select_questions$question_name)[i]
    print(col_temp)
    choices_temp<-xls_lt_select_questions %>% filter(question_name==col_temp) %>% pull(choice_name)
    data<-data %>%
      mutate(!!col_temp:= forcats::fct_expand(as.factor(!!sym(col_temp)), choices_temp))
  }
  return(data)

}


add_rowwise_nfi_food_medians<-function(df){
  df<-df %>%
    mutate(i.food_item_median= matrixStats::rowMedians(as.matrix(df[,food_prices]),na.rm=T),
           i.nfi_item_median= matrixStats::rowMedians(as.matrix(df[,nfi_prices]),na.rm=T)
    )
  return(df)
}


percentage_pt_change<-function(x,y){
  red_tri<-".\\triangles\\red_triangle.png"
  green_tri<-".\\triangles\\green_triangle.png"
  grey_tri<-".\\triangles\\grey_triangle.png"
  results<-list()
  x_minus_y<-(x-y)*100
  triangle<-x_minus_y %>%
    mutate_all(.funs = ~case_when(.> 5~red_tri,
                                  .< -5~green_tri,
                                  TRUE~grey_tri)) %>%
    rename_all(~paste0("@",.,".tri"))
  results[["percent"]]<-round(x*100,0)
  results[["triangle_cols"]]<- triangle
  return(results)
}

percent_diff_dm<-function(x,y){
  red_tri<-".\\triangles\\red_triangle.png"
  green_tri<-".\\triangles\\green_triangle.png"
  grey_tri<-".\\triangles\\grey_triangle.png"
  results<-list()
  x2<-round(((x-y)/x)*100,0)
  triangles<-x2 %>%
    mutate_all(.funs = ~case_when(.==0~grey_tri,
                                  .>0~red_tri,
                                  is.na(.)~ "",
                                  TRUE~ green_tri)) %>%
    rename_all(.funs = ~ paste0("@",.,".tri"))
  x_abs<-x2%>%
    abs() %>%
    rename_all(.funs = ~paste0(.,".perc"))
  results[["current_round"]]<-round(x*100,0)
  results[["percent_diff"]]<-x_abs
  results[["triangle"]]<-triangles
  return(results)
}

