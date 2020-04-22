
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


