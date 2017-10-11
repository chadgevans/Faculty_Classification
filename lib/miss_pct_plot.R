miss_pct_plot <- function(data){
  miss_pct<-data %>%
    map_dbl(function(x) { 
    round((sum(is.na(x)) / length(x)) * 100, 1)
    })
  plot<-data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) +
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))
  return(plot)
}