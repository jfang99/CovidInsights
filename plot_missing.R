plot_missing <- function(mydata, percent = FALSE) {
  dp <- mydata
  dp <- dp %>%
    rownames_to_column("id") %>%
    gather(key, value, -id) %>%
    mutate(missing = ifelse(is.na(value), "yes", "no"))
  ggplot(dp, aes(x = key, y = fct_rev(id), fill = missing)) +
    geom_tile() +
    ggtitle("Missing Value Heatmap") +
    scale_fill_viridis_d() +
    xlab("Variables")+
    ylab("Observations")+
    theme(axis.text.x = element_text(angle=50, hjust=1),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank()) +
    theme(plot.title = element_text(hjust = 0.5))
}