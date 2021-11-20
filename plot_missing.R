plot_missing <- function(mydata, percent = FALSE) {
  missing_patterns <- data.frame(is.na(mydata)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  if (percent) {
    missing_patterns$count <- missing_patterns$count/nrow(mydata)
  }
  tmp <- missing_patterns[,1:(ncol(missing_patterns)-1)]
  tmp$pattern <- 1:nrow(missing_patterns)
  
  tmpp <- missing_patterns[,1:(ncol(missing_patterns)-1)] * missing_patterns$count
  
  tmp <- pivot_longer(tmp, cols = !pattern, names_to = "variables", values_to = "TF")
  ind <- colSums(tmpp)
  tmp$var_count <- ind[tmp$variables]
  
  g <- ggplot(tmp, aes(reorder(variables,var_count), pattern)) +
    geom_tile(aes(fill=TF), color="gray",size=2) +
    scale_y_continuous(breaks = 1:nrow(missing_patterns)) +
    scale_fill_manual(values = c("blue","yellow")) +
    labs(x = "variable") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  df <- data.frame(count = missing_patterns$count, pattern = 1:nrow(missing_patterns))
  g_v <- ggplot(df, aes(x = pattern,y = count)) +
    geom_col() +
    coord_flip() +
    scale_x_continuous(breaks = 1:nrow(missing_patterns)) +
    labs(y = ifelse(percent, "% rows", "count"), x = "pattern")
  
  df_tmp <- data.frame(count = sort(colSums(tmpp),decreasing = F), variable = names(sort(colSums(tmpp),decreasing = F)))
  g_h <- ggplot(df_tmp, aes(x = reorder(variable,count), y = count)) +
    geom_col() +
    scale_y_reverse() +
    labs(x = "variable", y = ifelse(percent, "% rows missing", "count")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  g + g_v + g_h + plot_spacer()
}