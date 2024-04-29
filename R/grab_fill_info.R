grab_fill_info <- function(plot = last_plot(), i = NULL){
  
  if(is.null(i)){i <- length(plot$layers)}
  
fill_values_df <- ggplot2::layer_data(plot, i = i) %>%  
  .[,c("fill", "group")] |> 
  dplyr::distinct()

fill_var_name <- plot$mapping$fill |> 
  capture.output() %>% .[2] %>% 
  stringr::str_extract("\\^.+") %>% 
  stringr::str_remove("\\^")

fill_var_df <- plot$data[,fill_var_name] |> 
  dplyr::distinct()

names(fill_var_df) <- "fill_var"

fill_var_df <- fill_var_df |> mutate(group = as.numeric(as.factor(fill_var)))


dplyr::left_join(fill_values_df, 
                 fill_var_df, 
                 by = dplyr::join_by(group)) %>% 
  dplyr::mutate(html_replacements = 
                  paste0("<span style = 'color: ", 
                         .data$fill, 
                         "'>", 
                         .data$fill_var, 
                         "</span></strong>") ) %>% 
  ggplot2::remove_missing()  # where category is NA
  
}
