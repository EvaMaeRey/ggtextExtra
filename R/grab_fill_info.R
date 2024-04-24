grab_fill_info <- function(plot){
  
fill_values_df <- ggplot2::layer_data(plot) %>%  
  .[,c("fill", "group")] |> 
  dplyr::distinct()

fill_var_name <- plot$mapping$fill |> 
  capture.output() %>% .[2] %>% 
  stringr::str_extract("\\^.+") %>% 
  stringr::str_remove("\\^")

fill_var <- plot$data %>% 
  .[,fill_var_name] 

fill_var_df <- tibble::tibble(fill_var, group = as.numeric(fill_var)) %>%
  dplyr::distinct()


dplyr::left_join(fill_values_df, 
                 fill_var_df, 
                 by = dplyr::join_by(group)) %>% 
  dplyr::mutate(html_statements = 
                  paste0("<span style = 'color: ", 
                         .data$fill, 
                         "'>", 
                         .data$fill_var, 
                         "</span></strong>") )
  
}
