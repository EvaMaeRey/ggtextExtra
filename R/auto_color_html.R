auto_color_html <- function(x, fill_df ){
  
 for(i in 1:nrow(fill_df)){

  x <- x |> stringr::str_replace(fill_df$fill_var[i] %>% as.character, fill_df$html_statements[i])
  
 }
  
  x
  
}
