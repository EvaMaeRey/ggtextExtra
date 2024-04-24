use_fill_scale_in_title_words <- function(plot){
  
  out <- plot
  plot_fill_df <- grab_fill_info(plot)
  
  out$labels$title <- out$labels$title |> 
        auto_color_html(plot_fill_df)

  return(out)
  
}
