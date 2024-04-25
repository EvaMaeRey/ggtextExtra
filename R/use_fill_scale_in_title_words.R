use_fill_scale_in_title_words <- function(plot, i = 1){
  
  out <- plot
  plot_fill_df <- grab_fill_info(plot, i = i)
  
  out$labels$title <- out$labels$title |> 
        auto_color_html(plot_fill_df)

  return(out)
  
}
