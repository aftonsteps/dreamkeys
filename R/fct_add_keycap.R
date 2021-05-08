add_keycap <- function (ggkeyboard, keys, images, size = 1, buffer = 0.1,
                        ...) 
{
  keyboard <- purrr::map(ggkeyboard$layers, "data") %>% purrr::keep(~"key" %in% 
                                                                      names(.x)) %>% dplyr::bind_rows()
  key_data <- keyboard %>% dplyr::filter(key %in% !!keys) %>% 
    dplyr::distinct(key, x_start, x_end, y_start, y_end, height, width)
  
  for (i in 1:length(images)) {
    ggkeyboard <- 
      ggkeyboard +
      ggplot2::annotation_custom(grid::rasterGrob(images[[i]]),
                                 xmin = key_data$x_start[i] - buffer, 
                                 ymin = key_data$y_start[i] - buffer,
                                 xmax = key_data$x_end[i] + buffer,
                                 ymax = key_data$y_end[i] + buffer)
  }
  
  return(ggkeyboard)
}
