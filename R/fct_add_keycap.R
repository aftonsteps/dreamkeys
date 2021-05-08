add_keycap <- function (ggkeyboard, keys, image, size = 1, buffer = 0.1,
                        ...) 
{
  keyboard <- purrr::map(ggkeyboard$layers, "data") %>% purrr::keep(~"key" %in% 
                                                                      names(.x)) %>% dplyr::bind_rows()
  key_data <- keyboard %>% dplyr::filter(key %in% !!keys) %>% 
    dplyr::distinct(key, x_start, x_end, y_start, y_end, height, width)
  
  ggkeyboard +
    ggplot2::annotation_custom(grid::rasterGrob(image),
                               xmin = key_data$x_start[1] - buffer, 
                               ymin = key_data$y_start[1] - buffer,
                               xmax = key_data$x_end[1] + buffer,
                               ymax = key_data$y_end[1] + buffer)
    
}
