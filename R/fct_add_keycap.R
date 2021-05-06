add_keycap <- function (ggkeyboard, keys, image, size = 1, 
                        ...) 
{
  keyboard <- purrr::map(ggkeyboard$layers, "data") %>% purrr::keep(~"key" %in% 
                                                                      names(.x)) %>% dplyr::bind_rows()
  key_data <- keyboard %>% dplyr::filter(key %in% !!keys) %>% 
    dplyr::distinct(key, x_start, x_end, y_start, y_end, height, width)
  
  # ggkeyboard + ggplot2::geom_rect(data = key_data, ggplot2::aes(xmin = x_start, 
  #                                                               xmax = x_end, ymin = y_start, ymax = y_end), colour = colour, 
  #                                 fill = fill, size = size, ...)
  
  cowplot::ggdraw(xlim = c(0, 10), ylim = c(0, 20), clip = "on") +
    cowplot::draw_plot(ggkeyboard, width = 10, height = 20) +
    cowplot::draw_image(image,
                        clip = "on",
                        x = key_data$x_start[1], 
                        y = key_data$y_start[1], 
                        scale = 1)
                        #width = key_data$height[1], 
                        #height = key_data$width[1])

}
