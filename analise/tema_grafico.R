tema <- function(base_size = 13,
                        base_family = "Inconsolata") {
  (
    theme_foundation(base_size = base_size,
                     base_family = "Inconsolata") + theme(
                       axis.line.x = element_line(
                         colour = "black",
                         size = 0,
                         linetype = "solid"
                       ),
                       axis.line.y = element_line(
                         colour = "black",
                         size = 0,
                         linetype = "solid"
                       ),
                       axis.text = element_text(size = ceiling(base_size * 0.7), colour = "black"),
                       axis.title = element_text(size = ceiling(base_size * 0.7), hjust = 1),
                       panel.grid.minor = element_blank(),
                       panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
                       panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       strip.background = element_rect(linetype = 0),
                       strip.text = element_text(),
                       strip.text.x = element_text(vjust = 0.5),
                       strip.text.y = element_text(angle = -90),
                       legend.text = element_text(
                         size = ceiling(base_size / 1.4), 
                         family = "Inconsolata"
                         ),
                       legend.title = element_text(
                         size = base_size / 1.2,
                         face = "bold",
                         family = "Inconsolata"
                       ),
                       legend.position = "right",
                       legend.key = element_rect(fill = "white", colour = NA),
                       legend.background = element_rect(colour = NA),
                       plot.background = element_rect(colour = "white"),
                       plot.title = element_text(size = ceiling(base_size * 1.5), face = "bold"),
                       plot.subtitle = element_text(size = ceiling(base_size * 1.05),margin=margin(5,0,20,0)),
                       plot.caption = element_text(size = 9, hjust = 0, margin=margin(20,0,0,0)),
                       plot.margin = unit(c(1, 1, 1, 1), "lines")
                     )
  )
}
