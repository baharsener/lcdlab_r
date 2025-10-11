#' Custom ggplot2 theme for publication-quality plots
#'
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
#' @export
theme_Publication <- function(base_size=18, base_family="Helvetica") {
  ggthemes::theme_foundation(base_size=base_size, base_family=base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.size = grid::unit(0.8, "cm"),
      legend.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.8)),
      legend.key = ggplot2::element_rect(colour = NA),
      plot.margin = grid::unit(c(10,5,5,5), "mm"),
      strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
