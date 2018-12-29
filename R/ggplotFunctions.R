usethis::use_package('ggplot2')

#' pm_ggplot_theme
#'
#' Formatting for graphs
#'
#' @return ggplot theme elements
#'
#'
#' @export
pm_ggplot_theme <- function () {
  theme_bw(base_size=12, base_family="Avenir") %+replace%
    theme(
      # change stuff here
      panel.grid.major.y = element_line(colour = "gray89",linetype = 'longdash'),
      panel.grid.minor.y = element_line(colour = "gray99", linetype= 'dotted'  ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
