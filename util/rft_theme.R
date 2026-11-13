# theme

library(showtext)
font_add_google('Noto Sans')
showtext_opts(dpi = 300)
showtext_auto()

# theme
rft_theme <- function() {
  theme_bw() + 
    theme(
      text = element_text(family = 'Noto Sans', size = 12), 
      title = element_text(family = 'Noto Sans', size = 14, face = 'bold'), 
      panel.grid = element_blank(),
      panel.border = element_rect(color = NA, fill = NA),
      axis.line = element_line(linewidth = .5, color = 'grey20'),
      axis.ticks = element_line(linewidth = .4, color = 'grey30'),
      legend.position = 'top', 
      legend.title.position = 'top',
      legend.justification = 'left', 
      strip.background = element_rect(color = NA, fill = NA), 
      strip.text = element_text(face = 'bold'), 
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
