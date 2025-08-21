# Graphical abstract
#
# On the left: Fig1c (map of geographic distribution of data sites)
# Upper right: p1 + map_legend (predicted RA over MAP cases)
# Upper left: p4 + fa_legend (predicted RA over forest age cases)
# All are created in the analysis: 
source("/GlobalForesRA_analysis.r")

# Load libraries 
library(gridExtra)
library(grid)
library(cowplot)

# Crop Fig1c, we're short on space
Fig1c_cropped <- Fig1c + xlim(-170, 170)

# Edit ylab (in text ylab of these figures is R/(R+L))
p1_edited <- p1 + ylab("RA proxy (R/(R+L))") 
p4_edited <- p4 + ylab("RA proxy (R/(R+L))") 

# Get p1, p4 legends and reduce text size
map_legend <- get_legend(RRL_plot + theme(legend.title = element_text(size = 10), 
                                          legend.text =  element_text(size = 8)))
fa_legend <- get_legend(RRL_FA_plot + theme(legend.title = element_text(size = 10), 
                                            legend.text = element_text(size = 8)))

# Add margins to p1 and p4
p1_margins <- p1_edited + theme(
  panel.grid.major = element_line(color = "grey90", size = 0.5), # Add grid lines
  panel.grid.minor = element_line(color = "grey95", size = 0.25),
  plot.margin = margin(t = 30, r = 2, b = 5, l = 5, unit = "pt")  # Large top margin
) 

p4_margins <- p4_edited + theme(
  panel.grid.major = element_line(color = "grey90", size = 0.5), # Add grid lines
  panel.grid.minor = element_line(color = "grey95", size = 0.25),
  plot.margin = margin(t = 5, r = 2, b = 30, l = 5, unit = "pt")  # Large bottom margin
) 

# Fine-tune MAP legend position, why are these so wonky??
map_legend_positioned <- ggdraw() + draw_grob(map_legend, x = 0.035, hjust = 0)
fa_legend_positioned <- ggdraw() + draw_grob(fa_legend, x = -0.105, hjust = 0, vjust = -.2)

layout_matrix <- rbind(
  c(1, 1, 1, 1, 2, 3),
  c(1, 1, 1, 1, 4, 5)
)

# Arrange all elements
graphical_abstract <- grid.arrange(
  Fig1c_cropped,
  p1_margins,
  map_legend_positioned,
  p4_margins,
  fa_legend_positioned,
  layout_matrix = layout_matrix,
  widths = c(1, 1, 1, 1, 1, 0.45),
  heights = c(1, 1),
  padding = unit(1, "mm")
)

# Save
# Increase width to 12.7 so that legend text is not cut off on the right side
# After much unsuccessful fiddling, it seems like this is the easiest way
ggsave(filename = "Output/GraphicalAbstract_1000dpi.jpeg",
       graphical_abstract, width = 12.7, height = 5, units = "in", dpi = 1000) 




