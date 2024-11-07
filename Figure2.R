# Figure 2 (R0 curves & countour plot) ------------------------

# requires the following functions:
# concatSamples from functions/fn_concat_samples.R
# pullSamples from functions/fn_pull_samples.R
# r0_plot from functions/fn_plot_r0.R

# data
sero_df <- read.csv('seroSites_Aaa_v2.csv')

anc_r0_plot <- r0_plot(validationName = 'ancestry', genQuantName = 'R0_ancestry_new') + ylim(0,6)
temp_r0_plot <- r0_plot(validationName = 'temperature', genQuantName = 'R0_climate_new') + ylim(0,6)

contour_samps <- concatAncestrySamples(validationName = 'contour', genQuantName = 'R0_full_new', percentiles = 50)

# plot
contourPlot <- ggplot(contour_samps, aes(temp, anc*100, z=median)) +
  geom_contour_filled(breaks = c(0, 0.5, 0.9, 1, 1.1, 1.5, 2, 3, 4)) + #seq(from = 0, to = 5, by = 0.5)
  # geom_contour(breaks = c(0.5, 1.5), color = 'black', size = 0.7) +
  guides(fill=guide_legend(expression(R[0]))) +
  metR::geom_text_contour(aes(z = median),  col = 'white', size = 5) +
  theme(panel.grid=element_blank(), text=element_text(size=12)) +  # delete grid lines
  scale_x_continuous(limits=c(min(contour_samps$temp),max(contour_samps$temp)), expand=c(0,0)) + # set x limits
  scale_y_continuous(limits=c(min(contour_samps$anc*100),max(contour_samps$anc*100)), expand=c(0,0)) +  # set y limits
  xlab(expression(paste("Temperature (",degree,"C)"))) +
  ylab(expression('Human specialist ancestry (%' *italic(Aaa) *')')) +
  geom_point(sero_df, mapping = aes(x = bio8_20, y = aaa2015*100, z = 0), fill = 'black', color = 'white', pch = 16, size = 3) #+

# combine plots
r0_contours <- plot_grid(anc_r0_plot, temp_r0_plot, contourPlot, ncol = 3, rel_widths = c(0.5, 0.5, 0.9))

# Save
ggsave('Figure2.pdf', r0_contours, width = 12, height = 4)
