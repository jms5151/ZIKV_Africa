# Figure 3 (R0 in big cities through time) ---------------------

# requires the following functions:
# concatCitySamples from functions/fn_concat_city_samples.R

bc <- concatCitySamples(validationName = 'big_cities', genQuantName = 'R0_full_new')

bc$year <- ifelse(bc$year == 2020, 'Near-current', 'Future')

bc2 <- bc %>%
  spread(key = year, value = median_value) 

# add ordered factor
bc2$Suitability <- ifelse(bc2$Future > 1, 1, 0)
bc2$Suitability <- ifelse(bc2$`Near-current` > 1, 2, bc2$Suitability)

# calculate percent of cities in each category
round(sum(bc2$Suitability == 0)/nrow(bc2), 2) # not imminent
round(sum(bc2$Suitability == 1)/nrow(bc2), 2) # future
round(sum(bc2$Suitability == 2)/nrow(bc2), 2) # near-current
round(sum(bc2$Suitability != 0)/nrow(bc2), 2) # total

bc2 <- bc2[order(bc2$Suitability, bc2$`Near-current`),]
bc2$rank <- seq(1, nrow(bc2), 1)

bc2$City <- fct_reorder(bc2$City, bc2$rank)
bc2$Suitability <- as.factor(bc2$Suitability)

# Map
source('../google_api_key.R')

bc2$R0_max = pmax(bc2$`Near-current`, bc2$Future)

world <- ne_countries(scale='medium', returnclass = 'sf')

africa <- world %>% 
  filter(continent == "Africa")

africaMap <- ggplot(data = africa) +
  geom_sf(fill = 'grey95') +
  coord_sf(xlim = c(-20, 55), ylim = c(-50, 50)) +
  geom_point(data = bc2, mapping = aes(x = lon, y = lat, color = Suitability, size = R0_max), alpha = 0.7) +
  xlab('') +
  ylab('') +
  scale_color_manual(
    values = c('navyblue', 'maroon', 'orange'),
    labels = c('Not imminent', 'Near-current', 'Future'),
    guide = guide_legend(reverse = TRUE, override.aes=list(lwd = 1.3))
  ) + 
  theme_bw() +
  theme(legend.position = c(.15, .3), legend.background = element_rect(fill='transparent')) +
  scale_size_continuous(name = expression(paste('Maximum ', R[0])), breaks = seq(0, 3, 1)) +
  labs(title = expression(paste('Time period & manitude of peak ', R[0], ' by City')),
  ) +
  scale_y_discrete(position = "right") 


# lollipop plot
bc3 <- bc %>%
  left_join(bc2[,c('City', 'Suitability', 'rank')])

bc3$City <- gsub('Democratic Republic of the Congo', 'DRC', bc3$City)
bc3$City <- fct_reorder(bc3$City, bc3$rank)
bc3$year <- factor(bc3$year, levels = c('Near-current', 'Future'))

lollipopPlot <- ggplot(bc3, aes(x = median_value, y = City, pch = as.factor(year), group = City, color = Suitability)) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  xlab(expression('R'[0])) +
  ylab('') +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = expression(paste('Change in ', R[0],' through time'))) +
  theme(legend.position = c(.8, .2), legend.background = element_rect(fill='transparent')) + 
  scale_color_manual(
    values = c('navyblue', 'maroon', 'orange'),
    labels = c('Not imminent', 'Near-current', 'Future'),
    guide = guide_legend(reverse = TRUE, override.aes=list(linetype = 1, shape = NA, lwd = 1.3))
  ) +
  scale_shape_manual(name = 'Time period', 
                     values = c('Future' = 16, 'Near-current' = 2)
  )

# combine
bigCities <- ggarrange(lollipopPlot, africaMap, ncol = 2) # , labels = c('A', 'B')

# save
ggsave('Figure3.pdf', bigCities, height = 7.5, width = 14)