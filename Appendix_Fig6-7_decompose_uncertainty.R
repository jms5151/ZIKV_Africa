# Appendix figure, deconstructing variance --------------------------

# requires the following functions:
# decomposeUncertainty from functions/fn_decompose_uncertainty.R
# plotUncertaintyDecomposed from functions/fn_plot_decomposed_uncertainty.R

udf <- decomposeUncertainty(validationName = 'big_cities', genQuantName = 'R0_full_new')

udf$City <- fct_reorder(udf$City, udf$Point_mean)

# Reshape data to long format for uncertainties
long_udf <- udf %>%
  pivot_longer(cols = starts_with("Param_uncertainty"):starts_with("Total_uncertainty"),
               names_to = "Uncertainty_Type",
               values_to = "Uncertainty") %>%
  mutate(
    lower = Point_mean - Uncertainty,
    upper = Point_mean + Uncertainty
  )

full_decomp <- plotUncertaintyDecomposed(df = long_udf)
ggsave('Appendix_Figure6.pdf', full_decomp, width = 8, height = 9)

# Select 10 random cities
random_cities <- long_udf %>%
  distinct(City) %>%  # Get unique cities
  sample_n(6)  # Sample 10 random cities

# Filter the data frame to include only the selected random cities
random_udf <- long_udf %>%
  filter(City %in% random_cities$City)

samp_decomp <- plotUncertaintyDecomposed(df = random_udf)
ggsave('Appendix_Figure7.pdf', samp_decomp, width = 8, height = 6)
