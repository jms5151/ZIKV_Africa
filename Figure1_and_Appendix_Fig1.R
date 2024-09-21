# Fig. 1 (Seroprevalence validation) ---------------------------

# requires the following functions:
# concatSamples from functions/fn_concat_samples.R
# create_summary_dataset from functions/fn_create_summary_dataset.R0
# validation_plot from functions/fn_plot_validation.R

sero_df <- read.csv('seroSites_Aaa_v2.csv')

cdf <- create_summary_dataset(val_type = 'R0_climate_new')
adf <- create_summary_dataset(val_type = 'R0_ancestry_new')
fdf <- create_summary_dataset(val_type = 'R0_full_new')

val.df <- do.call(rbind, list(cdf, adf, fdf))
val.df$Model <- factor(val.df$Model, levels = c('Climate model', 'Ancestry model', 'Full model'))
valAll <- validation_plot(df = val.df)
ggsave(valAll, filename = 'Figure3.pdf', width = 10, height = 5)

val.df.neutr <- subset(val.df, NeutAnti == 'Yes')
valNeut <- validation_plot(df = val.df.neutr) 
ggsave(valNeut, filename = 'Appendix_Figure1.pdf', width = 10, height = 5)
