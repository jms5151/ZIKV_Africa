# summary statistics ---------------------------------------------

# Extract summary from the STAN fit object
fit_summary <- summary(r0_mod)

# Extract the summary statistics
summary_stats <- fit_summary$summary
summary_stats <- summary_stats[1:27, ]

# Extract Rhat and ESS
rhat <- round(summary_stats[, "Rhat"], 2)
ess <- round(summary_stats[, "n_eff"])

# Create a dataframe with the diagnostics
diagnostics_df <- data.frame(Parameter = rownames(summary_stats), Rhat = rhat, ESS = ess)

# save
write.csv(diagnostics_df, 'Appendix_Table3.csv', row.names = F)
