# Appendix Fig. 5: PPC plots ---------------------------------------

# extract ppc samples and summarise
ppc_indexes <- names(r0_mod)[grep('ppc', names(r0_mod))]
ppc_estimates <- rstan::extract(r0_mod, ppc_indexes)
ppc_estimates <- lapply(ppc_estimates, quantile, probs=c(0.025,0.50,0.975), na.rm=TRUE)

ppc_estimates_quants <- do.call(rbind.data.frame, ppc_estimates)
colnames(ppc_estimates_quants) <- c('lower', 'median', 'upper')
ppc_estimates_quants$trait <- names(ppc_estimates)
ppc_estimates_quants$trait <- gsub('_ppc.*', '', ppc_estimates_quants$trait)

prior_data_df <- mod_data[unique(ppc_estimates_quants$trait)]
prior_data_df <- map_df(prior_data_df, ~as.data.frame(.x), .id='trait2')
colnames(prior_data_df)[2] <- 'value'

# combine estimated and observed data
ppc <- cbind(ppc_estimates_quants, prior_data_df)
ppc$trait <- gsub('_ancestry|_climate', '', ppc$trait)
ppc$trait <- gsub('_', ', ', ppc$trait)

# edit pMI since there are two 
ppc$trait[ppc$trait2 == 'pMI_climate'] <- 'pMI (climate)'
ppc$trait[ppc$trait2 == 'pMI_ancestry'] <- 'pMI (ancestry)'

# Format Aaa from proportion (0-1) to percent (0-100)
ppc[grepl('ancestry', ppc$trait2), c('lower', 'median', 'upper', 'value')] <- lapply(ppc[grepl('ancestry', ppc$trait2), c('lower', 'median', 'upper', 'value')], function(x) x * 100)

# plot
pdf('Appendix_Figure5.pdf', width = 11, height = 8.5)
ggplot(ppc, aes(value, median)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme_classic() + 
  facet_wrap(.~trait, scales = 'free') +
  geom_abline() +
  ylab('Predicted') +
  xlab('Observed')
dev.off()