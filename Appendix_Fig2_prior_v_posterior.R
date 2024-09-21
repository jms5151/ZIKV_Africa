# Appendix Fig. 2: Prior vs posterior plots --------------------------

# requires the following functions:
# overlay_distributions_plot from functions/plot_prior_posterior_distributions.R

o_anc_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'omega_ancestry_constant', priorValue1 = 0.15, priorValue2 = 0.1)
o_anc_d <- overlay_distributions_plot(mod = r0_mod, param_name = 'omega_ancestry_d', priorValue1 = 1.5, priorValue2 = 1)
o_anc_e <- overlay_distributions_plot(mod = r0_mod, param_name = 'omega_ancestry_e', priorValue1 = 0.5, priorValue2 = 0.1)
a_clim_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'alpha_climate_constant', priorValue1 = 2.02E-04, priorValue2 = 0.01)
a_clim_Tmin <- overlay_distributions_plot(mod = r0_mod, param_name = 'alpha_climate_Tmin', priorValue1 =  13.35, priorValue2 = 1)
a_clim_Tmax <- overlay_distributions_plot(mod = r0_mod, param_name = 'alpha_climate_Tmax', priorValue1 = 40.08, priorValue2 = 0.01)
b_clim_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'b_climate_constant', priorValue1 = 5.5E-04, priorValue2 = 0.1)
b_clim_Tmin <- overlay_distributions_plot(mod = r0_mod, param_name = 'b_climate_Tmin', priorValue1 = 12, priorValue2 = 2)
EIR_clim_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'EIR_climate_constant', priorValue1 = 6.65E-05, priorValue2 = 0.1)
EIR_clim_Tmin <- overlay_distributions_plot(mod = r0_mod, param_name = 'EIR_climate_Tmin', priorValue1 = 10.68, priorValue2 = 1)
EIR_clim_Tmax <- overlay_distributions_plot(mod = r0_mod, param_name = 'EIR_climate_Tmax', priorValue1 = 45.90, priorValue2 = 1)
lf_clim_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'lf_climate_constant', priorValue1 = -1.48E-01, priorValue2 = 0.1)
lf_clim_Tmin <- overlay_distributions_plot(mod = r0_mod, param_name = 'lf_climate_Tmin', priorValue1 = 9.16, priorValue2 = 1)
lf_clim_Tmax <- overlay_distributions_plot(mod = r0_mod, param_name = 'lf_climate_Tmax', priorValue1 = 37.73, priorValue2 = 1)
pMI_anc_const <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_ancestry_constant', priorValue1 = 0.15, priorValue2 = 0.1)
pMI_anc_d <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_ancestry_d', priorValue1 = 0.5, priorValue2 = 0.1)
pMI_anc_e <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_ancestry_e', priorValue1 = 0.5, priorValue2 = 0.1)
pMI_clim_rmax <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_climate_rmax', priorValue1 = 0.24, priorValue2 = 0.1)
pMI_clim_Topt <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_climate_Topt', priorValue1 = 30.08, priorValue2 = 1)
pMI_clim_a <- overlay_distributions_plot(mod = r0_mod, param_name = 'pMI_climate_a', priorValue1 = 3.60, priorValue2 = 1)

p <- plot_grid(o_anc_const
               , o_anc_d
               , o_anc_e
               , a_clim_const
               , a_clim_Tmin
               , a_clim_Tmax
               , b_clim_const
               , b_clim_Tmin
               , EIR_clim_const
               , EIR_clim_Tmin
               , EIR_clim_Tmax
               , lf_clim_const
               , lf_clim_Tmin
               , lf_clim_Tmax
               , pMI_anc_const
               , pMI_anc_d
               , pMI_anc_e
               , pMI_clim_rmax
               , pMI_clim_Topt
               , pMI_clim_a
)
ggsave('Appendix_Figure2.pdf', p, width = 11, height = 11)
