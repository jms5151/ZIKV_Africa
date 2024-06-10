create_summary_dataset <- function(val_type){
  # format names
  sero_full <- concatSamples(validationName = 'seroprevalence', genQuantName = val_type, percentiles = 95)
  sero_full$Country <- sero_df$Country
  sero_full$Seroprevalence <- sero_df$Seroprevalence
  sero_full$NeutAnti <- sero_df$Neutralizing_antibodies
  
  # create name for model
  modtype <- gsub('R0_|_new', '', val_type)
  modtype <- paste0(toupper(substr(modtype, 1, 1)), substr(modtype, 2, nchar(modtype)), ' model')
  
  # summarize data
  sero <- sero_full %>%
    # filter(anc < 0.2) %>%
    group_by(Country, NeutAnti) %>%
    summarise(S_median = median(Seroprevalence)
              , S_5th = quantile(Seroprevalence, 0.25)
              , S_95th = quantile(Seroprevalence, 0.75)
              , R0_median = median(median)
              , R0_5th = quantile(median, 0.25)
              , R0_95th = quantile(median, 0.75)) %>%
    mutate('Model' = modtype) %>%
    as.data.frame()
  return(sero)
}