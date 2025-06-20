# bioavailability

### Sitewide RBA error estimation of As

App is hosted at: https://kwnli.shinyapps.io/bioavailability_sitewide/

This app creates three csv outputs to download:

**'DU_error.csv'** contains the mean sample results for every DU across every site, over all simulation iterations. If the sampling plan included increments, these have already been averaged for each sample in this table. These are the column meanings:

- `iter` - iteration number
- `DU` - DU number
- `DU_mn` - the "true" DU-specific RBA mean, drawn from the sitewide distribution
- `n_rba` - the number of samples measured for RBA
- `est_ivb_DU` - the measured IVBA value for the DU, taken from the mean of the sample estimates
- `est_rba_DU` - the estimated RBA value for the DU, based on the IVBA. These values have averaged individual sample increments already, if there were any specified
- `est_rba_site` - the estimated sitewide RBA, based on averaging the DU RBA estimates (`est_rba_site`)
- `DU_error` - the % relative error for each DU, comparing the sitewide RBA estimate (`est_rba_site`) to the true RBA of the DU (`DU_mn`) 
- `DU_abserror` - the absolute value of the % relative error for each DU

**'site_error.csv'** contains the summarized results for each site (i.e., simulation iteration), averaging across DUs for each site and iteration. Column meanings:

- `iter` - iteration number
- `site_error` - % relative error between estimated sitewide RBA (`est_rba_site` from 'DU_error.csv') and the "true" sitewide RBA, i.e., the mean RBA parameter used to define the site RBA distribution
- `site_abserror` - absolute value of `site_error`
- `DU_error_mean` - mean of the `DU_error`(error comparing the sitewide RBA estimate to the true RBA value of each DU) across each site
- `DU_error_lowerci` and `DU_error_upperci` - lower 2.5% and upper 97.5 intervals of `DU_error` in each site
- `DU_error_max` - maximum `DU_error` value of all the DUs in a site/iteration
- `DU_abserror_mean`, `DU_abserror_lowerci`, `DU_abserror_upperci`, `DU_abserror_max` - corresponding absolute values of the above statistics
  
**'sim_error.csv'** contains the summarized results of 'site_error.csv' across all iterations. Column meanings:

- `site_abserror_mean` - average sitewide RBA error across all iterations (`site_abserror` in 'site_error.csv')
- `DU_abserror_mean` - average of `DU_abserror_mean` values across all iterations
- `DU_abserror_lowerci` and `DU_abserror_upperci` - averages of lower and upper intervals, respectively, across all iterations
- `DU_abserror_max` - average of `DU_abserror_max` values across all iterations
