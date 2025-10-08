# bioavailability

### Sitewide RBA error estimation of As

App is hosted at: <https://kwnli.shinyapps.io/bioavailability_sitewide/>

This app creates three csv outputs to download:

**'out1_DU_values.csv'** contains the true RBA value for every DU across the site, over all simulation iterations. These values are true values only, no sampling was simulated. Each row is a separate DU.

-   `sim.num` - iteration number
-   `tru_DU_rba` - the (true) RBA for the DU
-   `est_rba_site` - the estimated sitewide RBA, based on the **samples** taken in the *samples.csv* file for the corresponding `sim.num`.
-   `DU_error_siteRBA` - the error for each DU, comparing the true DU RBA (`DU_rba`) to the sitewide RBA estimate based on **samples** (from *samples.csv*). `DU_abserror_siteRBA` is the absolute version of this.

**'out2_samples.csv'** contains the simulated sample calculation intermediate values and results.

-   `sim.num` - iteration number
-   `sample.num` - the sample number. If at some point we want to see actual increment values, this is used to keep track of which increment belongs to which sample.
-   `tru.rba` - the (true) RBA for the sample. If the sample is made up of increments, this is the average of the increment true RBA values.
-   `tru.ivb` - the `tru.ivb` value run through the IVBA model. Currently, we are not adding in model error.
-   `meas.ivb` - measured IVBA, adding in measurement error based on the coefficient of variation for measurement error.
-   `est_rba_sample` - the estimated RBA for the sample, by running the `meas.ivb` value back through the IVBA model.
-   `est_rba_site` - the sample-estimated site RBA value. Calculated by averaging `est_rba_sample` across the simulation iteration indicated by `sim.num`. This value matches the column with the same name in the *DUvalues.csv* file.

**'out3_site_error.csv'** contains the summarized results for each site (i.e., simulation iteration), averaging across DU errors for each site/iteration.

-   `sim.num` - simulation iteration number
-   `est_rba_site` - the sample-estimated site RBA value
-   `DU_error_siteRBA_mean` - the mean across the error for each DU, calculated by comparing the true DU RBA to the sitewide RBA estimate (`est_rba_site`). `DU_abserror_siteRBA_mean` is the mean of the absolute values of these errors (absolute value taken over the DU values, not the mean value).
-   `siteRBA_error` - error (difference) between estimated sitewide RBA (`est_rba_site`) and the "true" sitewide RBA, i.e., the mean RBA parameter used to define the site RBA distribution
-   `siteRBA_abserror` - absolute value of `siteRBA_error`

**'out4_sim_error.csv'** contains the summarized results of 'site_error.csv' across all iterations. Column meanings:

-   `siteRBA_abserror_mean` - average sitewide RBA absolute error across all iterations (`siteRBA_abserror` in 'site_error.csv')
-   `siteRBA_abserror_lowerci` and `siteRBA_abserror_upperci` - lower and upper intervals of site absolute error, respectively, across all iterations
-   `siteRBA_abserror_max` - maximum of sitewide RBA absolute error values across all iterations
-   `DU_abserror_siteRBA_mean` - mean across all simulation iterations of the `DU_abserror_siteRBA_mean` (within-site mean of the error comparing the DU RBA estimate to the sitewide RBA) across each site
-   `DU_abserror_siteRBA_lowerci` and `DU_abserror_siteRBA_upperci` - lower 2.5% and upper 97.5 intervals of `DU_abserror_siteRBA_mean` in each site
-   `DU_abserror_siteRBA_max` - maximum `DU_abserror_siteRBA_mean` value of all the DUs in a site/iteration
