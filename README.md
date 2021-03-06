# What is this
This code conducts estimation and inference for Local Modified Moran's I. Local Modified Moran's I is a local indicator of spatial association (LISA), derived from Jackson et al's (2010) Modified Moran's I and Anselin's (1995) Local Moran's I.

This new test statistic corrects for common problems with LISAs, includinglack of a clear null distribution, population heterogeneity, multiple testing issues, fixed neighborhood sizes and spatial correlation of test statistics.

The code is mainly complete, but still needs to apply Tango's (2000) correction multiple testing for Moran's I in the LISA context.

# Inputs
Data file of regional rates and coordinate data for each corresponding region

# Outputs
CSV of original data and p-values (likelihood of being a cluster) under the Local Modified Moran's I test

# File list
*LISA_paper.pdf* is the working paper for this research. We submitted it to *Spatial Statistics* in Jan 2017, and it is still under review as of Oct 2017.

*CODE_LISA_Simulations.R* is the main code file. It estimates and conducts inference for the Local Modified Moran's I.

*CODE_Null.R* generates the null data (no spatial pattern). We use county data in this example. 

Several files generate the alternative county data, based on various spatial patterns. *CODE_Linear Trend_heterog.R* generates data with a linear trend (left-right global trend) under heterogeneous county populations. *CODE_Linear Trend_homog.R* does the same, for homogeneous county populations. *CODE_Local Clusters_heterog.R* generates a local cluster, under heterogeneous populations, and *CODE_Local Clusters_homog.R* under homogeneous populations. The files generate clusters of various size, from 5-30% of all U.S. counties. *CODE_Spiral Trend.R* generates a spiral trend.

*CODE_Plot_Clusters.R* plots the local clusters generated by *CODE_Local Clusters_heterog.R* and *CODE_Local Clusters_homog.R*. The plots are saved as *PLOT_...*.

# Note
Code calls upon R libraries "snow" and "snowfall" for parallel computing.

# References
Jackson MC, Huang L, Xie Q, Tiwari RC. (2010). A modified version of Moran’s I. International Journal of Health Geography, 9:33.

Anselin, L. (1995). Local indicators of spatial association – LISA. Geographical Analysis, 27, 93-115.
