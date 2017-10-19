# What is this
This code conducts estimation and inference for Local Modified Moran's I. Local Modified Moran's I is a local indicator of spatial association (LISA), derived from Jackson et al's (2010) Modified Moran's I and Anselin's (1995) Local Moran's I.

This new test statistic corrects for common problems with LISAs, includinglack of a clear null distribution, population heterogeneity, multiple testing issues, fixed neighborhood sizes and spatial correlation of test statistics.

The code is mainly complete, but still needs to apply Tango's (2000) correction multiple testing for Moran's I in the LISA context.

# Inputs
Data file of regional rates and coordinate data for each corresponding region
# Outputs
CSV of original data and p-values (likelihood of being a cluster) under the Local Modified Moran's I test

# Note
Code calls upon R libraries "snow" and "snowfall" for parallel computing.

# References
Jackson MC, Huang L, Xie Q, Tiwari RC. (2010). A modified version of Moran’s I. International Journal of Health Geography, 9:33.

Anselin, L. (1995). Local indicators of spatial association – LISA. Geographical Analysis, 27, 93-115.
