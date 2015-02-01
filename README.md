`spcaHelper` is a small package containing two function to help with the processing of files, when conducting a sPCA analysis in the `adegenet` package.

Specifically, the two functions allow users to calculate a spatial weights matrix between site along a landscape. By passing a raster of a river network, the `distCalc` function makes it possible to calculate river distance, rather than the default Euclidean distances.

The package is only available from github, and can be installed using the `devtools` package as follows:

```r
devtools::install_github("kkeenan02/spcaHelper")
```