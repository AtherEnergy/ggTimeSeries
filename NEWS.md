# ggTimeAnalysis Updates

Hi Aditya/Ather

Hope you don't mind but I really want to see this package uploaded to CRAN, so I took the liberty to enhance your package to remove all notes and warnings that would prevent it from being accepted.

Appreciate the work you've put into the package, but please consider merging my fork into a different branch and submitting to CRAN, everything should be ready to be uploaded on [cran.r-project.org/submit](https://cran.r-project.org/submit.html).

Please contact me if theres any concerns or issues.

## Summary of modifications

-   Cleaning up DESCRIPTION file to meet CRAN requirements
-   Updating params for all functions to ensure included in rdocumentation
-   Updating examples for all functions to ensure they work with CRAN checks
-   Updating `\links` in roxygen documentation to link to referenced packages
-   Created Vignette file `ggTimeSeries.Rmd` with content from `README.Rmd` file to be consumed on CRAN
-   Removed .RMD as CRAN does not allow it at the package level
-   Updated `README.md` image references to your github link to remove dependencies on folder
-   Updated LICENSE file to meet CRAN criteria

## CRAN check results - R 3.5.1 iMac

    * using R version 3.5.1 (2018-07-02)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using options ‘--no-manual --as-cran’
    * checking for file ‘ggTimeSeries/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘ggTimeSeries’ version ‘1.0.0’
    * package encoding: UTF-8
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    * checking if this is a source package ... OK
    * checking if there is a namespace ... OK
    * checking for executable files ... OK
    * checking for hidden files and directories ... OK
    * checking for portable file names ... OK
    * checking for sufficient/correct file permissions ... OK
    * checking serialization versions ... OK
    * checking whether package ‘ggTimeSeries’ can be installed ... OK
    * checking installed package size ... OK
    * checking package directory ... OK
    * checking ‘build’ directory ... OK
    * checking DESCRIPTION meta-information ... OK
    * checking top-level files ... OK
    * checking for left-over files ... OK
    * checking index information ... OK
    * checking package subdirectories ... OK
    * checking R files for non-ASCII characters ... OK
    * checking R files for syntax errors ... OK
    * checking whether the package can be loaded ... OK
    * checking whether the package can be loaded with stated dependencies ... OK
    * checking whether the package can be unloaded cleanly ... OK
    * checking whether the namespace can be loaded with stated dependencies ... OK
    * checking whether the namespace can be unloaded cleanly ... OK
    * checking dependencies in R code ... OK
    * checking S3 generic/method consistency ... OK
    * checking replacement functions ... OK
    * checking foreign function calls ... OK
    * checking R code for possible problems ... OK
    * checking Rd files ... OK
    * checking Rd metadata ... OK
    * checking Rd line widths ... OK
    * checking Rd cross-references ... OK
    * checking for missing documentation entries ... OK
    * checking for code/documentation mismatches ... OK
    * checking Rd \usage sections ... OK
    * checking Rd contents ... OK
    * checking for unstated dependencies in examples ... OK
    * checking contents of ‘data’ directory ... OK
    * checking data for non-ASCII characters ... OK
    * checking data for ASCII and uncompressed saves ... OK
    * checking installed files from ‘inst/doc’ ... OK
    * checking files in ‘vignettes’ ... OK
    * checking examples ... OK
    * checking for unstated dependencies in vignettes ... OK
    * checking package vignettes in ‘inst/doc’ ... OK
    * checking re-building of vignette outputs ... OK
    * DONE

    Status: OK

    R CMD check results
    0 errors | 0 warnings | 0 notes

## Win CRAN check results - R Windows

    * using log directory 'd:/RCompile/CRANguest/R-devel/ggTimeSeries.Rcheck'
    * using R Under development (unstable) (2018-07-23 r75001)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'ggTimeSeries/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'ggTimeSeries' version '1.0.0'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    New submission
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    * checking if this is a source package ... OK
    * checking if there is a namespace ... OK
    * checking for hidden files and directories ... OK
    * checking for portable file names ... OK
    * checking serialization versions ... OK
    * checking whether package 'ggTimeSeries' can be installed ... OK
    * checking installed package size ... OK
    * checking package directory ... OK
    * checking 'build' directory ... OK
    * checking DESCRIPTION meta-information ... OK
    * checking top-level files ... OK
    * checking for left-over files ... OK
    * checking index information ... OK
    * checking package subdirectories ... OK
    * checking R files for non-ASCII characters ... OK
    * checking R files for syntax errors ... OK
    * loading checks for arch 'i386'
    ** checking whether the package can be loaded ... OK
    ** checking whether the package can be loaded with stated dependencies ... OK
    ** checking whether the package can be unloaded cleanly ... OK
    ** checking whether the namespace can be loaded with stated dependencies ... OK
    ** checking whether the namespace can be unloaded cleanly ... OK
    ** checking loading without being on the library search path ... OK
    ** checking use of S3 registration ... OK
    * loading checks for arch 'x64'
    ** checking whether the package can be loaded ... OK
    ** checking whether the package can be loaded with stated dependencies ... OK
    ** checking whether the package can be unloaded cleanly ... OK
    ** checking whether the namespace can be loaded with stated dependencies ... OK
    ** checking whether the namespace can be unloaded cleanly ... OK
    ** checking loading without being on the library search path ... OK
    ** checking use of S3 registration ... OK
    * checking dependencies in R code ... OK
    * checking S3 generic/method consistency ... OK
    * checking replacement functions ... OK
    * checking foreign function calls ... OK
    * checking R code for possible problems ... [9s] OK
    * checking Rd files ... OK
    * checking Rd metadata ... OK
    * checking Rd line widths ... OK
    * checking Rd cross-references ... OK
    * checking for missing documentation entries ... OK
    * checking for code/documentation mismatches ... OK
    * checking Rd \usage sections ... OK
    * checking Rd contents ... OK
    * checking for unstated dependencies in examples ... OK
    * checking contents of 'data' directory ... OK
    * checking data for non-ASCII characters ... OK
    * checking data for ASCII and uncompressed saves ... OK
    * checking installed files from 'inst/doc' ... OK
    * checking files in 'vignettes' ... OK
    * checking examples ...
    ** running examples for arch 'i386' ... [11s] OK
    ** running examples for arch 'x64' ... [12s] OK
    * checking for unstated dependencies in vignettes ... OK
    * checking package vignettes in 'inst/doc' ... OK
    * checking re-building of vignette outputs ... [13s] OK
    * checking PDF version of manual ... OK
    * DONE
    Status: 1 NOTE New Submission
