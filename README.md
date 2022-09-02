# hurricane-irma

Code for the [Hurricane Irma visualization](https://owi.usgs.gov/vizlab/hurricane-irma/).

## Initial Setup:

Clone this repository.

Open the included `hurricane-irma.Rproj`

If you don't have it, install [`vizlab`](https://github.com/USGS-VIZLAB/vizlab)

Check to make sure you have required packages listed in `viz.yaml` installed. To get the `svglite` package, you will need to install it from GitHub using `devtools::install_github("jread-usgs/svglite@svgnano")`. For Windows users, you might see an error similar to `Error: running command '"C:/PROGRA~1/R/R-34~1.1/bin/x64/R" --no-site-file --no-environ --no-save --no-restore --quiet CMD config CC' had status 127`. If this is the case, clone [Jordan Read's repository](https://github.com/jread-usgs/svglite) and build the package locally on the branch `svgnano`.

do:
```r
library(vizlab)
createProfile()
createMakefiles()
```

then make sure you have the proper packages and versions installed with:
```r
vizlab::checkVizPackages()
```
and use `install.packages()` or `devtools::install_github()` to update

`createProfile()` may not be necessary if you've worked on vizlab before.

Go to the "build" tab in Rstudio. You should see a "Build All" option. Click it!

If there are errors, logs with errors are in `.hurricane-irma/vizlab/make/log`

Once you are able to complete the build, open `target/index.html` to see the finished product locally.

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


[
  ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
](http://creativecommons.org/publicdomain/zero/1.0/)
