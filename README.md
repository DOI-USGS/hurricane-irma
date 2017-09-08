# hurricane-irma

Initial Setup:

Clone this repository.

Open the included `hurricane-irma.Rproj`

If you don't have it, install [`vizlab`](https://github.com/USGS-VIZLAB/vizlab)

Check to make sure you have required packages listed in `viz.yml` installed.

do:
```
library(vizlab)
createProfile()
createMakefiles()
```

`createProfile()` may not be necessary if you've worked on vizlab before.

Go to the "build" tab in Rstudio. You should see a "Build All" option. Click it!

If there are errors, logs with errors are in `.hurricane-irma/vizlab/make/log`

