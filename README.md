# What cruisePlanning provides

The primary motivation to develop this package was to centralize and generalize the functionality of the original cruise planning script that was used to plan countless Fisheries and Oceans Canada Atlantic Zone Monitoring Program. At it's core, it does simple calculations to estimate the total time it would take to complete operations at a given set of stations. This helps Principal Investigators and/or Chief Scientists plan a mission, both prior and while at sea, by allowing them to run various scenarios to best utilize the allotted amount of ship time provided for them to complete their program.

# How to download

This is currently a provisional package, so it is only available on github. To download :

```
library(devtools)
install_github('clayton33/cruisePlanning', ref = 'main', build_vignettes = TRUE)
```

# General information

A comprehensive vignette has been written. To access, in an `R Console`

```
library(cruisePlanning)
browseVignettes("cruisePlanning")
```

this will open up a local browser, click on the `HTML` link.
