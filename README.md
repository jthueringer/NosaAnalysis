NosaAnalysis
------------------

The NosaAnalysis package is specifically designed to provide the results of the [NOSA software tool](https://github.com/DavideR2020/NOSA) as R objects.
Various analysis methods for the (paired) input data are provided.

NOSA analysis should be used with care. It will not check every user defined settings for validity and may crash.

# Running NosaAnalysis from source code

To run this package from source code, the following prerequisites are needed:

```
    cubature,
    dplyr,
    gdata,
    ggplot2 (>= 3.3.5),
    ggpubr (>= 0.4.0),
    knitr (>= 1.10),
    latex2exp,
    methods,
    openxlsx,
    plotrix (>= 3.8-2),
    readxl,
    rlang,
    rmarkdown,
    tidyr,
    yaml
```

> For the code blocks below: Run **each line** separately in your R console, i.e. do not copy and paste the whole block.
> If an error should occur, this allows to track it down more easily.

```
## R
if (!require(devtools, quietly = TRUE)) install.packages("devtools")
library("devtools")

install_github("jthueringer/NosaAnalysis", dependencies = TRUE, vignettes = TRUE)
library("NosaAnalysis")
```

# Usage

All analysis settings are made via a YAML file. A corresponding file with all possible default values can be written to disk via the following command:
```
## R
writeDefaultYaml('path/filename.yaml')
```

After customizing the YAML file, the analysis is performed using:
```
## R
performAnalysis('path/filename.yaml')
```

A more detailed description of the setting options, as well as sample analyses, are provided with vignettes. See

```
vignette("BasicGuide", package = "NosaAnalysis")
vignette("Auc", package = "NosaAnalysis")
vignette("PeakCount", package = "NosaAnalysis")
vignette("Responses", package = "NosaAnalysis")
vignette("SEM", package = "NosaAnalysis")
vignette("TimeSlots", package = "NosaAnalysis")
vignette("Trace", package = "NosaAnalysis")
```

