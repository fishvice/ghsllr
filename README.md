## Installation

```
devtools::install_github("fishvice/ghsllr")
```
## Quick demo

```
library(ghsllr)
vms_setup_directory()
vms_setup_templates()
```

Then "knit" each of the 01vms_standardize.Rmd, 02vms_rasterize.Rmd and 03vms_tracks.Rmd in sequence to generate documents (html is set as default). Short-cut for that is to run the following.
```
ghsllr:::vms_render_documents()
```
