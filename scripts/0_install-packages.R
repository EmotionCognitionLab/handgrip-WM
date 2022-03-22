# install R packages needed for data preprocessing
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


## list of packages
packages <- c('here', 'data.table', 'dplyr', 'stringr',
              'lubridate', 'RHRV', 'ggplot2', 'fuzzyjoin',
              'tidyr', 'gazer', 'zoo')

## install packages that are not installed
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

# install gazer and saccades packages from github
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!'saccades' %in% rownames(installed.packages())) devtools::install_github("tmalsburg/saccades/saccades", dependencies=TRUE)
if (!'gazer' %in% rownames(installed.packages())) remotes::install_github("dmirman/gazer")