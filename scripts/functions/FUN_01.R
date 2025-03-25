# Libraries & functions ----
## libraries ----
pacman::p_load(update = T,  
               equatiomatic, beepr, tictoc, 
               tidyverse, purrr, furrr, easystats, rio, janitor, ggthemes, car,
               gtsummary, skimr, sjPlot, flextable, ggpubr, rstatix, tidymodels
               
)
## function specification ----
conflicted::conflicts_prefer(
  janitor::remove_empty,
  dplyr::filter,
  dplyr::mutate,
  dplyr::rename,
  dplyr::summarize,
  dplyr::summarise,
  dplyr::select,
  purrr::map,
  tidyr::extract,
  stats::chisq.test,
  base::intersect,
  base::setdiff,
  dplyr::last,
  dplyr::first, 
  dplyr::between
)

## Conflicted functions ----
ls_conflicted_01 <- conflicted::conflict_scout()

# Options ----
furrr_options(seed = TRUE,
              scheduling = Inf)
options(knitr.kable.NA = '') # empty space in cells with NAs in kable

## Possibly ----


## Number of cores ----
ncores <- availableCores() -1

## Okabe & Ito palette - colorblind palette ----
pal_okabe_ito <- colorblind_pal()(8)[2:8] # ggthemes
median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 50000)
  bb <- boot.ci(bt, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

# Tables ----
## skimr ----
my_skim <- skimr::skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE),
                                          mad = ~ mad(., na.rm = TRUE)), 
                            append = T)


# Others ----