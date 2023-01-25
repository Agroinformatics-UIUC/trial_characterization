# install.packages('usethis')
devtools::install_github("r-lib/usethis")
install.packages('curl', 'git2r', 'yaml')
library(usethis)
library(glue)

use_git_config(user.name = "germanmandrini", user.email = "germanm2@illinois.edu")
use_git_config()

Sys.getenv("GITHUB_PAT")

devtools::install_github(repo = "rodrigoagronomia/DIFMR")

library(DIFMR)
