pkgs <-
  c(
    "remotes",
    "tidyverse",
    "countrycode",
    "future",
    "furrr",
    "humaniformat",
    "knitr",
    "janitor",
    "curl",
    "rvest",
    "httr",
    "jsonlite",
    "pdftools",
    "readtext",
    "quanteda",
    "phonics",
    "refinr",
    "vroom",
    "rpivotTable",
    "trelliscopejs",
    "formattable",
    "rio",
    "highcharter",
    "flexclust",
    "R.utils",
    "stopwords",
    "uwot",
    "tidylo",
    "widyr",
    "qdapRegex",
    "viridis",
    "gt",
    "hopach",
    "fpc"
  )
install.packages(pkgs)

other_package <- c("tidytext", "hrbrthemes" ,"highcharter","ggtext", "plotly", "esquisse", "tidygraph", "ggraph", "d3r", "treemap", "sunburstR", "reactable", "skimr")
install.packages(other_package)
remotes::install_github("kosukeimai/wru")
remotes::install_github("abresler/entities")
remotes::install_github("egenn/rtemis")
remotes::install_github("hrbrmstr/markdowntemplates")

install.packages(
  "https://abresler.github.io/private_r_packages/govtrackR_0.1.121.tar.gz",
  repos = NULL,
  type = "source"
)

