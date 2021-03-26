installed_packages <- rownames(utils::installed.packages())
required_cran_packages <- c(
  "rmarkdown",
  "knitr",
  "BiocManager",
  "magrittr",
  "dplyr",
  "details",
  "rstudioapi",
  "pryr",
  "DT",
  "RColorBrewer",
  "scales",
  "devtools",
  "tibble",
  "gridExtra",
  "data.table"
)

required_bioc_packages <- c(
  "BiocGenerics",
  "Biobase",
  "IRanges",
  "S4Vectors",
  "AnnotationDbi",
  "affy",
  "mouse430a2.db"
)

for (cran_pkg in required_cran_packages) {
  if (!cran_pkg %in% installed_packages) {
    install.packages(pkg, dependencies=TRUE)
  }
}

for (bioc_pkg in required_bioc_packages) {
  if (!bioc_pkg %in% installed_packages) {
    install.packages(pkg, dependencies=TRUE)
  }
}

rm(installed_packages)