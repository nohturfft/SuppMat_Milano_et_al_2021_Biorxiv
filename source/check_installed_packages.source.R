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
  "data.table",
  "openxlsx",
  "enrichR",
  "gplots",
  "Hmisc",
  "superheat"
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
    message(paste("Installing:", cran_pkg))
    install.packages(cran_pkg, dependencies=TRUE)
  }
}

for (bioc_pkg in required_bioc_packages) {
  if (!bioc_pkg %in% installed_packages) {
    BiocManager::install(bioc_pkg, dependencies=TRUE)
  }
}

rm(installed_packages)