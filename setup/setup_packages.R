get_packages <- function(pkg){
  suppressWarnings({
    if (!require(pkg,character.only = T,quietly = T)) {
      install.packages(pkg)
    } else {
      message(paste0(pkg, " is already installed."))
      T
    }
  })
}

packages <- c("anytime","data.table","stringr","httr","zoo")

sapply(packages, get_packages)
