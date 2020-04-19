# list.of.packages <- c("rpanel", "rgl", "ks", "lattice", "misc3d", "MASS", "mvtnorm", "ellipse")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, repos="https://cran.r-project.org", dependencies = TRUE, quiet = TRUE)

# List of packages for session
.packages = c("rpanel", "rgl", "ks", "lattice", "misc3d", "MASS", "mvtnorm", "ellipse")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()[,"Package"]
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos="https://cran.r-project.org", dependencies = TRUE, quiet = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

.onAttach <- function(...) {
  packageStartupMessage(paste(paste(" **********   MLANP Version",  packageDescription("MLANP")$Version), " ********** \n")
                        ,appendLF = FALSE)
  # packageStartupMessage(paste("For more on MLANP look at",  packageDescription("gamlss")$URL))
  # packageStartupMessage("Type gamlssNews() to see new features/changes/bug fixes.\n")
  .packages = c("rpanel", "rgl", "ks", "lattice", "misc3d", "MASS", "mvtnorm", "ellipse")
  lapply(.packages, require, character.only=TRUE)
}