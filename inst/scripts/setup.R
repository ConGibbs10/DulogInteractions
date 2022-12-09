# check for dependencies
inpkgs <- installed.packages()[, "Package"]
if (!'renv' %in% inpkgs) {
  install.packages('renv')
}
deppkgs <-
  setdiff(unique(renv::dependencies()$Package),
          c('R', 'DulogInteractions'))
nepkgs <- deppkgs[!deppkgs %in% inpkgs]
if (length(nepkgs) > 0) {
  install.packages(nepkgs)
}

# set up directories for the raw dulog data
if(!dir.exists('dulogs-raw') || !dir.exists('dulogs-raw/2021') || !dir.exists('dulogs-raw/2022')) {
  stop('Add dulogs-raw to project directory.')
}

# check if folder is zipped
is_zipped <- function(filepath){
  result <- tryCatch({
    unzip(filepath, list = TRUE)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
  return(result)
}
if(is_zipped('dulogs-raw')) {
  stop('Unzip dulogs-raw.')
}

# if data are in place prepare and write to dulogs folder
if(!dir.exists('dulogs/2021')) {
  source('dulogs/prepare_2021.R')
}
if(!dir.exists('dulogs/2022')){
  source('dulogs/prepare_2022.R')
}

# check that files are written
if(!dir.exists('dulogs/2021') || !dir.exists('dulogs/2022')) {
  stop('Unforeseen error. Contact Connor Gibbs at congibbs10@gmail.com.')
}

# install package
devtools::install()

