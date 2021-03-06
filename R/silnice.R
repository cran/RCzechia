#' Road Network
#'
#' Function returning data frame of roads of the Czech Republic as \code{sf} lines. It has no obligatory parameters.
#'
#' Due to package size constraints the data are stored externally (and a working internet connection is required to use the package).
#'
#' The data is current to January 2015. Downloaded size is 1.5 MB.
#'
#' @format \code{sf} data frame with 18.979 rows of 4 variables + geometry:
#'
#' \describe{
#'   \item{TRIDA}{Class of the road: highway = dálnice, speedway = rychlostní silnice, 1st class road = silnice I. třídy, 2nd class road = silnice II. třídy, 3rd class road = silnice III. třídy, other road = neevidovaná silnice}
#'   \item{CISLO_SILNICE}{Local road code}
#'   \item{MEZINARODNI_OZNACENI}{International road code}
#' }
#'
#' @source © ArcČR, ARCDATA PRAHA, ZÚ, ČSÚ, 2016 \url{https://www.arcdata.cz/produkty/geograficka-data/arccr-4-0}
#'
#' @export

silnice <- function() {
  result <- downloader("Silnice.rds")
  result
}
