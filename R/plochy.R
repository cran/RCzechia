#' Water Bodies
#'
#' Function returning data frame of water bodies of the Czech Republic as \code{sf} polygons. It takes no parameters.
#'
#' Due to package size constraints the data are stored externally (and a working internet connection is required to use the package).
#'
#' The data is current to January 2015. Downloaded size is <1 MB.
#'
#' @format \code{sf} data frame with 480 rows of 5 variables + geometry
#'
#' \describe{
#'   \item{TYP}{Type of water body: 1 = dam, 2 = pond, 3 = lake}
#'   \item{NAZEV}{Name, with Czech accents}
#'   \item{NAZEV_ASCII}{Name, without Czech accents}
#'   \item{VYSKA}{water level, meters above sea level}
#'   \item{Major}{Boolean indicating major water bodies}
#' }
#'
#' @source © ArcČR, ARCDATA PRAHA, ZÚ, ČSÚ, 2016 \url{https://www.arcdata.cz/produkty/geograficka-data/arccr-4-0}
#'
#' @export

plochy <- function() {
  result <- downloader("Plochy.rds")
  result
}
