#' City Parts
#'
#' Function taking no parameters and returning data frame of districts of Prague and other major cities as \code{sf} polygons.
#'
#' Due to package size constraints the data are stored externally (and a working internet connection is required to use the package).
#'
#' The data is current to June 2021. Downloaded size is 1.5 MB.
#'
#'
#' @format \code{sf} data frame with 142 rows of 4 variables + geometry
#'
#' \describe{
#'   \item{KOD}{Code of the city part / kod mestske casti}
#'   \item{NAZEV}{Name of the city part / nazev mestske casti}
#'   \item{KOD_OBEC}{Code of the city}
#'   \item{NAZ_OBEC}{Name of the city}
#' }
#'
#' @source © ČÚZK, 2021 \url{https://vdp.cuzk.cz/}
#'
#' @export

casti <- function() {
  result <- downloader("casti-R-2021-06.rds")
  result
}
