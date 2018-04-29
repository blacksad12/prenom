library(data.table)

#-############################################################################-
#' Get data
#'
#' @param genre 
#' @param minGiven 
#' @param minYear 
#'
#' @return
#' @export
#'
#' @examples data.get("f", 30, 1980, c(2000, 2016), 5)
data.get <- function(genre, minGiven, minYear) {
  ## Get Etat-Civil data
  prenomsDt <- data.getEtatCivil()
  
  ## Filter by genre
  this.genre <- genre
  prenomsDt <- prenomsDt[genre == this.genre]
  
  ## Remove if never given more than x times per year
  prenomsDt <- data.filterByMinGiven(prenomsDt, minGiven)
  
  ## Remove if never given before year X
  prenomsDt <- data.filterExistingBefore(prenomsDt, minYear)
  
  ## Filter by popularity
  prenomsDt <- data.filterByPopular(prenomsDt, c(2010, 2020), 5)
  prenomsDt <- data.filterByPopular(prenomsDt, c(2000, 2010), 5)
  prenomsDt <- data.filterByPopular(prenomsDt, c(1990, 2000), 5)
  prenomsDt <- data.filterByPopular(prenomsDt, c(1980, 1990), 5)
  prenomsDt <- data.filterByPopular(prenomsDt, c(1970, 1980), 5)
  prenomsDt <- data.filterByPopular(prenomsDt, c(1960, 1970), 5)
  
  ## Filter by origin
  excludeOrigin <- list(doesNotContain = c("arabic"),
                        isNotOnly = c("english"))
  prenomsDt <- data.filterByOrigin(prenomsDt, excludeOrigin)
  
  return(prenomsDt)
}

#-############################################################################-
#' Title
#'
#' @param genre 
#'
#' @return
#' @export
#'
#' @examples data.getEtatCivil("f")
data.getEtatCivil <- function(genre) {
  ## Read dat from file
  prenomsDt <- fread("data/nat2016.txt", encoding = "Latin-1")
  
  ## Re-format to common standard
  prenomsDt <- prenomsDt[, preusuel := tolower(preusuel)]
  prenomsDt <- prenomsDt[, .(prenom = preusuel, sexe, annee = annais, nombre)]
  prenomsDt <- prenomsDt[sexe == 1, genre := "m"]
  prenomsDt <- prenomsDt[sexe == 2, genre := "f"]
  prenomsDt <- prenomsDt[, sexe := NULL]
  
  ## Remove 'XXXX' years
  prenomsDt <- prenomsDt[annee != "XXXX"]
  
  ## Remove composed prenom
  prenomsDt <- prenomsDt[!grepl("-", prenom)]
  
  
  return(prenomsDt)
}

#-############################################################################-
#' Remove prenom which were frequent within a timeframe
#'
#' @param prenomsDt 
#' @param freqDateRange 
#' @param maxFrequency 
#'
#' @return
#' @export
#'
#' @examples \dontrun{"NO EXAMPLE"}
data.filterByPopular <- function(prenomsDt, freqDateRange, maxFrequency) {
  print(paste0("Removing if prenom is given more than ", maxFrequency, "/1000 times between ", freqDateRange[1], " and ", freqDateRange[2]))
  numberBefore <- length(unique(prenomsDt$prenom))
  
  popularDt <- prenomsDt[annee >= freqDateRange[1] & annee <= freqDateRange[2]]
  totalBirth <- sum(popularDt$nombre)
  popularDt <- popularDt[, .(frequence = (sum(nombre) / totalBirth) * 1000), by = prenom]
  
  prenomsDt <- merge(prenomsDt, popularDt[frequence <= maxFrequency, .(prenom)])
  
  numberAfter <- length(unique(prenomsDt$prenom))
  print(paste0("Before: ", numberBefore, " - After: ", numberAfter, " (", numberBefore-numberAfter, " removed)"))
  return(prenomsDt)
}

#-############################################################################-
#' Keep only prenom which were given before \code{year}
#'
#' @param prenomsDt 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples \dontrun{"NO EXAMPLE"}
data.filterExistingBefore <- function(prenomsDt, year) {
  numberBefore <- length(unique(prenomsDt$prenom))
  
  prenomsDt <- prenomsDt[, minYear := min(annee), by = prenom]
  prenomsDt <- prenomsDt[minYear <= year]
  prenomsDt <- prenomsDt[, minYear := NULL]
  
  numberAfter <- length(unique(prenomsDt$prenom))
  print(paste0("Before: ", numberBefore, " - After: ", numberAfter, " (", numberBefore-numberAfter, " removed)"))
  return(prenomsDt)
}

#-############################################################################-
#' Keep only prenom which were given more than \code{minGiven} times, at least 1 year
#'
#' @param prenomsDt 
#' @param minGiven 
#'
#' @return
#' @export
#'
#' @examples \dontrun{"NO EXAMPLE"}
data.filterByMinGiven <- function(prenomsDt, minGiven) {
  numberBefore <- length(unique(prenomsDt$prenom))
  
  prenomsDt <- prenomsDt[, maxNombre := max(nombre), by = prenom]
  prenomsDt <- prenomsDt[maxNombre >= minGiven]
  prenomsDt <- prenomsDt[, maxNombre := NULL]
  
  numberAfter <- length(unique(prenomsDt$prenom))
  print(paste0("Before: ", numberBefore, " - After: ", numberAfter, " (", numberBefore-numberAfter, " removed)"))
  return(prenomsDt)
}

data.filterByOrigin <- function(prenomsDt, excludeOrigins) {
  numberBefore <- length(unique(prenomsDt$prenom))
  
  origineDt <- data.getOrigine()
  
  prenomsDt <- merge(x = prenomsDt, 
                     y = origineDt[, .(prenom, langage)],
                     by = "prenom", 
                     all.x = TRUE)
  
  ## Should not contain
  excludePatern <- paste(excludeOrigins$doesNotContain, collapse ="|")
  prenomsDt <- prenomsDt[!grepl(excludePatern, langage)]
  
  ## Is not only
  for (origin in excludeOrigins$isNotOnly) {
    prenomsDt <- prenomsDt[langage != origin]
  }
  
  numberAfter <- length(unique(prenomsDt$prenom))
  print(paste0("Before: ", numberBefore, " - After: ", numberAfter, " (", numberBefore-numberAfter, " removed)"))
  return(prenomsDt)
}

data.getOrigine <- function() {
  ## Read dat from file
  prenomsDt <- fread("data/prenoms.csv", encoding = "Latin-1")
  
  ## Re-format to common standard
  prenomsDt <- prenomsDt[, .(prenom = `01_prenom`, 
                             genre = `02_genre`, 
                             langage = `03_langage`)]
  
  return(prenomsDt)
}

other <- function() {
  ## Merge
  prenomsDt <- merge(prenomsFrequenceDt, prenomsOrigineDt,
                     by = "prenom", all = TRUE)
  
  print(paste("Total :", nrow(unique(prenomsDt[, .(prenom)])), "prénoms"))
  
  origines <- unique(prenomsOrigineDt$langage)
  
  selection <- unique(prenomsFrequenceDt[, .(prenom, frequence)])
  selection <- prenomsDt[is.na(langage)]
  selection <- prenomsDt[grepl("french", langage)]
  selectionShort <- unique(prenomsDt[genre.y == "f", .(prenom)])
  
}
