#' pick one version of certain guys' names and stick with it
#' @param name_vector a vector of names
#' @return a vector of the same length but with each individual allowed only one version of their name
#' @export
ds.mung_names <- function(name_vector) {
  name_vector <- stringr::str_replace(name_vector, "MICHAEL.RUPP",           "MIKE.RUPP")
  name_vector <- stringr::str_replace(name_vector, "DANNY.BRIERE",           "DANIEL.BRIERE")
  name_vector <- stringr::str_replace(name_vector, "RJ.UMBERGER",            "R.J..UMBERGER")
  name_vector <- stringr::str_replace(name_vector, "MICHAEL.GRIER",          "MIKE.GRIER")
  name_vector <- stringr::str_replace(name_vector, "MICHAEL.VERNACE",        "MIKE.VERNACE")
  name_vector <- stringr::str_replace(name_vector, "STEVEN.REINPRECHT",      "STEVE.REINPRECHT")
  name_vector <- stringr::str_replace(name_vector, "OLIVIER.MAGNAN-GRENIER", "OLIVIER.MAGNAN")
  name_vector <- stringr::str_replace(name_vector, "DANNY.CLEARY",           "DAN.CLEARY")
  name_vector <- stringr::str_replace(name_vector, "BRADLEY.MILLS",          "BRAD.MILLS")
  name_vector <- stringr::str_replace(name_vector, "NICHOLAS.DRAZENOVIC",    "NICK.DRAZENOVIC")
  name_vector <- stringr::str_replace(name_vector, "TJ.HENSICK",             "T.J..HENSICK")
  name_vector <- stringr::str_replace(name_vector, "WILLIAM.THOMAS",         "BILL.THOMAS")
  name_vector <- stringr::str_replace(name_vector, "THOMAS.MCCOLLUM",        "TOM.MCCOLLUM")
  name_vector <- stringr::str_replace(name_vector, "PATRICK.MAROON",         "PAT.MAROON")
  name_vector <- stringr::str_replace(name_vector, "JAMES.WYMAN",            "JT.WYMAN")
  name_vector <- stringr::str_replace(name_vector, "MATTHEW.IRWIN",          "MATT.IRWIN")
  name_vector <- stringr::str_replace(name_vector, "MAXWELL.REINHART",       "MAX.REINHART")
  name_vector <- stringr::str_replace(name_vector, "MATTHEW.NIETO",          "MATT.NIETO")
  name_vector <- stringr::str_replace(name_vector, "MICHA\\xcbL.BOURNIVAL",  "MICHAEL.BOURNIVAL") # sic
  name_vector <- stringr::str_replace(name_vector, "MATHEW.DUMBA",           "MATT.DUMBA")
  name_vector <- stringr::str_replace(name_vector, "MARTY.HAVLAT",           "MARTIN.HAVLAT")
  name_vector <- stringr::str_replace(name_vector, "PHILIP.VARONE",          "PHIL.VARONE")
  name_vector <- stringr::str_replace(name_vector, "PIERRE-ALEX.PARENTEAU",  "P.A..PARENTEAU")
  name_vector <- stringr::str_replace(name_vector, "JEAN-FRANCOIS.BERUBE",   "J-F.BERUBE")
  name_vector <- stringr::str_replace(name_vector, "J.F..BERUBE",            "J-F.BERUBE")
  name_vector <- stringr::str_replace(name_vector, "VINCENT.HINOSTROZA",     "VINNIE.HINOSTROZA")
  name_vector <- stringr::str_replace(name_vector, "MATTHEW.MURRAY",         "MATT.MURRAY")
  name_vector <- stringr::str_replace(name_vector, "MICHAEL.MATHESON",       "MIKE.MATHESON")
  name_vector <- stringr::str_replace(name_vector, "JOSHUA.MORRISSEY",       "JOSH.MORRISSEY")
  name_vector <- stringr::str_replace(name_vector, "ZACHARY.SANFORD",        "ZACH.SANFORD")
  name_vector <- stringr::str_replace(name_vector, "NICHOLAS.BAPTISTE",      "NICK.BAPTISTE")
  name_vector <- stringr::str_replace(name_vector, "ANTHONY.DEANGELO",       "TONY.DEANGELO")
  name_vector <- stringr::str_replace(name_vector, "AJ.GREER",               "A.J..GREER")
  name_vector <- stringr::str_replace(name_vector, "CRISTOVAL.NIEVES",       "BOO.NIEVES")
  name_vector <- stringr::str_replace(name_vector, "DANIEL.O'REGAN",         "DANNY.O'REGAN")
  name_vector <- stringr::str_replace(name_vector, "JT.COMPHER",             "J.T..COMPHER")
  name_vector <- stringr::str_replace(name_vector, "ZACHARY.ASTON-REESE",    "ZACH.ASTON-REESE")
  name_vector <- stringr::str_replace(name_vector, "CALVIN.PETERSEN",        "CAL.PETERSEN")
  name_vector <- stringr::str_replace(name_vector, "QUINTIN.HUGHES",         "QUINN.HUGHES")
  name_vector <- stringr::str_replace(name_vector, "TIMOTHY.GETTINGER",      "TIM.GETTINGER")

  name_vector
}
