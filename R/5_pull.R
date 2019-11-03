# lowest-level functions for obtaining Web data
# these ultimately wrap RCurl::getURL with useful options and loops for retrying on failure

#' Don't Repeat Yourself when pulling JSON
#' @inheritParams ds.pull_URL
#' @inheritParams ds.get_pbp
#' @param clean logical. Clean certain special characters from the JSON? Default \code{FALSE}.
#' @return a JSON object or \code{NULL}
#' @export
ds.pull_JSON <- function(url, agents, try_tolerance = 3, ref="nhl.com", clean=FALSE) {
  raw_text   <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    raw_text      <- try(ds.pull_URL(url, agents, ref))
    raw_text      <- if(clean) {gsub("^.+?\\(\\{", "\\{", raw_text)} else {raw_text}
    json_check    <- try(rjson::fromJSON(raw_text))
    try_tolerance <- try_tolerance - 1
  }
  
  raw_text <- if(clean) {gsub("^.+?\\(\\{", "\\{", raw_text)} else {raw_text}
  raw_json <- try(rjson::fromJSON(raw_text))
  
  if(class(raw_json) == "try-error") {NULL} else {raw_json}
}

#' Don't Repeat Yourself when pulling text
#' @inheritParams ds.pull_JSON
#' @return a nice HTML thingy or \code{NULL}
#' @export
ds.pull_text <- function(url, agents, try_tolerance = 3, ref="nhl.com") {
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    raw_text      <- try(ds.pull_URL(url, agents, ref))
    try_tolerance <- try_tolerance - 1
  }
  
  raw_text
}


#' Don't Repeat Yourself
#'
#' \code{pull_URL()} is a one-line wrapper around \code{RCurl::getURL()} with repeatedly used options
#' @param url    character.        A URL.
#' @param agents character vector. One or more HTTP user agent strings.
#' @param ref    character.        A referer. Default \code{"nhl.com"}.
#' @return output of \code{RCurl::\link[RCurl]{getURL}}.
#' @export
ds.pull_URL <- function(url, agents, ref="nhl.com") {
  RCurl::getURL(url, header = FALSE, .opts = RCurl::curlOptions(referer = ref,
                                                                verbose = FALSE,
                                                                followLocation = TRUE,
                                                                useragent = agents[sample(1:length(agents), 1)])
  )
}

