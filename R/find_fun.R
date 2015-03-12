#' Finds the name of the package that a function is from.
#' 
#' Uses Hadley's "where" from pryr, but gets just the name of the package and
#' can handle passing in a function instead of just a character string.
#' Also can handle a string that includes the function name (e.g., 'paris::find_fun' or 'paris/find_fun')
#'
#' @param fun function or character. The function to find the name of the package for.
#' @export
find_fun <- function(fun) {
  fun <- deparse(substitute(fun))
  for(char in c('/', ':::', '::')) {
    if (grepl(char, fun)) return(strsplit(fun, char)[[1]][[1]])
  }
  fun %>% pryr::where(.) %>% attr(., 'name') %>% strsplit(., ':') %>% .[[1]] %>% .[[2]]
}
