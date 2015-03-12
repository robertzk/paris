#' Finds the name of the package that a function is from.
#' 
#' Uses Hadley's "where" from pryr, but gets just the name of the package and
#' can handle passing in a function instead of just a character string.
#' Also can handle a string that includes the function name (e.g., 'paris::find_fun' or 'paris/find_fun')
#'
#' @param fun function or character. The function to find the name of the package for.
#' @export
find_fun <- function(fun) {
  if (is.character(fun)) eval(parse(text = fun))  # Make sure function exists.
  else fun <- deparse(substitute(fun))

  for(char in c('/', ':::', '::')) {
    if (grepl(char, fun)) return(strsplit(fun, char)[[1]][[1]])
  }

  fun_where <- fun %>% pryr::where()
  base <- pryr::where('mean')
  if (identical(fun_where, base)) { return('base') }
  fun_where %>% attr(., 'name') %>% strsplit(., ':') %>% .[[1]] %>% .[[2]]
}
