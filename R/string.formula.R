#' Model in the formula object converted to a character
#'
#'
#' @param formula formula object
#' @return character. model
#' 
#' @examples
#' aformula <- as.formula(medv ~ lstat)
#' astring <- string.formula(aformula)
#' print(astring)
#' 
#' @importFrom stats terms
#' @export
string.formula <- function(formula) {
  fterms <- as.character(formula(terms(formula)))
  outc <- fterms[2]
  mod.part <- fterms[3]
  mod <- paste(outc, "~", mod.part)
  return(mod)
}
