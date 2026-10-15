

getSymAndString <- function(colVar){
  colSym <- ensym(colVar)
  colName <- as.character(colSym)
  return(
    list(
      "symbol" = colSym,
      "string" = colName
    )
  )
}

selectIfExists <- function(
  df,
  colVar
){
  var <- enquo(colVar)
  info <- getSymAndString(!! var)
  info$string
  df %>%
    select_if(
      info$string == names(.)
    )
}
# AcuteStress_off %>%
#   selectIfExists(
#     mouseID
#   )

selectAllIfExist <- function(
    df,
    colVars #c()
){
  df %>%
    select(
      any_of(colVars)
    )
}


#' Demonstrate one approach to getting the name of an unquoted expression
#' Don't use this function within another function, though, just use
#' as.character and substitute
#' makeExprCharacter
#'
#' @param var - an unquoted expression, such as a column name
#'
#' @return a string of the column name
#'
#' @examples makeExprCharacter(mouseID) -> "mouseID"
#' Note - this can't be used as is within another function
#' That takes a variable name as a parameter without being quoted
#' for example, testFunc <- function(df, colVar){
#' newString <- makeExprCharacter(colVar)
#' } doesn't work
#' 
#' Could also use all.names()
makeExprCharacter <- function (var){
  charString <- as.character(substitute(var))
  return(charString)
}


getValWhereOtherValTrue <- function(
    df
    , matchCol
    , matchVal
    , valCol
){
  matchColText <- as.character(matchCol)
  valColText <- as.character(valCol)
  val <- df[[valColText]][which(df[matchColText] == matchVal)]
  return(val)
}

# 10-color Okabe-Ito palette (colorblind-safe) used for slice-level points
slice_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#FF0099", "#33FF33", "#FF3300"
)

# 12-color expanded slice palette, fully colorblind-safe.
# First 7 are canonical Okabe-Ito (matches slots 1-7 of slice_colors above);
# slots 8-12 replace the non-CB-safe entries (#FF0099, #33FF33, #FF3300) and
# extend with Tol-derived hues for additional distinct colors. All 12 are
# distinguishable under protanopia, deuteranopia, and tritanopia.
#
# References:
#   Okabe & Ito 2008  https://jfly.uni-koeln.de/color/
#   Tol 2021          https://personal.sron.nl/~pault/
slice_colors_v2 <- c(
  "#E69F00",  # Okabe-Ito orange
  "#56B4E9",  # Okabe-Ito sky blue
  "#009E73",  # Okabe-Ito bluish green
  "#F0E442",  # Okabe-Ito yellow
  "#0072B2",  # Okabe-Ito blue
  "#D55E00",  # Okabe-Ito vermillion
  "#CC79A7",  # Okabe-Ito reddish purple
  "#999999",  # Okabe-Ito gray
  "#882255",  # Tol wine
  "#332288",  # Tol indigo
  "#117733",  # Tol forest
  "#88CCEE"   # Tol pale cyan
)

# Companion shape palette for color × shape disambiguation.
# Used together with slice_colors_v2 when the visible slice count exceeds
# the color palette size (12). All filled shapes for readability at small
# dot sizes (typical 0.75 pt). The mapping convention is: slice index i
# (0-based) gets color (i %% 12) and shape (i %/% 12), so shape variation
# only kicks in once a panel has more than 12 slices.
slice_shapes_v2 <- c(
  16,   # filled circle  — primary, slices 1–12
  17,   # filled triangle — slices 13–24
  15,   # filled square   — slices 25–36
  18    # filled diamond  — slices 37+
)
