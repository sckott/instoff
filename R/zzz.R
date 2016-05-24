cp <- function(x) Filter(Negate(is.null), x)

last <- function(x) x[length(x)]

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
