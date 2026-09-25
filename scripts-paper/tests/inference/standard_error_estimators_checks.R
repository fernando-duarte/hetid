# Checks for paper-owned SE frame assembly. Numerical helper coverage
# remains in the package and the public covariance adapter suite.

check("se frame is one row per coef, sqrt of the vcov diagonal", {
  fr <- logvar_se_frame(list(a = diag(c(4, 9)), b = diag(c(1, 16))), c("x1", "x2"))
  identical(fr$coef, c("x1", "x2")) &&
    isTRUE(all.equal(fr$a, c(2, 3))) && isTRUE(all.equal(fr$b, c(1, 4)))
})
check("se frame renders NA on a negative diagonal", {
  fr <- logvar_se_frame(list(a = diag(c(-1, 4))), c("x1", "x2"))
  is.na(fr$a[1]) && isTRUE(all.equal(fr$a[2], 2))
})
check("se na frame is all-NA with the given types", {
  fr <- logvar_se_na_frame(c("x1", "x2"), c("p", "q"))
  identical(fr$coef, c("x1", "x2")) && all(is.na(fr$p)) && all(is.na(fr$q))
})
