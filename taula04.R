#Quarta taula (3 cohorts, demogràfica)

#Crear còpia
RC_RSV_descriptiu <- RC_3

#Transformació de tipus de variable + etiquetes
RC_RSV_descriptiu$months_old <- round(RC_3$months_old, 0)
RC_RSV_descriptiu$month_of_birth <- factor(RC_3$month_of_birth)
RC_RSV_descriptiu$sex <- factor(RC_3$sex, levels = c(1,2), labels = c("male","female"))
RC_RSV_descriptiu$region <- factor(RC_3$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_RSV_descriptiu$ethnic <- factor(RC_3$ethnic, levels = c(1,2,3,4,5,6), labels = c("white/caucasian", "black/african/caribbean","hispanic/latino","asian","pacific islander","other/mixed"))
RC_RSV_descriptiu$kindergarten <- factor(RC_3$kindergarten, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$breast_binary <- factor(RC_3$breast_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$space_living <- factor(RC_3$space_living, levels = c(1,2,3,4), labels = c("house (with other family members)", "shelter center", "community space with other families", "other"))
RC_RSV_descriptiu$smokers_home <- factor(RC_3$smokers_home, levels = c(1,2), labels = c("yes", "no"))
RC_RSV_descriptiu$sister_home_binary <- factor(RC_3$sister_home_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$nirse_binary <- factor(RC_3$nirse_binary, levels = c(0,1), labels = c("No Nirse", "Nirse"))

# Eliminar files amb NA a "nirse_binary" pq es variable d'estratificació
no_na_nirse <- RC_RSV_descriptiu[!is.na(RC_RSV_descriptiu$nirse_binary), ]

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test if there are exactly two levels
    if (length(levels(g)) == 2) {
      p <- t.test(y ~ g)$p.value
    } else {
      # For more than two levels, return NA for p-value
      p <- NA
    }
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", ifelse(is.na(p), "NA", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))))
}

cat("### Anàlisi comparatiu d'inclosos i exclosos en l'estudi:\n")
taula_biv <-table1(~ sex + months_old + month_of_birth + ethnic + kindergarten + breast_binary + space_living + smokers_home + sister_home_binary | region*nirse_binary, data = no_na_nirse, overall=F, extra.col=list(`p-value`=pvalue))

taula_biv