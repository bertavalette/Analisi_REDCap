#Segona taula (bivariada per regió, demogràfica)
#Modificació -> Fisher en comptes de Chi-squared approximation per freq<=5

#Crear còpia
RC_RSV_descriptiu <- RC_RVI_descriptiu

#Transformació de tipus de variable + etiquetes
RC_RSV_descriptiu$months_old <- round(RC_RVI_descriptiu$months_old, 0)
RC_RSV_descriptiu$sex <- factor(RC_RVI_descriptiu$sex, levels = c(1,2), labels = c("male","female"))
RC_RSV_descriptiu$region <- factor(RC_RVI_descriptiu$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_RSV_descriptiu$ethnic <- factor(RC_RVI_descriptiu$ethnic, levels = c(1,2,3,4,5,6), labels = c("white/caucassian", "black/african/caribbean","hispanic/latino","asian","pacific islander","other/mixed"))
RC_RSV_descriptiu$kindergarten <- factor(RC_RVI_descriptiu$kindergarten, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$breast_binary <- factor(RC_RVI_descriptiu$breast_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$space_living <- factor(RC_RVI_descriptiu$space_living, levels = c(1,2,3,4), labels = c("house (with other family members)", "shelter center", "community space with other families", "other"))
RC_RSV_descriptiu$smokers_home <- factor(RC_RVI_descriptiu$smokers_home, levels = c(1,2), labels = c("yes", "no"))
RC_RSV_descriptiu$sister_home_binary <- factor(RC_RVI_descriptiu$sister_home_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$nirse_binary <- factor(RC_RVI_descriptiu$nirse_binary, levels = c(0,1), labels = c("no", "yes"))

pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    if (length(levels(g)) == 2) {
      p <- t.test(y ~ g)$p.value
    } else {
      p <- NA
    }
  } else {
    tbl <- table(y, g)
    if (all(tbl >= 5)) {
      p <- chisq.test(tbl)$p.value
    } else {
      # Intentar con más workspace primero
      p <- tryCatch({
        fisher.test(tbl, workspace = 2e5)$p.value
      }, error = function(e) {
        # Si falla, usar simulación para obtener el valor p
        fisher.test(tbl, simulate.p.value = TRUE, B = 1e5)$p.value
      })
    }
  }
  
  c("", ifelse(is.na(p), "NA", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))))
}

cat("### Anàlisi comparatiu d'inclosos i exclosos en l'estudi:\n")
taula_biv <- table1(~ sex + months_old + ethnic + kindergarten + breast_binary + space_living + smokers_home + sister_home_binary + nirse_binary | region, 
                    data = RC_RSV_descriptiu, overall=F, extra.col=list(`p-value`=pvalue))

taula_biv
