#Taula VRS (3 cohorts, outcomes)
#Agrupació per confirmació tests

#Crear còpia
RC_edit_vrs <- RC_3

RC_edit_vrs$region <- factor(RC_3$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_edit_vrs$nirse_binary <- factor(RC_3$nirse_binary, levels = c(0,1), labels = c("No Nirse", "Nirse"))

RC_edit_vrs$vrs_result <- factor(RC_3$vrs_result, levels = c(1,2), labels = c("positive", "negative"))
RC_edit_vrs$urg_viruses_VRS <- factor(RC_3$urg_viruses_VRS, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_vrs$urg_viruses_VRS) <- "urg_viruses_VRS (A and B)"
RC_edit_vrs$VRS_urg_antig <- factor(RC_3$VRS_urg_antig, levels = c(0,1), labels = c("no VRS", "VRS (antigen or pcr)"))


#RC_edit_vrs$months_old <- round(RC_3$months_old, 0)

# Eliminar files amb NA a "nirse_binary" pq es variable d'estratificació
no_na_nirse <- RC_edit_vrs[!is.na(RC_edit_vrs$nirse_binary), ]

# p-value
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
taula_biv <-table1(~ vrs_result + urg_viruses_VRS + VRS_urg_antig | region*nirse_binary, data = no_na_nirse, overall=F, extra.col = list(`p-value`=pvalue))

taula_biv