#Taula VRS (3 cohorts, outcomes)
#Agrupació per diagnòstics especifics RSV
#Cohort dels nascuts en seasonal a octubre

#Crear còpia
RC_edit_descriptiu <- RC_3

RC_edit_descriptiu$region <- factor(RC_3$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_edit_descriptiu$nirse_binary <- factor(RC_3$nirse_binary, levels = c(0,1), labels = c("No Nirse", "Nirse"))

RC_edit_descriptiu$final_diagnosis_lt_rsv <- factor(RC_3$final_diagnosis_lt_rsv, levels = c(0,1), labels = c("no", "yes"))
RC_edit_descriptiu$final_diagnosis_ht_rsv <- factor(RC_3$final_diagnosis_ht_rsv, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$final_diagnosis_lt_rsv) <- "final_diagnosis_low_tract RSV"
label(RC_edit_descriptiu$final_diagnosis_ht_rsv) <- "final_diagnosis_high_tract RSV"

#Borrar no seasonals ni catchups
RC_edit_descriptiu <- subset(RC_edit_descriptiu, !is.na(seasonal_catchup))
#Seleccionar nascuts a oct
RC_edit_oct <- subset(RC_edit_descriptiu, cohort_oct == 1)

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


cat("### Anàlisi diferenciant les tres cohorts:\n")
taula_biv <-table1(~ final_diagnosis_lt_rsv + final_diagnosis_ht_rsv | region*nirse_binary, data = RC_edit_oct, overall=F, extra.col = list(`p-value`=pvalue))
taula_biv

cat("### Anàlisi agrupant administració Nirse:\n")
taula_biv <-table1(~ final_diagnosis_lt_rsv + final_diagnosis_ht_rsv | nirse_binary, data = RC_edit_oct, overall=F, extra.col = list(`p-value`=pvalue))
taula_biv
