#Quarta taula (3 cohorts, outcomes)
#Modificació -> Fisher en comptes de Chi-squared approximation per freq<=5

#Crear còpia
RC_edit_descriptiu <- RC_RVI_descriptiu

RC_edit_descriptiu$region <- factor(RC_RVI_descriptiu$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_edit_descriptiu$nirse_binary <- factor(RC_RVI_descriptiu$nirse_binary, levels = c(0,1), labels = c("No Nirse", "Nirse"))

RC_edit_descriptiu$vrs_result <- factor(RC_RVI_descriptiu$vrs_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$antigenic_performed_SARS_CoV_2 <- factor(RC_RVI_descriptiu$antigenic_performed, levels = c(0,1), labels = c("no","yes"))
RC_edit_descriptiu$antigenic_result_SARS_CoV_2 <- factor(RC_RVI_descriptiu$antigenic_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$othervirus_performed <- factor(RC_RVI_descriptiu$othervirus_performed, levels = c(0,1), labels = c("no","yes"))
RC_edit_descriptiu$adeno_result <- factor(RC_RVI_descriptiu$adeno_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$flu_a_result <- factor(RC_RVI_descriptiu$flu_a_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$flu_b_result <- factor(RC_RVI_descriptiu$flu_b_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$adm_hospital <- factor(RC_RVI_descriptiu$adm_hospital, levels = c(0,1), labels = c("no", "yes"))
RC_edit_descriptiu$picu_adm <- factor(RC_RVI_descriptiu$picu_adm, levels = c(0,1), labels = c("no", "yes"))
#RC_edit_descriptiu$months_old <- round(RC_RVI_descriptiu$months_old, 0)

# Eliminar files amb NA a "nirse_binary" pq es variable d'estratificació
no_na_nirse <- RC_edit_descriptiu[!is.na(RC_edit_descriptiu$nirse_binary), ]

pvalue <- function(x, ...) {
  # Construir vectores de datos y y grupos (estratos) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # Para variables numéricas, realizar una prueba t estándar de 2 muestras si hay exactamente dos niveles
    if (length(levels(g)) == 2) {
      p <- t.test(y ~ g)$p.value
    } else {
      # Para más de dos niveles, devolver NA para el valor p
      p <- NA
    }
  } else {
    # Para variables categóricas, realizar una prueba de chi-cuadrado de independencia
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
  
  # Formatear el valor p, usando una entidad HTML para el signo menor que.
  # La cadena inicial vacía coloca la salida en la línea debajo de la etiqueta de la variable.
  c("", ifelse(is.na(p), "NA", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))))
}

cat("### Anàlisi comparatiu d'inclosos i exclosos en l'estudi:\n")
taula_biv <-table1(~ vrs_result + antigenic_performed_SARS_CoV_2 + antigenic_result_SARS_CoV_2 
                   + othervirus_performed + adeno_result + flu_a_result + flu_b_result + adm_hospital 
                   + picu_adm | region*nirse_binary, data = no_na_nirse, overall=F, extra.col=list(`p-value`=pvalue))

taula_biv