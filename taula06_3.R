#Segona taula (bivariada per regió, outcomes)
#Diferenciant seasonals i catchups

#Crear còpia
RC_edit_descriptiu <- RC_3

RC_edit_descriptiu$region <- factor(RC_3$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_edit_descriptiu$nirse_binary <- factor(RC_3$nirse_binary, levels = c(0,1), labels = c("no", "yes"))

RC_edit_descriptiu$vrs_result <- factor(RC_3$vrs_result, levels = c(1,2), labels = c("positive", "negative"))
RC_edit_descriptiu$antigenic_performed_SARS_CoV_2 <- factor(RC_3$antigenic_performed, levels = c(0,1), labels = c("no","yes"))
RC_edit_descriptiu$antigenic_result_SARS_CoV_2 <- factor(RC_3$antigenic_result, levels = c(1,2), labels = c("positive", "negative"))
RC_edit_descriptiu$othervirus_performed <- factor(RC_3$othervirus_performed, levels = c(0,1), labels = c("no","yes"))
RC_edit_descriptiu$adeno_result <- factor(RC_3$adeno_result, levels = c(1,2,3), labels = c("positive", "negative","unknown/not valid"))
RC_edit_descriptiu$flu_a_result <- factor(RC_3$flu_a_result, levels = c(1,2), labels = c("positive", "negative"))
RC_edit_descriptiu$flu_b_result <- factor(RC_3$flu_b_result, levels = c(1,2), labels = c("positive", "negative"))
RC_edit_descriptiu$adm_hospital <- factor(RC_3$adm_hospital, levels = c(0,1), labels = c("no", "yes"))
RC_edit_descriptiu$picu_adm <- factor(RC_3$picu_adm, levels = c(0,1), labels = c("no", "yes"))
RC_edit_descriptiu$emergency_dep_binary <- factor(RC_3$emergency_dep_binary, levels = c(0,1), labels = c("no", "yes"))
RC_edit_descriptiu$urg_viruses___1 <- factor(RC_3$urg_viruses___1, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___1) <- "urg_viruses_SARS-CoV-2"
RC_edit_descriptiu$urg_viruses___2 <- factor(RC_3$urg_viruses___2, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___2) <- "urg_viruses_influenza A H1N1pdm09"
RC_edit_descriptiu$urg_viruses___3 <- factor(RC_3$urg_viruses___3, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___3) <- "urg_viruses_influenza A H3N2"
RC_edit_descriptiu$urg_viruses___4 <- factor(RC_3$urg_viruses___4, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___4) <- "urg_viruses_influenza B"
RC_edit_descriptiu$urg_viruses___5 <- factor(RC_3$urg_viruses___5, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___5) <- "urg_viruses_VRS-A"
RC_edit_descriptiu$urg_viruses___6 <- factor(RC_3$urg_viruses___6, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___6) <- "urg_viruses_VRS-B"
RC_edit_descriptiu$urg_viruses___7 <- factor(RC_3$urg_viruses___7, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___7) <- "urg_viruses_adenovirus"
RC_edit_descriptiu$urg_viruses___8 <- factor(RC_3$urg_viruses___8, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___8) <- "urg_viruses_human metapneumovirus"
RC_edit_descriptiu$urg_viruses___9 <- factor(RC_3$urg_viruses___9, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___9) <- "urg_viruses_parainfluenza 1,2,3,4"
RC_edit_descriptiu$urg_viruses___10 <- factor(RC_3$urg_viruses___10, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___10) <- "urg_viruses_bocavirus"
RC_edit_descriptiu$urg_viruses___11 <- factor(RC_3$urg_viruses___11, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___11) <- "urg_viruses_enterovirus"
RC_edit_descriptiu$urg_viruses___12 <- factor(RC_3$urg_viruses___12, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___12) <- "urg_viruses_rhinovirus"
RC_edit_descriptiu$urg_viruses___13 <- factor(RC_3$urg_viruses___13, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___13) <- "urg_viruses_other CoV"
RC_edit_descriptiu$urg_viruses___14 <- factor(RC_3$urg_viruses___14, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___14) <- "urg_viruses_other viruses"
RC_edit_descriptiu$urg_viruses___15 <- factor(RC_3$urg_viruses___15, levels = c(0,1), labels = c("no", "yes"))
label(RC_edit_descriptiu$urg_viruses___15) <- "urg_viruses_none"
RC_edit_descriptiu$final_diagnosis_code <- factor(RC_3$final_diagnosis_code, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21), labels = c("U07.1 SARS-CoV-2 virus identified", "J21.0 RSV bronquiolitis",
                                                                                                                                                           "J10.1 Influenza virus identified","B97.0 Adenovirus","J12.0 Adenovirus pneumonia",
                                                                                                                                                           "J12.1 RSV pneumonia","J12.2 Pneumonia due to parainfluenza virus","J12.3 Pneumonia due to human metapneumovirus",
                                                                                                                                                           "J12.9 Unspecified viral pneumonia","J20.3 Acute bronchitis due to coxsackievirus",
                                                                                                                                                           "J20.4 Acute bronchitis due to parainfluenza virus","J20.5 Acute bronchitis due to RSV",
                                                                                                                                                           "J20.6 Acute bronchitis due to rhinovirus","J20.7 Acute bronchitis due to echovirus",
                                                                                                                                                           "J20.8 Acute bronchitis due to other specified microorganisms","J20.9 Unspecified acute bronchitis",
                                                                                                                                                           "J21.1 Acute bronchiolitis due to human metapneumovirus","J21.8 Acute bronchiolitis due to other specified microorganisms",
                                                                                                                                                           "J21.9 Unspecified acute bronchiolitis","B97.4 Unspecified RSV infection","J06.9 Unspecified acute high respiratory tract infection"))
#Borrar no seasonals ni catchups
RC_edit_descriptiu <- subset(RC_edit_descriptiu, !is.na(seasonal_catchup))

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

# Dataset dels seasonals
RC_edit_seasonal <- subset(RC_edit_descriptiu, seasonal_catchup == "seasonal")
RC_edit_seasonal$vrs_result <- factor(RC_edit_seasonal$vrs_result, levels = c("positive", "negative"))

# Dataset dels catchup (només Catalunya)
RC_edit_catalan <- subset(RC_edit_descriptiu, region == "Catalonia")
RC_edit_catchup <- subset(RC_edit_catalan, seasonal_catchup == "catchup")

taula_biv <-table1(~ seasonal_catchup + nirse_binary + vrs_result + antigenic_performed_SARS_CoV_2 + antigenic_result_SARS_CoV_2 + othervirus_performed + adeno_result + flu_a_result
                   + flu_b_result + emergency_dep_binary + urg_viruses___1 + urg_viruses___2 + urg_viruses___3 + urg_viruses___4 
                   + urg_viruses___5 + urg_viruses___6 + urg_viruses___7 + urg_viruses___8 + urg_viruses___9 + urg_viruses___10 + urg_viruses___11 + urg_viruses___12 
                   + urg_viruses___13 + urg_viruses___14 + urg_viruses___15 + adm_hospital + picu_adm + final_diagnosis_code | region, data = RC_edit_descriptiu, overall=F, extra.col=list(`p-value`=pvalue))

taula_biv_seasonal <-table1(~ seasonal_catchup + nirse_binary + vrs_result + antigenic_performed_SARS_CoV_2 + antigenic_result_SARS_CoV_2 + othervirus_performed + adeno_result + flu_a_result 
                           + flu_b_result + emergency_dep_binary + urg_viruses___1 + urg_viruses___2 + urg_viruses___3 + urg_viruses___4 
                           + urg_viruses___5 + urg_viruses___6 + urg_viruses___7 + urg_viruses___8 + urg_viruses___9 + urg_viruses___10 + urg_viruses___11 + urg_viruses___12 
                           + urg_viruses___13 + urg_viruses___14 + urg_viruses___15 + adm_hospital + picu_adm + final_diagnosis_code | region, data = RC_edit_seasonal, overall=F, extra.col=list(`p-value`=pvalue))

taula_uv_catchup <-table1(~ seasonal_catchup + nirse_binary + vrs_result + antigenic_performed_SARS_CoV_2 + antigenic_result_SARS_CoV_2 + othervirus_performed + adeno_result + flu_a_result 
                          + flu_b_result + emergency_dep_binary + urg_viruses___1 + urg_viruses___2 + urg_viruses___3 + urg_viruses___4 
                          + urg_viruses___5 + urg_viruses___6 + urg_viruses___7 + urg_viruses___8 + urg_viruses___9 + urg_viruses___10 + urg_viruses___11 + urg_viruses___12 
                          + urg_viruses___13 + urg_viruses___14 + urg_viruses___15 + adm_hospital + picu_adm + final_diagnosis_code, data = RC_edit_catchup)



taula_biv
taula_biv_seasonal
taula_uv_catchup