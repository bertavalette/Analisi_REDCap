#Quarta taula (3 cohorts, demogràfica)
#Separant seasonals i catchups

#Crear còpia
RC_RSV_descriptiu <- RC_3

#Borrar no seasonals ni catchups
RC_RSV_descriptiu <- subset(RC_RSV_descriptiu, !is.na(seasonal_catchup))

# Convertir format data
conv_data <- function(data_mm_aa) {
  data_data <- dmy(paste0("01-", data_mm_aa))
  format(data_data, "%b-%y")
}

RC_RSV_descriptiu$month_of_birth <- sapply(RC_RSV_descriptiu$month_of_birth, conv_data)
ordered_levels <- unique(RC_RSV_descriptiu$month_of_birth[order(parse_date_time(RC_3$month_of_birth, orders="b-y", locale="en"))])
RC_RSV_descriptiu$month_of_birth <- factor(RC_RSV_descriptiu$month_of_birth, levels=ordered_levels)

#Transformació de tipus de variable + etiquetes
RC_RSV_descriptiu$months_old <- round(RC_RSV_descriptiu$months_old, 0)
RC_RSV_descriptiu$month_of_birth <- factor(RC_RSV_descriptiu$month_of_birth)
RC_RSV_descriptiu$sex <- factor(RC_RSV_descriptiu$sex, levels = c(1,2,3), labels = c("male","female","not specified"))
RC_RSV_descriptiu$region <- factor(RC_RSV_descriptiu$region, levels = c(0,1), labels = c("Catalonia", "Italy"))
RC_RSV_descriptiu$ethnic <- factor(RC_RSV_descriptiu$ethnic, levels = c(1,2,3,4,5,6), labels = c("white/caucasian", "black/african/caribbean","hispanic/latino","asian","pacific islander","other/mixed"))
RC_RSV_descriptiu$kindergarten <- factor(RC_RSV_descriptiu$kindergarten, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$breast_binary <- factor(RC_RSV_descriptiu$breast_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$space_living <- factor(RC_RSV_descriptiu$space_living, levels = c(1,2,3,4), labels = c("house (with other family members)", "shelter center", "community space with other families", "other"))
RC_RSV_descriptiu$smokers_home <- factor(RC_RSV_descriptiu$smokers_home, levels = c(1,2), labels = c("yes", "no"))
RC_RSV_descriptiu$sister_home_binary <- factor(RC_RSV_descriptiu$sister_home_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$nirse_binary <- factor(RC_RSV_descriptiu$nirse_binary, levels = c(0,1), labels = c("No Nirse", "Nirse"))


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
RC_RSV_seasonal <- subset(RC_RSV_descriptiu, seasonal_catchup == "seasonal")
RC_RSV_seasonal$sex <- factor(RC_RSV_seasonal$sex, levels = c("male","female"))
RC_RSV_seasonal$month_of_birth <- sapply(RC_RSV_seasonal$month_of_birth, conv_data)
ordered_levels <- unique(RC_RSV_seasonal$month_of_birth[order(parse_date_time(RC_3$month_of_birth, orders="b-y", locale="en"))])
RC_RSV_seasonal$month_of_birth <- factor(RC_RSV_seasonal$month_of_birth, levels=ordered_levels)

# Dataset dels catchup (només Catalunya)
RC_RSV_catalan <- subset(RC_RSV_descriptiu, region == "Catalonia")
RC_RSV_catchup <- subset(RC_RSV_catalan, seasonal_catchup == "catchup")
RC_RSV_catchup$month_of_birth <- sapply(RC_RSV_catchup$month_of_birth, conv_data)
ordered_levels <- unique(RC_RSV_catchup$month_of_birth[order(parse_date_time(RC_3$month_of_birth, orders="b-y", locale="en"))])
RC_RSV_catchup$month_of_birth <- factor(RC_RSV_catchup$month_of_birth, levels=ordered_levels)


taula_biv <-table1(~ sex + months_old + month_of_birth + seasonal_catchup + ethnic + kindergarten + breast_binary + space_living + smokers_home | region*nirse_binary, data = RC_RSV_descriptiu, overall=F, extra.col=list(`p-value`=pvalue))
taula_biv_seasonal <-table1(~ sex + months_old + month_of_birth + ethnic + kindergarten + breast_binary + space_living + smokers_home | region*nirse_binary, data = RC_RSV_seasonal, overall=F, extra.col=list(`p-value`=pvalue))
taula_uv_catchup <-table1(~ sex + months_old + month_of_birth + ethnic + kindergarten + breast_binary + space_living + smokers_home | region*nirse_binary, data = RC_RSV_catchup, overall=F, extra.col=list(`p-value`=pvalue))

taula_biv
taula_biv_seasonal
taula_uv_catchup