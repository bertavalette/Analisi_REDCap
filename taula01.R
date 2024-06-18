#Primera taula (univariada, demogràfica)

#Crear còpia
RC_RSV_descriptiu <- RC_3

RC_RSV_descriptiu$months_old <- round(RC_3$months_old, 0)
RC_RSV_descriptiu$sex <- factor(RC_3$sex, levels = c(1,2), labels = c("male","female"))
RC_RSV_descriptiu$region <- factor(RC_3$region, levels = c(0,1), labels = c("catalonia", "italy"))
RC_RSV_descriptiu$month_of_birth <- factor(RC_3$month_of_birth)
RC_RSV_descriptiu$ethnic <- factor(RC_3$ethnic, levels = c(1,2,3,4,5,6), labels = c("white/caucasian", "black/african/caribbean","hispanic/latino","asian","pacific islander","other/mixed"))
RC_RSV_descriptiu$kindergarten <- factor(RC_3$kindergarten, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$breast_binary <- factor(RC_3$breast_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$space_living <- factor(RC_3$space_living, levels = c(1,2,3,4), labels = c("house (with other family members)", "shelter center", "community space with other families", "other"))
RC_RSV_descriptiu$smokers_home <- factor(RC_3$smokers_home, levels = c(1,2), labels = c("yes", "no"))
#RC_RSV_descriptiu$sister_home_binary <- factor(RC_3$sister_home_binary, levels = c(0,1), labels = c("no", "yes"))
RC_RSV_descriptiu$nirse_binary <- factor(RC_3$nirse_binary, levels = c(0,1), labels = c("no", "yes"))
taula_uv <- table1(~ region + sex + months_old + month_of_birth + ethnic + kindergarten + breast_binary + space_living + smokers_home + nirse_binary, data = RC_RSV_descriptiu)
taula_uv
