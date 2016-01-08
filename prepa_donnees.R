# Préparation données

library(Insee2MonetDB)
library(dplyr)
library(MonetDB.R)

load("./data/communes_ident.Rdata")

# téléchargement données 2012

# tbl_indcvi_2012 <- Insee2MonetDB(url = "http://telechargement.insee.fr/fichiersdetail/RP2012/txt/RP2012_INDCVI_txt.zip", folder = "~/monetdb/")
# tbl_indreg_2012 <- Insee2MonetDB(url = "http://telechargement.insee.fr/fichiersdetail/RP2012/txt/RP2012_INDREG_txt.zip", folder = "~/monetdb/")

conn <- src_monetdb(embedded = "~/monetdb/")

tbl_indcvi_2012 <- tbl(conn, from = "rp2012_indcvi")
tbl_indreg_2012 <- tbl(conn, from = "rp2012_indreg")

# en couple ou pas par commune / d'abord par CV donc + arrondissements marseillais

couples_iris <- tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76) %>%
  group_by(cantville, iris, couple) %>%
  summarise(n = sum(ipondi)) %>%
  collect() %>%
  group_by(cantville, iris) %>%
  mutate(pop = sum(n), p = n / pop * 100)
  
couples_CV <- couples_iris %>%
  group_by(cantville, couple) %>%
  filter(couple == "1") %>%
  summarise(p = sum(n) / sum(pop) * 100)

couples_MRS <- couples_iris %>%
  filter(cantville %in% "1399") %>%
  mutate(arr = substr(iris, 1, 5)) %>%
  group_by(arr, couple) %>%
  filter(couple == "1") %>%
  summarise(p = sum(n) / sum(pop) * 100)

couples_communes <- data_frame(CodeInsee = communes_ident$CodeInsee, p = couples_CV[match(communes_ident$CodeCant, couples_CV$cantville),][["p"]])
couples_communes <- bind_rows(couples_communes %>% filter(!(CodeInsee %in% 13201:13216)), data_frame(CodeInsee = couples_MRS[["arr"]], p = couples_MRS[["p"]]))

couples_communes$Nom <- as.character(communes_ident[match(couples_communes[['CodeInsee']], communes_ident$CodeInsee), "NOM_COMM"])

couples_communes13 <- couples_communes %>% filter(substr(CodeInsee, 1, 2) == "13" & CodeInsee != "13055")


# moyenne nationale : 64

tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76) %>%
  group_by(couple) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  mutate(p = n / sum(n) * 100)


# moyenne régionale : 62,2

tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76, region == "93") %>%
  group_by(couple) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  mutate(p = n / sum(n) * 100)

# moyenne 13 : 60,2

tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76, dept == "13") %>%
  group_by(couple) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  mutate(p = n / sum(n) * 100)

# moyenne marseille : 53,8

couples_iris %>%
  filter(cantville == "1399") %>%
  group_by(couple) %>%
  summarise(p= sum(n) / sum(pop) * 100)

couples_communes13 <- bind_rows(couples_communes13, data_frame(CodeInsee = c("MRS", "BdR", "PACA", "France"), p = c(53.8, 60.2, 62.2, 64), Nom = c("Marseille", "Bouches-du-Rhône", "PACA", "France")))

save(couples_communes13, file = "./data/couples_communes.Rdata")


# par sexe

tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76, dept == "13") %>%
  group_by(couple, sexe) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  group_by(sexe) %>%
  mutate(p = n / sum(n) * 100)

# par âge
couple_age <- tbl_indcvi_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76, dept == "13") %>%
  group_by(couple, agerev) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  group_by(agerev) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(couple == 1) %>%
  arrange(as.integer(agerev))

save(couple_age, file = "./data/couple_age.Rdata")

# CSP, âge et sexe

relabel <- function(df, modalites, variable, VAR_CODE = "VAR_CODE", VAR_LIB = "VAR_LIB", MOD_LIB = "MOD_LIB", MOD_CODE = "MOD_CODE") {
  df[, unique(modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), VAR_LIB])] <-  modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), MOD_LIB][match(df[[variable]], modalites[tolower(modalites[[VAR_CODE]]) %in% tolower(variable), MOD_CODE])]
  return(df)
}

modalites11reg <- read.csv("./data/MOD_INDREG_2011.txt", sep = ";", stringsAsFactors = FALSE)

couples_csp <- tbl_indreg_2012 %>%
  filter(as.integer(agerev) > 17, as.integer(agerev) < 76, dept == "13") %>%
  group_by(couple, agerev, sexe, cs2) %>%
  summarise(n = sum(ipondi)) %>%
  collect %>%
  group_by(agerev, sexe, cs2) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(couple == 1) %>%
  ungroup %>%
  mutate(agerev = as.integer(agerev)) %>%
  relabel(modalites11reg, "cs2") %>%
  relabel(modalites11reg, "sexe") %>%
  rename(CS2 = `Catégorie socioprofessionnelle en 24 postes`) %>%
  filter(n > 30) %>%
  mutate(id = 1:nrow(couples_csp)) 
  

save(couples_csp, file = "./data/couples_csp.Rdata")
