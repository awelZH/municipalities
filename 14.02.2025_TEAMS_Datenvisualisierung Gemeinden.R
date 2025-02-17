
################################################################################
                     ### Datenvisualisierung Gemeinden ###                  
################################################################################

#################
### Data prep ###
#################

# install packages
install.packages("tidyverse")
install.packages("readxl")
install.packages("openxlsx")
install.packages("readr")
install.packages("writexl")

# load libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)
library(writexl)

################################################################################

#################
### Gemeinden ###
#################

# read CH municipalities from https://www.agvchapp.bfs.admin.ch/de into R
# to change date, simply change last part of URL (e.g., for 1 Jan. 2025, use https://www.agvchapp.bfs.admin.ch/api/communes/snapshot?date=01-01-2025)
ch_gem <- read_delim("https://www.agvchapp.bfs.admin.ch/api/communes/snapshot?date=01-01-2024", delim = ",")

# delete rows below HistoricalCode == 2 (Bern)
ch_gem2 <- slice(ch_gem, 1:which(HistoricalCode == 2, arr.ind = TRUE))

# select ZH municipalities
zh_gem <- ch_gem2 %>% 
  filter(Level == 3)

zh_gem2 <- zh_gem[, c(2, 6, 7)]

# check numbers for Bezirke
unique(zh_gem[,'Parent'])

# rename columns
zh_gem2 <- zh_gem2 %>% 
  rename(BFS = 1, Bezirk = 2, Gemeinde = 3)

# rename Bezirk values
zh_gem2 <- zh_gem2 %>% 
  mutate(Bezirk = recode(Bezirk, '10053' = 'Affoltern', '10074' = 'Winterthur', '10075' = 'Uster', '10076' = 'Pfäffikon', '10077' = 'Meilen', '10078' = 'Horgen', '10079' = 'Hinwil', '10080' = 'Dielsdorf', '10081' = 'Bülach', '10082' = 'Andelfingen', '10228' = 'Zürch', '10229' = 'Dietikon'))

################################################################################

#############
### Kurse ###
#############

# import file
kurse <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Gemeindevisualisierung Kurse.xlsx", sheet = 1)

kurse_2 <- kurse  %>%
  # rename values
  mutate(Gemeinde = recode(Gemeinde, "Pfäffikon" = "Pfäffikon (ZH)"))
  
################################################################################

############################
### Netto-Null Strategie ###
############################

# import file
nettonull <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Gemeindevisualisierung Netto-Null Strategie (Stand 2024_04).xlsx", sheet = 1)

################################################################################

####################
### Energiestadt ###
####################

# import file
energiestadt <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Liste_ES_ZH_20241205.xlsx", sheet = 1)

# rename second column
colnames(energiestadt)[2] <- "Energiestadt"

energiestadt_2 <- energiestadt  %>%
  # rename values
  mutate(Energiestadt = recode(Energiestadt, "Energiestadt Gold" = "Gold", "Energiestadt Trägerverein" = "TV", "Energiestadt" = "Ja")) %>%
  mutate(Gemeinde = recode(Gemeinde, "Wald" = "Wald (ZH)", "Wetzikon" = "Wetzikon (ZH)", "Rüti" = "Rüti (ZH)", "Kilchberg" = "Kilchberg (ZH)", "Erlenbach" = "Erlenbach (ZH)"))

# rename second column to "Energiestadt (Nein/TV/Ja/Gold)"
colnames(energiestadt_2)[2] <- "Energiestadt (Nein/TV/Ja/Gold)"

################################################################################

######################
### Energieplanung ###
######################

# import file, selecting only first sheet (Energieplanung)
energieplanung <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Gasversorgung und Energieplanung.xlsx", sheet = 1)

# select columns 1 (BFS-Gemeinde) and 3 (Energieplanung Jahr)
energieplanung_2 <- energieplanung[, c(1, 3)]

# rename second column to "Energieplanung (Jahr)"
colnames(energieplanung_2)[2] <- "Energieplanung (Jahr)"

# add new column named "Energieplanung (Ja/Nein)" based on "Energieplanung (Jahr)"
energieplanung_2$`Energieplanung (Ja/Nein)` <- ifelse(!is.na(energieplanung_2[[2]]) & energieplanung_2[[2]] != "", "Ja", "Nein")

# view updated data
head(energieplanung_2)

################################################################################

#####################
### Gasversorgung ###
#####################

gasversorgung <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Gasversorgung und Energieplanung.xlsx", sheet = 2)

# rename two columns
colnames(gasversorgung)[2] <- "Gemeinde"
colnames(gasversorgung)[3] <- "Gasversorgung (Ja/Nein, Stand Nov. 2024)"

# recode "Gasversorgung...": existing year becomes "Ja", NA becomes "Nein"
gasversorgung$`Gasversorgung (Ja/Nein, Stand Nov. 2024)` <- ifelse(!is.na(gasversorgung$`Gasversorgung (Ja/Nein, Stand Nov. 2024)`) & gasversorgung$`Gasversorgung (Ja/Nein, Stand Nov. 2024)` != "", "Ja", "Nein")

# check if recoding worked
head(gasversorgung)

################################################################################

###################
### Klimadialog ###
###################

klimadialog <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/Klimadialog_Masterliste.xlsx", sheet = 1, skip = 1)

# select columns 3 (BFS-Gemeinde), 4 (Gemeinde), 15 (AG E-Mob), 16 (AG Verstetigung), 17 (AG Hitzeminderung), 18 (AG Teilhabe), 19 (AG Wärmeverbünde), 20 (Klimadialog 2022), 21 (Klimadialog vor Ort 2023), 22 (Klimadialog Okt. 2023), 23 (Klimadialog Mai 2024), 24 (Klimadialog Okt. 2024)
klimadialog_2 <- klimadialog[, c(3, 4, 17:26)]

# rename two columns
colnames(klimadialog_2)[1] <- "BFS"
colnames(klimadialog_2)[2] <- "Gemeinde"

# count people per municipality // e.g., KD 2022: how many "x"s appear in "Klimadialog 2022" column for each "BFS-Nr." and add this count as a new column "KD 2022 (# Personen)"
klimadialog_3 <- klimadialog_2 %>%
  group_by(`BFS`) %>%
  mutate(`KD 2022 (# Personen)` = sum(`Klimadialog 2022` == "x", na.rm = TRUE)) %>%
  mutate(`KD 2023 Juni (# Personen)` = sum(`Klimadialog vor Ort 2023` == "x", na.rm = TRUE)) %>%
  mutate(`KD 2023 Okt. (# Personen)` = sum(`Klimadialog Oktober 2023` == "x", na.rm = TRUE)) %>%
  mutate(`KD 2024 Juni (# Personen)` = sum(`Klimadialog Mai 2024` == "x", na.rm = TRUE)) %>%
  mutate(`KD 2024 Okt. (# Personen)` = sum(`Klimadialog Oktober 2024` == "x", na.rm = TRUE)) %>%
  mutate(`AG E-Mobilität (# Personen)` = sum(`AG Elektromobilität` == "x", na.rm = TRUE)) %>%
  mutate(`AG Verstetigung (# Personen)` = sum(`AG Verstetigung` == "x", na.rm = TRUE)) %>%
  mutate(`AG Hitzeminderung (# Personen)` = sum(`AG Hitzeminderung` == "x", na.rm = TRUE)) %>%
  mutate(`AG Teilhabe (# Personen)` = sum(`AG Teilhabe` == "x", na.rm = TRUE)) %>%
  mutate(`AG Wärmeverbünde (# Personen)` = sum(`AG Wärmeverbünde` == "x", na.rm = TRUE)) %>%
  ungroup()

# extract only first entry by municipality
klimadialog_4 <- klimadialog_3 %>%
  group_by(`BFS`) %>%
  slice(1) %>%
  ungroup() %>%
# delete useless columns
  select(-`AG Elektromobilität`, -`AG Verstetigung`, -`AG Hitzeminderung`, -`AG Teilhabe`, -`AG Wärmeverbünde`, -`Klimadialog 2022`, -`Klimadialog vor Ort 2023`, -`Klimadialog Oktober 2023`, -`Klimadialog Mai 2024`, -`Klimadialog Oktober 2024`) %>%
# delete empty rows below "BFS-Nr."
  filter(`BFS`!="")

################################################################################

################
### Ecospeed ###
################

ecospeed <- read_excel("//home.kt.ktzh.ch/BAA4528$/Desktop/Datenvisualisierung Gemeinden/User_Auswertung_3.Quartal_2024.xlsx", sheet = 1, skip = 1)

# select column 3 (Gemeindename)
ecospeed_2 <- ecospeed[, c(3)] %>%
# extract only first entry by municipality
  group_by(`Gemeindename`) %>%
  slice(1) %>%
  ungroup() %>%
# add new column named "Ecospeed (Ja/Nein)"
  mutate(`Ecospeed (Ja/Nein)` = "Ja")

# rename 1st column Gemeindename to Gemeinde
colnames(ecospeed_2)[1] <- "Gemeinde"

################################################################################

####################
### Wärmeverbund ###
####################

# download zipped data 
url <- "https://data.geo.admin.ch/ch.bfe.thermische-netze/csv/2056/ch.bfe.thermische-netze.zip"
temp <- tempfile()
download.file(url,temp, mode = "wb")
waermeverbund <- read.csv(unzip(temp, "ch.bfe.thermische-netze.csv"))
unlink(temp)

head(waermeverbund)

# select ZH municipalities 
waermeverbund_2 <- waermeverbund[waermeverbund$Zip >= 8000 & waermeverbund$Zip < 9000,]

# rename Place to Gemeinde
colnames(waermeverbund_2)[4] <- "Gemeinde"

# extract only first entry by municipality
waermeverbund_3 <- waermeverbund_2 %>%
  group_by(`Gemeinde`) %>%
  slice(1) %>%
  ungroup()

# select column 4 ("Place" aka Gemeinde)
waermeverbund_4 <- waermeverbund_3[, c(4)] %>%
# remove ZIP code from "6056 Kägiswil"
  mutate(Gemeinde = recode(Gemeinde, '6056 Kägiswil' = 'Kägiswil', 'Benken' = 'Benken (ZH)', 'Birmensdorf' = 'Birmensdorf (ZH)', 'Rüti' = 'Rüti (ZH)', 'Wangen' = 'Wangen-Brüttisellen', 'Wil' = 'Wil (ZH)'))  %>%
# add column to indicate 'Wärmeverbund (Ja/Nein)'
  mutate(`Wärmeverbund (Ja/Nein)` = "Ja")


################################################################################

#############
### Merge ###
#############

# left join Energieplanung and Gasversorgung
ep_gv <- merge(x = gasversorgung, y = energieplanung_2, by = "BFS", all.x = TRUE)

# merge ZH Gemeinden and Ecospeed
gem_eco <- merge(x = zh_gem2, y = ecospeed_2, by = "Gemeinde", all = TRUE)

# merge ZH Gemeinden, Ecospeed, and KD
gem_eco_kd <- merge(x = gem_eco, y = klimadialog_4, by = "Gemeinde", all = TRUE)

# merge ZH Gemeinden, Ecospeed, KD, and Kurse
gem_eco_kd_kurse <- merge(x = gem_eco_kd, y = kurse_2, by = "Gemeinde", all = TRUE)

# merge ZH Gemeinden, Ecospeed, KD, Kurse, and Energiestadt
gem_eco_kd_kurse_es <- merge(x = gem_eco_kd_kurse, y = energiestadt_2, by = "Gemeinde", all = TRUE)

# enter some values in Pfäffikon that are missing but contained in Pfäffikon (ZH)
gem_eco_kd_kurse_es_2 <- gem_eco_kd_kurse_es %>%
  mutate(Bezirksnummer = ifelse(Gemeinde == "Pfäffikon" & is.na(Bezirksnummer), "108", Bezirksnummer)) %>%
  mutate(Bezirk.y = ifelse(Gemeinde == "Pfäffikon" & is.na(Bezirk.y), "Pfäffikon", Bezirk.y)) %>%
  mutate(BFS = ifelse(Gemeinde == "Pfäffikon" & is.na(BFS), "177", BFS))

# drop Pfäffikon (ZH) (we will work with Pfäffikon going forward)
gem_eco_kd_kurse_es_3 <- gem_eco_kd_kurse_es_2[!(gem_eco_kd_kurse_es_2$Gemeinde %in% "Pfäffikon (ZH)"),]

# rename Pfäffikon to Pfäffikon (ZH)
gem_eco_kd_kurse_es_4 <- gem_eco_kd_kurse_es_3  %>%
  # rename values
  mutate(Gemeinde = recode(Gemeinde, "Pfäffikon" = "Pfäffikon (ZH)"))

# left (outer) join keeps all rows from left table and any rows matching keys from right table
gem_eco_kd_kurse_es_wn <- merge(x = gem_eco_kd_kurse_es_4, y = waermeverbund_4, by = "Gemeinde", all.x = TRUE)

# left join for gem_eco_kd_kurse_es_wn and Energieplanung + Gasversorgung
gem_eco_kd_kurse_es_wn_ep_gv <- merge(x = ep_gv, y = gem_eco_kd_kurse_es_wn, by = "BFS", all.x = TRUE)

# left join for gem_eco_kd_kurse_es_wn_ep_gv and Netto-Null Strategie
gem_complete <- merge(x = gem_eco_kd_kurse_es_wn_ep_gv, y = nettonull, by = "BFS", all.x = TRUE)

# enter missing values for Humlikon & Adlikon
gem_complete_2 <- gem_complete %>%
  mutate(Gemeinde = ifelse(Gemeinde.x == "Humlikon" & is.na(Gemeinde), "Humlikon", Gemeinde)) %>%
  mutate(Gemeinde = ifelse(Gemeinde.x == "Adlikon" & is.na(Gemeinde), "Adlikon", Gemeinde)) %>%

  mutate(Bezirk = ifelse(Gemeinde.x == "Humlikon" & is.na(Bezirk), "Andelfingen", Bezirk)) %>%
  mutate(Bezirk = ifelse(Gemeinde.x == "Adlikon" & is.na(Bezirk), "Andelfingen", Bezirk)) %>%

  mutate(Bezirksnummer.x = ifelse(Gemeinde.x == "Humlikon" & is.na(Bezirksnummer.x), "102", Bezirksnummer.x)) %>%
  mutate(Bezirksnummer.x = ifelse(Gemeinde.x == "Adlikon" & is.na(Bezirksnummer.x), "102", Bezirksnummer.x))

# delete useless columns: BFS.x, BFS.y, Gemeinde.x, Gemeinde.y, Bezirk.x, Bezirk.y, Bezirksnummer.y
# (keep: BFS, Gemeinde, Bezirk, Bezirksnummer.x)
gem_complete_3 = subset(gem_complete_2, select = -c(BFS.x, BFS.y, Gemeinde.x, Gemeinde.y, Bezirk.x, Bezirk.y, Bezirksnummer.y))

# change order of columns + delete column "Link"ncol(gem_complete_3)
ncol(gem_complete_3)
gem_complete_4 <- gem_complete_3[,c(16,22,1,23,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,4,3,2,21,24,25,26,27)]

# rename Bezirksnummer.x to Bezirksnummer
colnames(gem_complete_4)[1] <- "Bezirksnummer"

###

# rename some columns to further work on data
colnames(gem_complete_4)[5] <- "Ecospeed"
colnames(gem_complete_4)[19] <- "Energiestadt" # for "Energiestadt (Nein/TV/Ja/Gold)"
colnames(gem_complete_4)[23] <- "Waermeverbund"

# replace NA in three columns with "Nein"
gem_complete_5 <- gem_complete_4 %>%
  mutate(Ecospeed = ifelse(is.na(Ecospeed), "Nein", Ecospeed)) %>%
  mutate(Energiestadt = ifelse(is.na(Energiestadt), "Nein", Energiestadt)) %>%
  mutate(Waermeverbund = ifelse(is.na(Waermeverbund), "Nein", Waermeverbund))

# rename columns
colnames(gem_complete_5)[5] <- "Ecospeed (Ja/Nein)"
colnames(gem_complete_5)[19] <- "Energiestadt (Nein/TV/Ja/Gold)"
colnames(gem_complete_5)[23] <- "Wärmeverbund (Ja/Nein)"

## sum up users for Kurse/Klimadialoge/Ecospeed & AGs Klimadialog
# count nr. of participants in AGs KD
gem_complete_6 <- gem_complete_5 %>%
  mutate(AGnr = rowSums(select(., starts_with("AG")), na.rm = TRUE))

# create column that recodes Ecospeed(Ja/Nein) to binary 
gem_complete_7 <- gem_complete_6 %>%
  mutate(nreco = ifelse(`Ecospeed (Ja/Nein)` == "Ja", 1, ifelse(`Ecospeed (Ja/Nein)` == "Nein", 0, NA)))

# count nr. of participants in KD, Ecospeed, Kurse (Netto-Null, Workshop)
gem_complete_8 <- gem_complete_7 %>%
  mutate(visitednr = rowSums(select(., starts_with("KD"), starts_with("nreco"), starts_with("Netto"), starts_with("Workshop")), na.rm = TRUE))

# rename AGnr to "# Personen in AGs KD"; visitednr to "# besuchte Kurse/Klimadialoge/Ecospeed genutzt"
ncol(gem_complete_8) # nr. of columns = 30
colnames(gem_complete_8)[28] <- "# Personen in AGs KD"
colnames(gem_complete_8)[30] <- "# besuchte Kurse/Klimadialoge/Ecospeed genutzt"

# reorder columns and drop nreco (29th column)
gem_complete_9 <- gem_complete_8[,c(1:16,30,28,17:27)]


################################################################################

######################
### Download files ###
######################

# download as excel file
write_xlsx(gem_complete_9, "table_municipalities_2.xlsx")

# clean up column names for csv
gem_complete_10 <- gem_complete_9

colnames(gem_complete_10)[1] <- "bzk_nr"
colnames(gem_complete_10)[2] <- "bzk"
colnames(gem_complete_10)[3] <- "bfs"
colnames(gem_complete_10)[4] <- "gemeinde"
colnames(gem_complete_10)[5] <- "ecospeed"
colnames(gem_complete_10)[6] <- "kd22"
colnames(gem_complete_10)[7] <- "kd23_jun"
colnames(gem_complete_10)[8] <- "kd23_oct"
colnames(gem_complete_10)[9] <- "kd24_jun"
colnames(gem_complete_10)[10] <- "kd24_oct"

colnames(gem_complete_10)[11] <- "ag_mob"
colnames(gem_complete_10)[12] <- "ag_verst"
colnames(gem_complete_10)[13] <- "ag_hitze"
colnames(gem_complete_10)[14] <- "ag_teil"
colnames(gem_complete_10)[15] <- "ag_waerme"
colnames(gem_complete_10)[16] <- "workshop"
colnames(gem_complete_10)[17] <- "nr_part"
colnames(gem_complete_10)[18] <- "nr_ag"
colnames(gem_complete_10)[19] <- "net22"
colnames(gem_complete_10)[20] <- "net23"

colnames(gem_complete_10)[21] <- "en_stadt"
colnames(gem_complete_10)[22] <- "en_plan"
colnames(gem_complete_10)[23] <- "en_plan_year"
colnames(gem_complete_10)[24] <- "gas"
colnames(gem_complete_10)[25] <- "heat"
colnames(gem_complete_10)[26] <- "status"
colnames(gem_complete_10)[27] <- "goal"
colnames(gem_complete_10)[28] <- "strat"
colnames(gem_complete_10)[29] <- "adapt"

# download as csv file
write.csv(gem_complete_10, "table_municipalities_2.csv", row.names = FALSE)

################################################################################

################
### Comments ###
################

## Maschwanden + Oberembrach enthalten keine Info zu Klimadialogen. ...
# Statt Nullen einzugeben, habe ich diese Einträge als NA beibehalten.

## Adlikon und Humlikon waren bis Dez. 2022 getrennte politische Gemeinden. ...
# Seit 2023 sind sie gemeinsam Teil der Gemeinde Andelfingen. ...
# Ich führe sie dennoch einzeln hier auf, um die Infos zur Gasversorgung/Energieplanung zu erhalten. 

################################################################################
