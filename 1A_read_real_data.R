library(readxl)
library(tidyverse)
source(here::here("utils.R"))
path_data <- here::here("../data")

####### adapt paths, sheets and range (i.e. columns of excel file)
#read in all the data
beetle1 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 1, range = "A1:G90")
beetle2 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 1, range = "H1:N43")
beetle3 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 1, range = "O1:U56")
beetle4 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 1, range = "V1:AB77")
beetle5 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 2, range = "A1:G32")
beetle6 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 2, range = "A38:G50", col_names = colnames(beetle5))
beetle7 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 2, range = "H1:N53")
beetle8 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 2, range = "O1:U64")
beetle9 <- read_excel(paste0(path_data, "/beetle_realData.xlsx"), sheet = 2, range = "V1:AB58")

# join all the data
beetle = bind_rows(beetle1, beetle2, beetle3, beetle4) %>% 
  mutate(Datum = as.character(Datum))%>% 
  bind_rows(beetle5, beetle6, beetle7, beetle8, beetle9)%>%
  drop_na(Anzahl)

# check for typos in Console
unique(beetle$Käferart) %>% sort()

####### adapt to your needs
# replace typos by correct name
beetle_corrected = beetle %>% 
  mutate(Käferart = ifelse(Käferart == "Amara aenae", "Amara aenea", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Amamara plebeja", "Amara plebeja", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Amara simulata", "Amara similata", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Anchmenus dorsalis", "Anchomenus dorsalis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Anchomenu dorsalis", "Anchomenus dorsalis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Ansiodactylus binotatus", "Anisodactylus binotatus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Bembidion obtusus", "Bembidion obtusum", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Bembidon obtusum", "Bembidion obtusum", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Bembidionproperans", "Bembidion properans", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Bembidion teracolum", "Bembidion tetracolum", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Haarpalus affinis", "Harpalus affinis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Harpalus 1cm,schwarz", "Harpalus", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Harp disting", "Harpalus distinguendes", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Harpalus distiguendes", "Harpalus distinguendes", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Harpalus runzellier Kopf", "Harpalus", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "Harpalus signa", "Harpalus signaticornis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Harpalus signati", "Harpalus signaticornis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Hatpalus signaticornis", "Harpalus signaticornis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Hatpalus signaticornis", "Harpalus signaticornis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Limodromus asiimili", "Limodromus assimilis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Limodromus assilimis", "Limodromus assimilis", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Loricera pericornis", "Loricera pilicornis", Käferart))%>%
  mutate(Käferart = ifelse(Käferart == "Microcelestes minutulus", "Microlestes minutulus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Microlestes minu", "Microlestes minutulus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Notiphilius biggutatus", "Notiophilus biguttatus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Notiphilius bigutatus", "Notiophilus biguttatus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Pterostichus antracinus", "Pterostichus anthracinus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Trechus qu", "Trechus quadristriatus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Trechus qua", "Trechus quadristriatus", Käferart)) %>% 
  mutate(Käferart = ifelse(Käferart == "Trechus quadri", "Trechus quadristriatus", Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "?", NA, Käferart))%>% 
  mutate(Käferart = ifelse(Käferart == "schwarz, mittel ?", NA, Käferart))

# check for typos
unique(beetle_corrected$Käferart) %>% sort()

# make column for genus and species
# this might give a warning, as in some rows the species is missing
# in order to inspect those in Console type e.g. beetle_split[121,]
beetle_split = beetle_corrected %>%
  separate(Käferart, c("genus", "species"), " ", remove = FALSE)

# check for typos in genus
unique(beetle_split$genus) %>% sort()

####### adapt to your needs
# correct typo and drop beetles with unknown genus
beetle_split = beetle_split%>% 
  mutate(genus = ifelse(genus == "Notiphilius", "Notiophilus", genus))%>% 
  drop_na(genus)


# adapt to my notation
beetle_adapted = beetle_split  %>%
  mutate(trap_row = ifelse(Distanz == "a", "1stRow", Distanz)) %>%
  mutate(trap_row = ifelse(trap_row == "b", "2ndRow", trap_row))%>%
  mutate(orientation = ifelse(Glas == 1, "out", Glas))%>%
  mutate(orientation = ifelse(orientation == 2, "circle", orientation))%>%
  mutate(orientation = ifelse(orientation == 3, "in", orientation))%>%
  mutate(orientation = ifelse(orientation == 4, "circle", orientation))%>%
  mutate(trap_row = as.factor(trap_row))%>%
  mutate(orientation = as.factor(orientation))

# quality check in Console
summary(beetle_adapted)

# sum different Transekts
beetle_sum = beetle_adapted %>%
  group_by(trap_row, orientation, genus, species, Käferart) %>%
  summarise(n_trapped_beetles = sum(Anzahl))

# make a column for each position
beetle_position = beetle_sum %>%
  unite(position, c(orientation, trap_row)) %>%
  spread(position, n_trapped_beetles)

# replace NA counts with 0
beetle_zero <- beetle_position %>%
  mutate(circle_1stRow = replace_na(circle_1stRow, 0))%>%
  mutate(circle_2ndRow = replace_na(circle_2ndRow, 0))%>%
  mutate(in_1stRow = replace_na(in_1stRow, 0))%>%
  mutate(in_2ndRow = replace_na(in_2ndRow, 0))%>%
  mutate(out_1stRow = replace_na(out_1stRow, 0))%>%
  mutate(out_2ndRow = replace_na(out_2ndRow, 0))

# add columns for relative trapping pattern
real.rel = beetle_zero%>%
  mutate(count_abs = circle_1stRow + circle_2ndRow +in_1stRow + in_2ndRow + out_1stRow +  out_2ndRow) %>%
  mutate(circle_1stRow_rel = ifelse(is.na(circle_1stRow/count_abs), 0, circle_1stRow/count_abs))%>%
  mutate(circle_2ndRow_rel = ifelse(is.na(circle_2ndRow/count_abs), 0, circle_2ndRow/count_abs))%>%
  mutate(in_1stRow_rel = ifelse(is.na(in_1stRow/count_abs), 0, in_1stRow/count_abs))%>%
  mutate(in_2ndRow_rel = ifelse(is.na(in_2ndRow/count_abs), 0, in_2ndRow/count_abs))%>%
  mutate(out_1stRow_rel = ifelse(is.na(out_1stRow/count_abs), 0, out_1stRow/count_abs))%>%
  mutate(out_2ndRow_rel = ifelse(is.na(out_2ndRow/count_abs), 0, out_2ndRow/count_abs))%>%
  mutate(count_rel = circle_1stRow_rel + circle_2ndRow_rel +in_1stRow_rel + in_2ndRow_rel + out_1stRow_rel + out_2ndRow_rel)

#control
summary(real.rel)

save(real.rel, file = paste0(path_data, "/dataReal.RData"))
