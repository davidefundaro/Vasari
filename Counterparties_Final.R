colnames(Anagrafiche_Raw) <- tolower(colnames(Anagrafiche_Raw))
Anagrafiche_Raw[] <- lapply(Anagrafiche_Raw,tolower)
#Posizioni_selected_raw <- Posizioni_selected_raw %>% distinct()

names(Anagrafiche_Raw) <- colname_function(names(Anagrafiche_Raw))


Counterparties <- Anagrafiche_Raw %>% select(ndg,intestaz..unica...rag..soc.,forma.giuridica,
                                             `partita.iva.(aziende)`,`codice.fiscale.(persona.fisica)`,
                                             provincia,comune,ndg_1,nome_1,codice_fiscale_1,
                                             ndg_2,nome_2,codice_fiscale_2,ndg_3,nome_3,codice_fiscale_2,
                                             ndg_4,nome_4,codice_fiscale_4,ndg_5,nome_5,codice_fiscale_5,`tipo.anagrafica.(d=debitore..g=garante..dg=debitore.e.garante)`)

Counterparties <- Counterparties %>% rename(names = intestaz..unica...rag..soc.)

Counterparties$names <- gsub("\\s+"," ",Counterparties$names)

selected_columns <- c("nome_1", "nome_2", "nome_3", "nome_4", "nome_5")

# Count non-empty values in each row of the selected columns and return 1 if none found
Counterparties$non_empty_count <- rowSums(!is.na(Counterparties[selected_columns]))

# Replace 0 counts with 1
Counterparties$non_empty_count <- ifelse(Counterparties$non_empty_count == 0, 1, Counterparties$non_empty_count)


Counterparties <- Counterparties %>% mutate(id.counterparty = paste0("c",row_number()))

Counterparties_test <- Counterparties %>% select(id.counterparty,ndg,`tipo.anagrafica.(d=debitore..g=garante..dg=debitore.e.garante)`
                                                 ,names,non_empty_count)

Counterparties_final <- Counterparties_test %>% rename(id.bor = ndg,role = `tipo.anagrafica.(d=debitore..g=garante..dg=debitore.e.garante)`
                                                       ,name = names,n_entities = non_empty_count)


Garanzie_new <- Garanzie_Raw %>% select(NDG,`Intestazione garante`)

Garanzie_new$`Intestazione garante` <- gsub("\\s+"," ",Garanzie_new$`Intestazione garante`)

Garanzie_new <- Garanzie_new %>% distinct()

Garanzie_new <- Garanzie_new %>% rename(names = `Intestazione garante`,ndg = NDG)

Garanzie_new$ndg <- Garanzie_new$ndg %>% as.character()

colnames(Garanzie_new) <- tolower(colnames(Garanzie_new))
Garanzie_new[] <- lapply(Garanzie_new,tolower)

Garanzie_new$names <- gsub(" e ", ", ", Garanzie_new$names)

Garanzie_new <- divide_column_by_character(Garanzie_new,"names",",")

Garanzie_Raw <- Garanzie_Raw %>%  rename(id.bor = `NDG garante`) 

Garanzie_Raw$id.bor <- as.character(Garanzie_Raw$id.bor)

dg_table <-  Counterparties_final %>% filter(role == "dg")

dg_guarantor <-  left_join(dg_table,Garanzie_Raw, by = "id.bor")


dg_guarantor <- dg_guarantor %>% select(id.counterparty,NDG,role,name,n_entities)

dg_guarantor <- dg_guarantor %>% rename(id.bor = NDG)

dg_guarantor$id.bor <- as.character(dg_guarantor$id.bor)

Counterparties_final <- Counterparties_final %>%
  mutate(role = case_when(
    role %in% c("d", "dg") ~ "borrower",
    role == "g" ~ "guarantor",
    TRUE ~ as.factor(role)
  ))

merged_table <-  bind_rows(dg_guarantor,Counterparties_final)


merged_table <- merged_table %>%
  mutate(role = case_when(
    role == "dg" ~ "guarantor",
    TRUE ~ as.factor(role)
  ))


merged_table <-  merged_table %>% select(-id.counterparty)

merged_table <- merged_table %>% distinct()

merged_table <- merged_table %>% mutate(id.counterparty = paste0("c",row_number()))

merged_table$id.group <- NA_real_

merged_table$flag.imputed <- NA_real_

merged_table$name <- gsub("\\s+"," ",merged_table$name)

merged_table$name <- gsub("\\*\\.\\*|time house", " ",merged_table$name)

merged_table$name <- gsub("\\s+$", "", merged_table$name)

counterparties_finished <- merged_table

counterparties_finished <- counterparties_finished %>% rename(n.entities = n_entities)


######################################
##### CHANGING DATA TYPE 
#####################################

columns_to_convert <- c("role")
counterparties_finished[columns_to_convert] <- lapply(counterparties_finished[columns_to_convert], as.factor)

counterparties_finished$id.group <- as.character(counterparties_finished$id.group)

columns_to_convert <- c("n.entities", "flag.imputed")
counterparties_finished[columns_to_convert] <- lapply(counterparties_finished[columns_to_convert], as.integer)
