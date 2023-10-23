

Loans_matrix_rounded <- find_dependencies_matrix(Anagrafiche_Raw)


Loans_matrix_rounded <- round(Loans_matrix_rounded,2)

Loans_matrix_rounded  <- Loans_matrix_rounded %>% as.data.frame()

column_sums_ones <- sapply(Loans_matrix_rounded, function(col) sum(col == 1))



Loans_matrix_rounded <- rbind(Loans_matrix_rounded, column_sums_ones)

rownames(Loans_matrix_rounded)[31] <- 'Sum_ones'

Loans_matrix_rounded <- Loans_matrix_rounded[c(31, 1:30), ]



Loans_matrix_ordered <- as.matrix(Loans_matrix_rounded)

col_index <- order(Loans_matrix_ordered[1,],  decreasing = TRUE)

Loans_matrix_ordered <- Loans_matrix_ordered[,col_index]

####################################
#------ CounterParties 
##################################

### Clean columns and make lower case

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

Counterparties_final <- Counterparties_final %>%
  mutate(role = case_when(
    role %in% c("d", "dg") ~ "borrower",
    role == "g" ~ "guarantor",
    TRUE ~ as.factor(role)
  ))



merged_table <-  merged_table %>% select(-id.counterparty)

merged_table <- merged_table %>% distinct()

merged_table <- merged_table %>% mutate(id.counterparty = paste0("c",row_number()))

merged_table$id.group <- NA_real_

merged_table$flag.imputed <- NA_real_
