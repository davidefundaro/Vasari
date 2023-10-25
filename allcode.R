#############################
### LOANS
############################


Posizioni_selected_raw <- Posizioni_Raw %>% select(`Numero rapporto`,NDG,
                                                   `Forma tecnica rapporto`,`Gross book value Totale`,
                                                   `Gross book value Capitale`,`Gross book value Spese`,
                                                   `Gross book value Interessi`,`Data di passaggio a sofferenza`)


### Clean columns and make lower case

colnames(Posizioni_selected_raw) <- tolower(colnames(Posizioni_selected_raw))
Posizioni_selected_raw[] <- lapply(Posizioni_selected_raw,tolower)
#Posizioni_selected_raw <- Posizioni_selected_raw %>% distinct()

names(Posizioni_selected_raw) <- colname_function(names(Posizioni_selected_raw))

#Posizioni_selected_raw$intestazione <- gsub("\\s+"," ",Posizioni_selected_raw$intestazione)

Posizioni_selected_raw <- Posizioni_selected_raw %>% rename(id.loans = numero.rapporto,id.bor = ndg,type = forma.tecnica.rapporto
                                                            ,gbv.original = gross.book.value.totale,principal = gross.book.value.capitale,
                                                            penalties = gross.book.value.spese,interest = gross.book.value.interessi,
                                                            date.status = data.di.passaggio.a.sofferenza)
columns_to_convert <- c("gbv.original","principal","interest","penalties")
Posizioni_selected_raw <- Posizioni_selected_raw %>% mutate(across(all_of(columns_to_convert),as.numeric))

Posizioni_selected_raw$gbv.residual <- Posizioni_selected_raw$gbv.original

Posizioni_selected_raw$date.status <- as.Date(Posizioni_selected_raw$date.status)

Posizioni_selected_raw$type <- as.factor(Posizioni_selected_raw$type)

#check_primary_key(Posizioni_selected_raw,"id.loans") TRUE id.loans is unique

Posizioni_selected_raw <- Posizioni_selected_raw %>% mutate(status = "bad",id.group=NA_real_,originator=NA_real_,ptf=NA_real_,cluster=NA_real_ ,
                                                            ptf=NA_real_,date.origination=NA_real_,date.last.act=NA_real_,
                                                            flag.imputed=NA_real_,expenses = penalties,penalties=NA_real_)


Posizioni_selected_raw <- Posizioni_selected_raw %>%
  mutate(type = case_when(
    type == "finanziamento" ~ "personal loans",
    type == "scopertocc" ~ "bank accounts",
    type == "mutuo" ~ "mortgages",
    TRUE ~ as.factor(type)
  ))

Loans <- Posizioni_selected_raw


###################################
### COUNTERPARTIES
##################################

colnames(Anagrafiche_Raw) <- tolower(colnames(Anagrafiche_Raw))
Anagrafiche_Raw[] <- lapply(Anagrafiche_Raw,tolower)
#Posizioni_selected_raw <- Posizioni_selected_raw %>% distinct()

names(Anagrafiche_Raw) <- colname_function(names(Anagrafiche_Raw))


Counterparties <- Anagrafiche_Raw %>% select(ndg,intestaz..unica...rag..soc.,forma.giuridica,
                                             `partita.iva.(aziende)`,`codice.fiscale.(persona.fisica)`,
                                             provincia,comune,ndg_1,nome_1,codice_fiscale_1,
                                             ndg_2,nome_2,codice_fiscale_2,ndg_3,nome_3,codice_fiscale_2,
                                             ndg_4,nome_4,codice_fiscale_4,ndg_5,nome_5,codice_fiscale_5,`tipo.anagrafica.(d=debitore..g=garante..dg=debitore.e.garante)`,data.di.nascita)

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

################################à
####à     ENTITIES 
########################à########

df <- Anagrafiche_Raw
# Fill in missing values in "nome_1" with values from "intestaz..unica...rag..soc."
df$nome_1 <- ifelse(is.na(df$nome_1), df$intestaz..unica...rag..soc., df$nome_1)
df$codice_fiscale_1 <-  ifelse(is.na(df$codice_fiscale_1), df$`codice.fiscale.(persona.fisica)`, df$codice_fiscale_1)
df$ndg_1 <- ifelse(df$ndg_1==0, df$ndg, df$ndg_1)

# Remove the "intestaz..unica...rag..soc." column
df <- df %>%
  select(-ndg,-`codice.fiscale.(persona.fisica)`,-intestaz..unica...rag..soc.)


##########################à

df1 <- df %>% select(ndg_1,nome_1,codice_fiscale_1,provincia,comune)
df2 <- df %>% select(ndg_2,nome_2,codice_fiscale_2,provincia,comune)
df3 <- df %>% select(ndg_3,nome_3,codice_fiscale_3,provincia,comune)
df4 <- df %>% select(ndg_4,nome_4,codice_fiscale_4,provincia,comune)
df5 <- df %>% select(ndg_5,nome_5,codice_fiscale_5,provincia,comune)

df1 <- df1 %>%
  rename(ndg = ndg_1,codice_fiscale = codice_fiscale_1,nome = nome_1)

df2 <- df2 %>%
  rename(ndg = ndg_2,codice_fiscale = codice_fiscale_2,nome = nome_2)

df3 <- df3 %>%
  rename(ndg = ndg_3,codice_fiscale = codice_fiscale_3,nome = nome_3)

df4 <- df4 %>%
  rename(ndg = ndg_4,codice_fiscale = codice_fiscale_4,nome = nome_4)  # Rename the same column name in df4 as in df1

df5 <- df5 %>%
  rename(ndg = ndg_5,codice_fiscale = codice_fiscale_5,nome = nome_5)

stacked_df <- bind_rows(df1, df2, df3, df4, df5)

filtered_df <- stacked_df %>%
  filter(rowSums(is.na(.)) < 2)

filtered_df$nome <- gsub("\\s+", " ",filtered_df$nome)

filtered_df$nome <- gsub("\\*\\.\\*|time house", " ",filtered_df$nome)

filtered_df$nome <- gsub("\\s+$", "", filtered_df$nome)

filtered_df <- filtered_df %>% distinct()

filtered_df <- filtered_df %>% mutate(id.entities = paste0("e",row_number()))

filtered_df <- filtered_df %>% rename(id.bor = ndg)

filtered_df <- filtered_df %>% rename(namer = nome)

entities <- filtered_df

####################################
###### LINK COUNTERPARTIES ENTITIES
####################################



test <- Anagrafiche_Raw

combine_until_na <- function(...) {
  non_na_values <- c(...)
  non_na_values <- non_na_values[!is.na(non_na_values)]
  return(paste(non_na_values, collapse = ", "))
}

# Unite the columns "nome_1" to "nome_5" into a single column "combined_names"
test <- test %>%
  unite(combined_names, c("nome_1", "nome_2", "nome_3", "nome_4", "nome_5"), sep = ", ", remove = FALSE, na.rm = TRUE) %>%
  mutate(combined_names = pmap_chr(select(., starts_with("nome_")), combine_until_na))

test <- test %>% rename(id.bor = ndg, name = combined_names)


counterparty_idbor_name <- test %>% select(id.bor,intestaz..unica...rag..soc.,name)

counterparty_idbor_name$name <- gsub("\\*\\.\\*|time house", " ",counterparty_idbor_name$name)
counterparty_idbor_name$name <- gsub("\\s+", " ",counterparty_idbor_name$name)
counterparty_idbor_name$name <- gsub("\\s+$", "", counterparty_idbor_name$name)

######## SE NON FUNZIONA FARE LA STESSA COSA PER INTESTAZIONE COLONNA #####

counterparty_idbor_name2 <- divide_column_by_character(counterparty_idbor_name,"name",",")

#counterparty_idbor_name2$intestaz..unica...rag..soc. <- gsub("\\s+", " ",counterparty_idbor_name2$intestaz..unica...rag..soc.)

#counterparty_idbor_name2$intestaz..unica...rag..soc. <- gsub("\\*\\.\\*|time house", "",counterparty_idbor_name2$intestaz..unica...rag..soc.)

counterparty_idbor_name2 <- counterparty_idbor_name2 %>% rename(name=intestaz..unica...rag..soc.,name.entities=name)


counterparty_idbor_name3 <- left_join(merged_table,counterparty_idbor_name2, by = c("id.bor","name"))

counterparty_idbor_name4 <- counterparty_idbor_name3 %>% select(name,name.entities)


counterparty_idbor_name3 <- left_join(counterparty_idbor_name3,counterparty_idbor_name4,by = "name")


counterparty_idbor_name3 <- counterparty_idbor_name3 %>% select(-name.entities.x)

counterparty_idbor_name3 <- counterparty_idbor_name3 %>%
  filter(!is.na(name.entities.y))

# Fill in missing values in "nome_1" with values from "intestaz..unica...rag..soc."
counterparty_idbor_name3$name.entities.y <- ifelse(counterparty_idbor_name3$name.entities.y=="",counterparty_idbor_name3$name , counterparty_idbor_name3$name.entities.y)

counterparty_idbor_name3 <- counterparty_idbor_name3 %>% distinct()

counterparty_idbor_name3 <- counterparty_idbor_name3 %>% rename(namer = name.entities.y)


link_table <- left_join(counterparty_idbor_name3,filtered_df,by = "namer")


link_table_entities_counterparties <- link_table %>% select(id.counterparty,id.entities)

link_table_entities_counterparties <- link_table_entities_counterparties %>% distinct()


