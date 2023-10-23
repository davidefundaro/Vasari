#Garanzie_unique <- Garanzie_Raw %>% distinct()

Garanzie_unique$role <- "guarantor"

Garanzie_new <- Garanzie_unique %>% select(NDG,`Intestazione garante`,role)

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


merged_table <-  bind_rows(dg_guarantor,Counterparties_final)


merged_table <- merged_table %>%
  mutate(role = case_when(
    role == "dg" ~ "guarantor",
    TRUE ~ as.factor(role)
  ))