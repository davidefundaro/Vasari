

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

df <- entities_complete 
df <- df %>% rename(namer = name)


################################################# HERE CHANGE
link_table <- left_join(counterparty_idbor_name3,df,by = "namer")


link_table_entities_counterparties <- link_table %>% select(id.counterparty,id.entities)

link_table_entities_counterparties <- link_table_entities_counterparties %>% distinct()


