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
