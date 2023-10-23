check_primary_key <- function(table, column) 
  if(column %in% colnames(table)) {
    if (length(unique(table[[column]])) == nrow(table) && !any(is.na(table[[column]]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return('Error: column not found')
  }


#clean column names and turn into lower case
colname_function <- function(name) {
  name <- gsub('[/, ]', '.', name)
  name <- tolower(name)
  return(name)
}



check_col1_in_col2 <- function(col1, col2){
  #lista <- c()
  for(i in 1:length(col1)){
    if(col1[i] %in% col2){
      check <- TRUE
    } else {
      check <- FALSE
      #lista <- c(lista,col1[i])
    }
  }
  result <- check
  #result <- c(check,lista)
  return(result)
}








check_dependencies<- function(table, key) {
  lista_num <- list()
  for (col in names(table)) {
    grouped_data <- split(table[[col]], table[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (all(dependency_check ==1 )) {
      lista_num[[col]] <- 1
    } else {
      lista_num[[col]] <- length(dependency_check) / sum(dependency_check)
    }
  }
  
  result <- lista_num
  return(result)
}


find_dependencies_matrix <- function(table) {
  num_cols <- ncol(table)
  dependency_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  colnames(dependency_matrix) <- colnames(table)
  rownames(dependency_matrix) <- colnames(table)
  
  for (col_2 in colnames(dependency_matrix)) {
    new_column <- check_dependencies(table, col_2) 
    new_column <- unlist(new_column)
    dependency_matrix[,col_2] <- new_column
  }
  return(dependency_matrix)
}


map_types_Loans <- function(table, table_types){
  table_types$type <- str_trim(tolower(table_types$type))
  
  table <- table %>% mutate(type = gsub("n\\..*", "", type))
  table$type <- str_trim(tolower(table$type))
  
  table_mapped <- table %>%
    left_join(table_types, by = "type")
  table_mapped <- table_mapped %>% select(-type)
  table_mapped <- table_mapped %>% relocate(Type, .before = status) %>% rename(type = Type)
  return(table_mapped)
}



divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>% mutate_all(~str_trim(., 'right'))
}





add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}

create_region_city <- function(table, key1) {
  for (j in 1:length(key1)) {                 
    if (is.na(key1[j])) {
      table$region[j] <- NA  
    } else {
      matching_row <- subset(GeoData, city == toupper(key1[j]))
      if (nrow(matching_row) > 0) {
        table$region[j] <- matching_row$region
      } else {
        table$region[j] <- NA
      }
    }
  }
  return(table)
}

create_area_city <- function(table, key1) {
  for (j in 1:length(key1)) {                 
    if (is.na(key1[j])) {
      table$area[j] <- NA  
    } else {
      matching_row <- subset(GeoData, city == toupper(key1[j]))
      if (nrow(matching_row) > 0) {
        table$area[j] <- matching_row$area
      } else {
        table$area[j] <- NA
      }
    }
  }
  return(table)
}