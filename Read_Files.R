paths_content <- readLines("File/file_paths.txt")
Posizioni_line <- grep("^Posizioni", paths_content)
Posizioni_value <- sub("^Posizioni=\\s*", "", paths_content[Posizioni_line])
Posizioni_Raw <- read_excel(Posizioni_value, sheet = "Posizioni")

paths_content <- readLines("File/file_paths.txt")
Anagrafiche_line <- grep("^Anagrafiche", paths_content)
Anagrafiche_value <- sub("^Anagrafiche=\\s*", "", paths_content[Anagrafiche_line])
Anagrafiche_Raw <- read_excel(Anagrafiche_value, sheet = "Anagrafiche")

paths_content <- readLines("File/file_paths.txt")
Garanzie_line <- grep("^Garanzie", paths_content)
Garanzie_value <- sub("^Garanzie=\\s*", "", paths_content[Garanzie_line])
Garanzie_Raw <- read_excel(Garanzie_value, sheet = "Garanzie")


paths_content <- readLines("File/file_paths.txt")
GeoMetadata_line <- grep("^GeoMetadata", paths_content)
GeoMetadata_value <- sub("^GeoMetadata=\\s*", "", paths_content[GeoMetadata_line])
GeoData <- read_excel(GeoMetadata_value, sheet = "Geo")

paths_content <- readLines("File/file_paths.txt")
infoprovide_cf_line <- grep("^Foglio1", paths_content)
infoprovide_cf_value <- sub("^Foglio1=\\s*", "", paths_content[infoprovide_cf_line])
infoproviding_cf <- read_excel(infoprovide_cf_value, sheet = "Foglio1")

paths_content <- readLines("File/file_paths.txt")
infoprovide_piva_line <- grep("^Worksheet", paths_content)
infoprovide_piva_value <- sub("^Worksheet=\\s*", "", paths_content[infoprovide_piva_line])
infoproviding_piva <- read_excel(infoprovide_piva_value, sheet = "Worksheet")
