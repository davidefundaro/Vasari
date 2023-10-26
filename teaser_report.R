wb <- createWorkbook()
addWorksheet(wb, sheetName = "Data Loans")
addWorksheet(wb, sheetName = "Data Borrowers")
addWorksheet(wb, sheetName = "Info Providing")


showGridLines(wb, sheet = 1, showGridLines = FALSE)
showGridLines(wb, sheet = 2, showGridLines = FALSE)
showGridLines(wb, sheet = 3, showGridLines = FALSE)


setColWidths(wb, sheet=1,cols = 1:7,widths = "auto")
setColWidths(wb, sheet=2,cols = 1:7,widths = "auto")
setColWidths(wb, sheet=3,cols = 1:7,widths = "auto")


percentage_rows <- createStyle(numFmt = "0.00%",fontSize = 10,halign = "right",valign = "center",fontColour = "black",wrapText = FALSE)
Milion_rows <- createStyle(
  numFmt = "0.0,,\"M\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
thousands_rows <- createStyle(
  numFmt = "0.0,\"k\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)


stringa <- "Loans Report \n ciao samet \n "
lines <- unlist(strsplit(stringa, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,1,df$Text[i],1,i)
}


writeData(wb, sheet = "Data Loans", x = "Summary Statistics", startCol = 1, startRow = 5)
writeDataTable(wb, 1, x = r.introductionP6 , startRow = 6,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")


writeData(wb, sheet = "Data Loans", x = "Possible Primary Keys", startCol = 1, startRow = 13)
writeDataTable(wb, 1, x = possible_keys_LOANS , startRow = 14,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Data Loans", x = "Profling Non Numeric Variables", startCol = 1, startRow = 23)
writeDataTable(wb, 1, x = Profile_LOANS , startRow = 24,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")


writeData(wb, sheet = "Data Loans", x = "Profiling Numeric Variables", startCol = 1, startRow = 35)
writeDataTable(wb, 1, x = Profile_Numeric , startRow = 36,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Data Borrowers", x = "Possible Primary Keys", startCol = 1, startRow = 5)
writeDataTable(wb, 2, x = possible_keys_COUNTERPARTIES , startRow = 6,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Data Borrowers", x = "Profiling", startCol = 1, startRow = 13)
writeDataTable(wb, 2, x = Profile_COUNTERPARTIES , startRow = 14,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Data Borrowers", x = "Number Of Borrowers By Province", startCol = 1, startRow = 26)
writeDataTable(wb, 2, x = r.p27.borrowersByProvince.head.support.table , startRow = 27,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Info Persone Fisiche", startCol = 1, startRow = 5)
writeDataTable(wb, 3, x = Profile_infoprov_pf , startRow = 6,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Info Persone Fisiche Variabili Numeriche", startCol = 1, startRow = 16)
writeDataTable(wb, 3, x = Profile_Numeric_infoprov , startRow = 17,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Info Persone Giuridiche", startCol = 1, startRow = 23)
writeDataTable(wb, 3, x = Profile_infoprov_pg , startRow = 24,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Average Income Net Per Type", startCol = 1, startRow = 35)
writeDataTable(wb, 3, x = table.info.income.solvency , startRow = 36,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Average Income Net Per Region", startCol = 1, startRow = 45)
writeDataTable(wb, 3, x = table.info.income.region , startRow = 46,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Percentage Of Status Cases By Corporate", startCol = 1, startRow = 53)
writeDataTable(wb, 3, x = table.info.piva.type , startRow = 54,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")

writeData(wb, sheet = "Info Providing", x = "Number Of Status Cases Per Region", startCol = 1, startRow = 61)
writeDataTable(wb, 3, x = table.info.piva.status.region , startRow = 62,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")


ggsave("Charts/solvency_class_n_cases.png",plot = chart.solvency.class.cases)
insertImage(wb,sheet = "Info Providing","Charts/solvency_class_n_cases.png",startCol = 3, startRow = 23, width = 6, height = 4.5, dpi = 300)

ggsave("Charts/borrowers_%_per_area.png",plot = r.p27.borrowerByArea)
insertImage(wb,sheet = "Data Borrowers","Charts/borrowers_%_per_area.png",startCol = 3, startRow = 23, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_per_area.png",plot = r.p27.gbvByArea)
insertImage(wb,sheet = "Data Borrowers","Charts/gbv_%_per_area.png",startCol = 10, startRow = 23, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_per_province.png",plot = r.p27.borrowersByProvince.top.5)
insertImage(wb,sheet = "Data Borrowers","Charts/gbv_%_per_province.png",startCol = 3, startRow = 46, width = 6, height = 4.5, dpi = 300)



ggsave("Charts/gbv_residual_%_loan_size.png",plot = r.p28.g.gbvByLoanSize)
insertImage(wb,sheet = "Data Loans","Charts/gbv_residual_%_loan_size.png",startCol = 10, startRow = 8, width = 6, height = 4.5, dpi = 300)

ggsave("Charts/histogram_loans_amount.png",plot = gg)
insertImage(wb,sheet = "Data Loans","Charts/histogram_loans_amount.png",startCol = 13, startRow = 30, width = 6, height = 4.5, dpi = 300)

saveWorkbook(wb, file = "File/Teaser.xlsx", overwrite = TRUE)
