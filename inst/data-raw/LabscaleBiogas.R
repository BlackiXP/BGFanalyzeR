## code to prepare `LabscaleBiogas` dataset goes here
# building the data set from external file  ####
LabscaleBiogas=from_AMPTSV2_report(
  ReactorLayout = c("2*Blank","Cellulose","2*S1 ctrl","2*S1 7d","2*S1 4d",
                    "2*S2 ctrl","2*S2 4d","2*S2 6d"),
  BlankLabel = "Blank",
  name = "LabscaleBiogas",
  ProcessTemp = 42,
  InocToSubRatio = 2,
  path = base::system.file("extdata","AMPTSV2.csv",package = "BGFanalyzeR"))

usethis::use_data(LabscaleBiogas, overwrite = TRUE)

