## code to prepare `LabscaleBiogasLayout` dataset goes here
# reactorLayout for LabscaleBiogas example data set ####
LabscaleBiogasLayout <- correct_RLayout(c("2*Blank","Cellulose",
                                          "2*S1 ctrl","2*S1 7d","2*S1 4d",
                                          "2*S2 ctrl","2*S2 4d","2*S2 6d"))

usethis::use_data(LabscaleBiogasLayout, overwrite = TRUE)

