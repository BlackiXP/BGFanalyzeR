## code to prepare `BGF_defaultcolors` dataset goes here
# define a coloring palette ####
grays=hcl.colors(8,"Grays")[c(2,4,6)]

lightblues=hcl.colors(24,"Blues")[c(14,17,20)]

darkgreens=hcl.colors(24,"Greens")[c(4,7,10)]

purples=hcl.colors(8,"Purples")[c(2,4,6)]

oranges=hcl.colors(8,"Oranges")[c(2,4,6)]

darkblues=hcl.colors(24,"Blues")[c(4,7,10)]

lightgreens=hcl.colors(24,"Greens")[c(14,17,20)]

browns=hcl.colors(16,"BrwnYl")[c(2,3,6)]

BGF_defaultcolors=c(grays,darkblues,darkgreens,purples,oranges,lightblues,lightgreens,browns)

usethis::use_data(BGF_defaultcolors, overwrite = TRUE)

