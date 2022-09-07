# mt4608
Package illustrating some ideas in Sampling Theory, for use with University of St Andrews module MT4608 Sampling Theory.

This should install it: 

pkgs <- c("devtools")
options(warn = -1)
for (i in pkgs){
    if (!require(i, quietly = TRUE, character.only = TRUE)){
        install.packages(i)
    }
}devtools::install_github("david-borchers/sampling",build = TRUE)
