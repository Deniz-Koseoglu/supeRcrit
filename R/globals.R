# Non-standard-evaluation column names used inside ggplot2::aes() (and similar) that R CMD
# check would otherwise report as "no visible binding for global variable". Declaring them
# here keeps `checking R code for possible problems` clean without importing rlang's .data.
utils::globalVariables(c("sigma", "pA", "compound"))
