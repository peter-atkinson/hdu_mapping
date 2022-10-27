#runs prior to ui/server
#loads libraries, loads r files, different modules
#run global variables - common text, navigation system, 


library(shiny); library(lubridate);library(dplyr)


dseq <- seq(from = as.Date("01-01-2018", format = "%d-%m-%Y"), to = as.Date("31-12-2021", format = "%d-%m-%Y"), by = 1)
list <- readRDS("list")

postcodes.all <- data.frame(dseq, readRDS("currentmax.df"))


capitalcities.code <- c("0800")
which(list==capitalcities.code)

#column no. of capital cities
capital.codes <- c(35, 466, 671, 1365, 1798, 2139, 2523, 1)
capital.names <- c("Sydney", "Canberra", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin")
#capitalcities.df <- data.frame(capital.names, onoff)



# input <- 5000
# postcode <- ifelse(input < 1000, paste0("0", input), input)
# 
# z <- which(list==postcode)
# 
# loc <- data.frame(dseq, df[,z])
# loc$df...z.
# loc$hdubin <- ifelse(loc$df...z. > 130, 'red', 'grey')
# loc$trans <- ifelse(loc$df...z. > 130, 1, 0)