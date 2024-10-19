install.packages('rsconnect')
rsconnect::setAccountInfo(name='slalwani1214',
                          token='BE83FA69EC7EC3018048C55ECCC75B09',
                          secret='oqlpiiejX9rWrIiOsHszk3+EFSDC4E9MVQWQdBei')
library(rsconnect)
rsconnect::deployApp("~/Desktop/my_shiny_app")