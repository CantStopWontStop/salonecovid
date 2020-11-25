
library(rsconnect)
rsconnect::setAccountInfo(name='afromation',
                          token='CA4AE543787019784C2097D0A8F8C3ED',
                          secret='<SECRET>')

rsconnect::deployApp('path/to/your/app')

devtools::install_github("rstudio/rsconnect")
1