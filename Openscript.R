#Makin Matrices

dicho<-sample(x=c(0:1),size=10000,replace=TRUE)
net1<-matrix(data=dicho,100,100)
diag(net1)=0
sumnet1<-rowSums(data[net1,1:100])
net1b<-t(net1)
.list<-net1,net1b

net2<-matrix(runif(n=10000,min=0,max=10),nrow=100,ncol=100)
diag(net2)=0

#small for playing with triads

dichos<-sample(x=c(0:1),size=16,replace=TRUE)
net3<-matrix(data=dichos,4,4)
net3t<-t(net3)
diag(net3)=0
net3s<-net3+net3t
net3s<-ifelse(net3s==2,1,0)
netGs<-graph_from_adjacency_matrix(net3s)
plot(netGs)

set.seed(123643)
net4<-matrix(sample(0:1,100,replace=TRUE),nrow=10,ncol=10)
diag(net4)=0
net4s<-net4+t(net4)
net4s<-ifelse(net4s==2,1,0)
net4s
netG<-graph_from_adjacency_matrix(net4s)
netGa<-graph_from_adjacency_matrix(net4)
plot(netG)
dyad_census(netGa)
triad_census(netGa)

install.packages("sna")
require(sna)
?sna
triad.census(net4s)

#webscrape test
fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

fsave <- function(x, file = NULL, location = "./data/processed/") {
  ifelse(!dir.exists("data"), dir.create("data"), FALSE)
  ifelse(!dir.exists("data/processed"), dir.create("data/processed"), FALSE)
  if (is.null(file))
    file = deparse(substitute(x))
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, ".rda", sep = "")
  save(x, file = totalname)  #need to fix if file is reloaded as input name, not as x. 
}

fload <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

fshowdf <- function(x, ...) {
  knitr::kable(x, digits = 2, "html", ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
    kableExtra::scroll_box(width = "100%", height = "300px")
}

install.packages("rvest")
require(rvest)
install.packages("jsonlite")
require(jsonlite)
require(dplyr)
jt <- read_html("https://api.openalex.org/authors?search=Vincent+Buskens") %>%
  html_text2()
jt_json <- fromJSON("https://api.openalex.org/authors?search=Vincent+Buskens", simplifyVector = FALSE)
glimpse(jt_json, max.level = 1)

jt_json$results$affiliations



#Scraping member info from the ICS website
require(rvest)
require(xml2)
require(dplyr)

#Faculty
icsfac<-read_html("https://ics-graduateschool.nl/faculty/")
icsfac<-icsfac %>%
  html_nodes("body") %>%
  xml_find_all("//tbody") %>%
  html_table()
icsfac<-bind_rows(icsfac)
icsfac$X5<-"Faculty"

#Postdocs
icspost<-read_html("https://ics-graduateschool.nl/postdocs/")
icspost<-icspost %>%
  html_nodes("body") %>%
  xml_find_all("//tbody") %>%
  html_table()
icspost<-bind_rows(icspost)
icspost$X5<-"Postdoc"

#PhDs
icsphd<-read_html("https://ics-graduateschool.nl/phds/")
icsphd<-icsphd %>%
  html_nodes("body") %>%
  xml_find_all("//tbody") %>%
  html_table()
icsphd<-bind_rows(icsphd)
icsphd$X5<-"PhD"

#Merging into one table
ics<-rbind(icsfac,icspost)
ics<-rbind(ics,icsphd)
ics

