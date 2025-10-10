#openscript2

require(igraph)
g <- make_graph("Zachary")
plot(g)

V(g)$size = betweenness(g, normalized = T, directed = FALSE) * 60 + 10  #after some trial and error
plot(g, mode = "undirected")

#set.seed(2345)
#g1 <- layout_with_mds(g)
#plot(g1, layout=1, mode="undirected")

#vragen:
#hoe kan je bij het subsetten dingen selecteren die in [[1]] staan, zoals hieronder
#sociololist<-x$demographics$naam[x$demographics$[[1]]$discipline.24=="sociology"]
#wat doet die bracket in de definitie van het 'sociologists' object hieronder
#waarom komen mijn webpaginas niet consistent in het menu? vreselijk irritant!

demographics <- do.call(rbind.data.frame, x$demographics)
demographics <- demographics %>%
  mutate(Universiteit1.22 = replace(Universiteit1.22, is.na(Universiteit1.22), ""),
         Universiteit2.22 = replace(Universiteit2.22, is.na(Universiteit2.22), ""), 
         Universiteit1.24 = replace(Universiteit1.24, is.na(Universiteit1.24), ""), 
         Universiteit2.24 = replace(Universiteit2.24, is.na(Universiteit2.24), ""), 
         discipline.22 = replace(discipline.22, is.na(discipline.22), ""), 
         discipline.24 = replace(discipline.24, is.na(discipline.24), ""))

sample <- which(demographics$discipline.22 == "sociology" | demographics$discipline.24 == "sociology")

demographics_soc <- demographics[sample, ]
sociologists <- lapply(x, "[", sample)

#we hebben de politicologen nu verwijderd, wat willen we allemaal selecteren?
#uit demographics: Naam, Universiteit1.22, Universiteit2.22, Universiteit1.24, Universiteit2.24, 
                    #Functie.22, Functie.24, au_id
#uit scholars_oa: id, works_count, cited_by_count, counts_by_year?(dit zijn tabellen, beetje lastig)
#uit works: authors(sws want dat zijn de ties), topics(mss controle? sws meenemen want fascinerend)

#Scraping names of alumni from ICS website
require(rvest)
require(xml2)
require(dplyr)

icsgrad<-read_html("https://ics-graduateschool.nl/alumni-projects/")
icsgrad<-icsgrad %>%
  html_nodes("body") %>%
  xml_find_all("//tbody") %>%
  html_table()
icsgrad<-bind_rows(icsgrad)
icsnames<-as.list(icsgrad$X2)
dupname<-which(duplicated(icsnames))
icsnames<-icsnames[-dupname]

#Making the network from the scholars dataset
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
  totalname <- paste(location, file, "_", datename, ".rda", sep = "")
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

packages <- c("tidyverse", "scholar", "openalexR", "rvest", "jsonlite")
fpackage.check(packages)

setwd("C:/Users/Groot/Documents/RDirectory")
scholars <- fload("scholars_20240924.rda")

fcolnet <- function(data = scholars, discipline = "sociology", waves = list(c(2015,
                                                                                                 2018), c(2019, 2023)), type = c("first")) {
  
  # step 1
  demographics <- do.call(rbind.data.frame, data$demographics)
  demographics <- demographics %>%
    mutate(Universiteit1.22 = replace(Universiteit1.22, is.na(Universiteit1.22), ""), Universiteit2.22 = replace(Universiteit2.22,
                                                                                                                 is.na(Universiteit2.22), ""), Universiteit1.24 = replace(Universiteit1.24, is.na(Universiteit1.24),
                                                                                                                                                                          ""), Universiteit2.24 = replace(Universiteit2.24, is.na(Universiteit2.24), ""), discipline.22 = replace(discipline.22,
                                                                                                                                                                                                                                                                                  is.na(discipline.22), ""), discipline.24 = replace(discipline.24, is.na(discipline.24), ""))
  
  sample <- which(demographics$discipline.22 %in% discipline | demographics$discipline.24 %in% discipline)
  
  demographics_soc <- demographics[sample, ]
  scholars_sel <- lapply(scholars, "[", sample)
  
  # step 2
  ids <- demographics_soc$au_id
  nwaves <- length(waves)
  nets <- array(0, dim = c(nwaves, length(ids), length(ids)), dimnames = list(wave = 1:nwaves, ids,
                                                                              ids))
  dimnames(nets)
  
  # step 3
  df_works <- tibble(works_id = unlist(lapply(scholars_sel$work, function(l) l$id)), works_author = unlist(lapply(scholars_sel$work,
                                                                                                                  function(l) l$author), recursive = FALSE), works_year = unlist(lapply(scholars_sel$work, function(l) l$publication_year),
                                                                                                                                                                                 recursive = FALSE))
  
  df_works <- df_works[!duplicated(df_works), ]
  
  # step 4
  if (type == "first") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        ego <- df_works_w$works_author[i][[1]]$au_id[1]
        alters <- df_works_w$works_author[i][[1]]$au_id[-1]
        if (sum(ids %in% ego) > 0 & sum(ids %in% alters) > 0) {
          nets[j, which(ids %in% ego), which(ids %in% alters)] <- 1
        }
      }
    }
  }
  
  if (type == "last") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        ego <- rev(df_works_w$works_author[i][[1]]$au_id)[1]
        alters <- rev(df_works_w$works_author[i][[1]]$au_id)[-1]
        if (sum(ids %in% ego) > 0 & sum(ids %in% alters) > 0) {
          nets[j, which(ids %in% ego), which(ids %in% alters)] <- 1
        }
      }
    }
  }
  
  if (type == "all") {
    for (j in 1:nwaves) {
      df_works_w <- df_works[df_works$works_year >= waves[[j]][1] & df_works$works_year <= waves[[j]][2],
      ]
      for (i in 1:nrow(df_works_w)) {
        egos <- df_works_w$works_author[i][[1]]$au_id
        if (sum(ids %in% egos) > 0) {
          nets[j, which(ids %in% egos), which(ids %in% egos)] <- 1
        }
      }
    }
  }
  output <- list()
  output$data <- scholars_sel
  output$nets <- nets
  return(output)
}

#see if it works
test <- fcolnet(data = scholars, 
                discipline = "sociology", 
                waves = list(c(2015, 2018), c(2019, 2023)), 
                type = c("first"))

test_w1 <- igraph::graph_from_adjacency_matrix(
  test$nets[1,,], #for this example I take the first wave of data. (thus I select the array of networks and take the first matrix)
  mode = c("directed"),
  weighted = NULL,
  diag = FALSE,
  add.colnames = NULL,
  add.rownames = NULL
)

plot(test_w1,
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2,
     mode="undirected",
     )

noisolates <- rowSums(atmnet1_un, na.rm = T) > 0
# length(noisolates) sum(noisolates) if you select, select both correct nomination network as ego
# characteristics
atmnet1_un_sel <- atmnet1_un[noisolates, noisolates]
# if you are going to use the dataset keyf to add characteristics to the plot later, make sure to
# run the correct selection as well!!!
keyf_sel <- keyf[noisolates, ]

G2_sel <- graph_from_adjacency_matrix(atmnet1_un_sel, mode = "undirected", weighted = NULL, diag = TRUE,
                                      add.colnames = NA, add.rownames = NA)
G2_sel <- simplify(G2_sel)
plot(G2_sel, mode = "undirected")

#now to add the grad school affiliations, starting with ics
ics<-as.data.frame(icsgrad$X2)
ics<-ics[-c(1), ]
ics$ics<-1
names(ics)<-c("Naam","ICS")
icsmatch<-
demographics_soc<-merge(demographics_soc, ics, all=TRUE, by="Naam")

