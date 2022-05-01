#load library
library(tidyverse)
library(rvest)

#buat link
vector_link <- vector()
for (i in seq(1, 901, 100)) {
  vector_link <- c(vector_link,
                   paste0("https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating,desc&count=100&start=",
                          i,
                   "&ref_=adv_nxt"))
}
vector_link

#scrapping
nodes_read <- function(page, class) {
  return(page %>%
           html_nodes(class) %>%
           html_text())
}

web_scrapping <- function(link) {
  web_page <- read_html(link)
  judul <- nodes_read(web_page,
                      ".lister-item-header a")
  tahun <- nodes_read(web_page,
                      ".text-muted.unbold")
  duration <- nodes_read(web_page,
                         ".runtime")
  genre <- nodes_read(web_page,
                      ".genre")
  rating <- nodes_read(web_page,
                       ".ratings-imdb-rating strong")
  voters <- nodes_read(web_page,
                       ".sort-num_votes-visible span:nth-child(2)")
  director_starts <- nodes_read(web_page,
                                ".text-muted+ p")
  return(tibble(
    judul, tahun, duration,
    genre, rating,
    voters, director_starts)
    )
}

hasil <- tibble()
for (link in vector_link) {
  hasil <- bind_rows(hasil,
                     web_scrapping(link))
}

hasil <- hasil %>%
  mutate(
    tahun = str_extract(tahun,
                        "\\d+"),
    duration = str_extract(duration,
                           "\\d+"),
    rating = as.double(rating),
    voters = str_remove_all(voters,
                            ","),
    voters = as.numeric(voters)
  )

hasil <- hasil %>%
  mutate(
    director_starts = str_trim(director_starts,
                               "both")) %>%
  separate(
    col = director_starts,
    into = c("director", "stars"),
    sep = "\\|")

hasil <- hasil %>%
  mutate(
    director = str_trim(director, "both"),
    stars = str_trim(stars, "both"),
    director = str_remove(director,
                          "(Director:)|(Directors:)"),
    stars = str_remove(stars,
                       "(Star:)|(Stars:)"))
  
hasil <- hasil %>%
  mutate(
    genre = str_replace_all(genre, "\n", ""),
    director = str_replace_all(director, "\n", ""),
    stars = str_replace_all(stars, "\n", ""),
    tahun = as.integer(tahun),
    duration = as.integer(duration)) %>%
  rename(
    duration_min = duration
  )
#hasil
# writexl::write_xlsx(hasil,
#                     "webScrappingIMDB.xlsx")
