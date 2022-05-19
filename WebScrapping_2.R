# load library
library(tidyverse)
library(httr)
library(rvest)

# lakukan webscrapping
links <- paste0("https://www.the-numbers.com/movie/budgets/all/",
               seq(1, 6201, 100))

# dapatkan tabel
dapatkan_tabel <- function(link) {
  return(
    read_html(link) %>%
      html_table()
  )
}

tabel_movie <- list()
for (i in seq_along(links)) {
  tabel_movie[i] <- dapatkan_tabel(links[i])
}

tabel_movie <- tabel_movie %>%
  map_df(select,
         ReleaseDate, Movie, ProductionBudget,
         DomesticGross, WorldwideGross)
# tabel_movie_save_lagi <- tabel_movie_save %>%
#   bind_rows(tabel_movie)
# tabel_movie_save_lagi

# writexl::write_xlsx(tabel_movie,
#                     "ProjectPSD/DataFilmNumbers/tabel_movie.xlsx")


# Informasi masih kurang

informasi_lagi <- function(link) {
  
  webpage_cobalagi <- read_html(link)
  
  duration <- webpage_cobalagi %>%
    html_nodes("h2+ table tr:nth-child(5) td+ td") %>%
    html_text() %>%
    pluck(1)
  
  genre <- webpage_cobalagi %>%
    html_nodes("tr:nth-child(10) td+ td a") %>%
    html_text() %>%
    pluck(1)
  
  prod_country <- webpage_cobalagi %>%
    html_nodes("tr:nth-child(14) td+ td a") %>%
    html_text() %>%
    pluck(1)
  
  rating <- webpage_cobalagi %>%
    html_nodes("table:nth-child(13) tr:nth-child(4) a") %>%
    html_text() %>%
    pluck(1)
  
  return(
    tibble(
      duration = duration,
      genre = genre,
      prod_country = prod_country,
      rating = rating
    )
  )
}

page_site <- paste0(
  "https://www.the-numbers.com/movie/budgets/all/",
  seq(1, 6201, 100))

# untuk setiap halaman pada situs
for (i in 3:8) {
  # ekstrak semua movie pada halaman tersebut
  films <- read_html(page_site[i]) %>%
    html_nodes("#page_filling_chart b a") %>%
    str_extract('/movie/.+summary')
  
  links_films <- paste0("https://www.the-numbers.com/",
                        films)
  
  # untuk setiap links film pada satu halaman, kita masuki webnya satu per satu
  tabel_tambahan <- tibble()
  for (link in links_films) {
    tabel_tambahan <- tabel_tambahan %>%
      bind_rows(informasi_lagi(link))
  }
  
  # untuk setiap iterasi halaman, kita simpan dalam bentuk excel
  writexl::write_xlsx(tabel_tambahan,
                      str_glue("ProjectPSD/DataFilmNumbers/tabel_tambahan{i}.xlsx"))
}
