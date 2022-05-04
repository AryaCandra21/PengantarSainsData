# load library
library(tidyverse)
library(rvest)
library(httr)

# buat fungsi mendapatkan tabel
dapatkan_tabel <- function(link) {
  json_web <- GET(link)
  json_web
  
  coba <- content(json_web, as = "text", simplifyVector = T)
  
  hasil_tabel <- rjson::fromJSON(coba) %>%
    map_df(~as_tibble(t(.))) %>%
    mutate(across(everything(), .fns = as.character))
  return(hasil_tabel)
}

# dapatkan tabel untuk seluruh Kabupaten di Jawa Tengah
tabel_kabupaten <- dapatkan_tabel("https://dapo.kemdikbud.go.id/rekap/dataPD?id_level_wilayah=1&kode_wilayah=030000&semester_id=20212")
tabel_kabupaten

# writexl::write_xlsx(tabel_kabupaten,
#                     "Project/tabel_kabupaten.xlsx")

# kode kab
df_kodekab <- tabel_kabupaten %>%
  select(nama, kode_wilayah) %>%
  mutate(
    kode_wilayah = str_trim(kode_wilayah)
  )
df_kodekab

link_kab <- paste0("https://dapo.kemdikbud.go.id/rekap/dataPD?id_level_wilayah=2&kode_wilayah=",df_kodekab$kode_wilayah,"&semester_id=20212")
link_kab



tabel_kecamatan <- tibble()
# cari kecamatan di seluruh Jawa Tengah
for(kab in link_kab) {
  tabel_kecamatan <- rbind(tabel_kecamatan, dapatkan_tabel(kab))
}
tabel_kecamatan

# kode kecamatan
df_kodekec <- tabel_kecamatan %>%
  select(nama, kode_wilayah) %>%
  mutate(
    kode_wilayah = str_trim(kode_wilayah)
  )

link_kec  <- paste0("https://dapo.kemdikbud.go.id/rekap/progresSP?id_level_wilayah=3&kode_wilayah=", df_kodekec$kode_wilayah, "&semester_id=20212")
link_kec

tabel_sekolah1 <- tibble()
# cari sekolah di seluruh kecamatan di Jawa Tengah part 1
for(kec in link_kec[1:87]) {
  tabel_sekolah1 <- rbind(tabel_sekolah1, dapatkan_tabel(kec))
}
tabel_sekolah1

# untuk yang Kabupaten tegal kita buat pengecualian dengan link kec idx 88
tabel_sekolah2 <- tibble()
# cari sekolah di seluruh kecamatan di Jawa Tengah part 2
for(kec in link_kec[89:576]) {
  tabel_sekolah2 <- rbind(tabel_sekolah2, dapatkan_tabel(kec))
}
tabel_sekolah2

# kita handle yang error
# link tabel yang error dari https://dapo.kemdikbud.go.id/rekap/progresSP?id_level_wilayah=3&kode_wilayah=032801&semester_id=20212
# kunjungi manual lalu copy paste ke error table
tabel_error <- rjson::fromJSON(file = "Project/ErrorTable.txt")
tabel_error

hasil_tabel_error <- tabel_error %>%
  map_df(~as_tibble(t(.))) %>%
  mutate(across(everything(), .fns = as.character))
hasil_tabel_error

############# Terakhir

# kita gabungkan semua tabel

tabel_sekolah_full <- bind_rows(tabel_sekolah1, hasil_tabel_error, tabel_sekolah2)
tabel_sekolah_full

# simpan
writexl::write_xlsx(tabel_sekolah_full,
                    "Project/Sekolah_JawaTengah.xlsx")
