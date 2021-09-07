

# numeric double columns from CHE and CHT tables
dbl_cols <- tbl_pfb %>% 
  colnames() %>% 
  grep(".x", ., value = TRUE) %>% 
  stringr::str_replace(., ".x", "") %>% 
  noquote()


# merge numeric double columns in a sensible way
for (col in 1:length(dbl_cols)) {
  # browser()
  # if all values from given soil property in CHE table are equal to those in CHT table...
  ifelse(nrow(tbl_pfb2 %>% 
                select(paste0(dbl_cols[[col]], ".x"), paste0(dbl_cols[[col]], ".y")) %>%
                filter(!paste0(dbl_cols[[col]], ".x") %in% NA) %>%
                mutate(diff_x.y = paste0(dbl_cols[[col]], ".x") - paste0(dbl_cols[[col]], ".y")) %>%
                filter(!diff_x.y %in% 0)
  ) == 0,
  # ...then soil property from CHE table is dropped
  tbl_pfb2 <- tbl_pfb2 %>%
    select(-paste0(dbl_cols[[col]], ".x")),
  # otherwise average of each duplicate measurement is taken
  tbl_pfb2 <- tbl_pfb2 %>% 
    rowwise() %>% 
    mutate(dbl_cols[[col]] == mean(c(paste0(dbl_cols[[col]], ".x"),
                                     paste0(dbl_cols[[col]], ".y")),
                                   na.rm = TRUE)) %>% 
    select(-c(paste0(dbl_cols[[col]], ".x"), paste0(dbl_cols[[col]], ".y")))
  )
}

for (col in 1:length(dbl_cols)) {
  # browser()
  # if all values from given soil property in CHE table are equal to those in CHT table...
  tbl_pfb2 <- tbl_pfb2 %>% 
    mutate(dbl_cols[[col]] == if_else(paste0(dbl_cols[[col]], ".x") == paste0(dbl_cols[[col]], ".y"),
                                      paste0(dbl_cols[[col]], ".y"),
                                      mean(c(paste0(dbl_cols[[col]], ".x"), paste0(dbl_cols[[col]], ".y")), na.rm = TRUE))) %>% 
    select(-c(paste0(dbl_cols[[col]], ".x"), paste0(dbl_cols[[col]], ".y")))
}


tbl_pfb2 %>% 
  mutate(CA_UIT = if_else(CA_UIT.x == CA_UIT.y,
                          CA_UIT.y,
                          mean(c(CA_UIT.x, CA_UIT.y), na.rm = TRUE))) %>% 
  select(-c(CA_UIT.x, CA_UIT.y))



ifelse(nrow(tbl_pfb %>% 
              select(CA_UIT.x, CA_UIT.y) %>%
              filter(!CA_UIT.x %in% NA) %>%
              mutate(diff_x.y = CA_UIT.x - CA_UIT.y) %>%
              filter(!diff_x.y %in% 0)
) == 0,
tbl_pfb2 <- tbl_pfb %>%
  select(-CA_UIT.x),
tbl_pfb2 <- tbl_pfb %>% 
  rowwise() %>% 
  mutate(CA_UIT = mean(c(CA_UIT.x, CA_UIT.y), na.rm = TRUE)) %>% 
  select(-c(CA_UIT.x, CA_UIT.y))
)