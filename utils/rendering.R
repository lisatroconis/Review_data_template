knit_big_table <- function(table_to_print, .width = "1000px", .height = "200px") {
  table_to_print %>% 
    knitr::kable() %>% 
    kable_paper() %>%
    kableExtra::scroll_box(width = .width, height = .height)
}


print_log <- function(log_to_print, message) {
  if(nrow(log_to_print) == 0) {
    cat(message)
  } else {
    log_to_print %>% knit_big_table()
  }
}
