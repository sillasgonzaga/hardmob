library(rvest)
library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)
library(glue)
library(zoo)

remove_spec_html_char <- function(x){
  str_replace_all(x, "[\r]|[\t]", "")
}
