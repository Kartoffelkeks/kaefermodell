library(tidyverse)
source(here::here("utils.R"))
path_data <- here::here("../data")

read_data = function() {
  ####### adapt to your needs
  data1 = read_csv("bias_original.csv")
  data2 = read_csv("bias_optimal_offset.csv")
  data3 = read_csv("bias_higher_distance.csv")
  data4 = read_csv("bias_same_circular_arc.csv")
  
  ####### adapt to your needs
  data.joined = bind_rows(data1, data2, data3, data4)
  
  # control
  # any(is.na(data))
  
  # replace junk init sd when beetles were initialized randomly
  data.corrected = data.joined %>%
    dplyr::mutate(init_sd = ifelse(init_beetles == "random", NA, init_sd))%>%
    ungroup()%>%
    select(!run_number)
  
  #control
  summary(data.corrected)
  
  # calculate relative trapping pattern
  sim.bias.rel = data.corrected%>%
    mutate(count_abs = circle_1stRow + circle_2ndRow +in_1stRow + in_2ndRow + out_1stRow +  out_2ndRow) %>%
    mutate(circle_1stRow_rel = ifelse(is.na(circle_1stRow/count_abs), 0, circle_1stRow/count_abs))%>%
    mutate(circle_2ndRow_rel = ifelse(is.na(circle_2ndRow/count_abs), 0, circle_2ndRow/count_abs))%>%
    mutate(in_1stRow_rel = ifelse(is.na(in_1stRow/count_abs), 0, in_1stRow/count_abs))%>%
    mutate(in_2ndRow_rel = ifelse(is.na(in_2ndRow/count_abs), 0, in_2ndRow/count_abs))%>%
    mutate(out_1stRow_rel = ifelse(is.na(out_1stRow/count_abs), 0, out_1stRow/count_abs))%>%
    mutate(out_2ndRow_rel = ifelse(is.na(out_2ndRow/count_abs), 0, out_2ndRow/count_abs))%>%
    mutate(count_rel = circle_1stRow_rel + circle_2ndRow_rel +in_1stRow_rel + in_2ndRow_rel + out_1stRow_rel + out_2ndRow_rel)
  
  #control
  summary(sim.bias.rel)
  
  ####### adapt to your needs
  save(sim.bias.rel, file = paste0(path_data, "/dataBias.RData"))
}

read_data()
