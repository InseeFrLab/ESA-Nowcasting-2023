targets::tar_read(data)

rm(list=ls())

x <- targets::tar_read(data_info)

targets::tar_load_globals()

targets::tar_load(data_info)
targets::tar_load(challenges)

get_data_from_destatis(data_info)
get_data(data_info,challenges)
