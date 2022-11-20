#install.packages("devtools")
#devtools::install_github("dhopp1/nowcastLSTM")

#pip install nowcast_lstm
#pip install pmdarima
#pip install torch
#library(devtools)
library(nowcastLSTM)
nowcastLSTM::initialize_session(python_path = "/opt/mamba/bin/python")
library(dplyr)
data <- readr::read_csv("data.csv")
data <- data %>% 
  select(date, x_jp, x_world, x_de, x_uk, ipi_cn, x_vol_world2) # random subset of columns for simplicity
train_end_date <- "2017-12-01" # training data through 2017
training <- data %>% 
  filter(date <= train_end_date)
model <- nowcastLSTM::LSTM(data, "target_col_name", n_timesteps=12, python_model_name = "model") # default parameters with 12 timestep history

model <- nowcastLSTM::LSTM(
  data=training, 
  target_variable="x_world", 
  n_timesteps=12, 
  n_models=10, 
  train_episodes=50, 
  python_model_name="model"
)

