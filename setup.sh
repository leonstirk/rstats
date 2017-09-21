#!/bin/bash

# Setup new user to run browser Rstudio environment

sudo useradd rstudio
sudo mkdir /home/rstudio
sudo passwd rstudio
sudo chmod -R 0777 /home/rstudio
