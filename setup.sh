#!/bin/bash
git clone https://github.com/InseeFrLab/ESA-Nowcasting-2023.git

cd ESA-Nowcasting-2023

git config --global credential.helper store

mc cp -r s3/projet-esa-nowcasting/targets-data/ .
