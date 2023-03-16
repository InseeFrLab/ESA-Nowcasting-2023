#!/bin/bash
git config --global credential.helper store

mc cp -r s3/projet-esa-nowcasting/targets-data/ .
