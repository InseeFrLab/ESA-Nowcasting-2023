FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

WORKDIR ${HOME}/ESA-Nowcasting-2023
COPY . .

# Install minimal python
RUN apt-get update && \
    apt-get install -y python3-pip libcurl4-openssl-dev && \
    # Install essential Python packages
    pip install -r requirements.txt --extra-index-url https://download.pytorch.org/whl/cpu && \
    # Configure renv to use RSPM to download packages by default
    echo 'options(renv.config.repos.override = getOption("repos"))' >> ${R_HOME}/etc/Rprofile.site && \
    # Install R packages
    Rscript -e "renv::restore()" && \
    # Fix permissions
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
