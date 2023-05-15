FROM inseefrlab/onyxia-rstudio:r4.2.3

WORKDIR ${HOME}/ESA-Nowcasting-2023
COPY . .

ENV RENV_CONFIG_REPOS_OVERRIDE=${CRAN}

RUN apt-get update && \
    # System libs
    apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libglpk-dev \
        openjdk-8-jdk && \
    # Install Python
    /rocker_scripts/install_python.sh && \
    # Install Python dependencies 
    pip install -r requirements.txt --extra-index-url https://download.pytorch.org/whl/cpu && \
    # Install R dependencies
    Rscript -e "renv::restore()" && \
    # Fix permissions 
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
