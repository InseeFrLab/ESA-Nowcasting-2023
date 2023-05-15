FROM inseefrlab/onyxia-rstudio:r4.2.3

WORKDIR ${HOME}/ESA-Nowcasting-2023

ENV RENV_CONFIG_REPOS_OVERRIDE=${CRAN}

# System libs
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libglpk-dev \
        openjdk-8-jdk

# Install Python
RUN /rocker_scripts/install_python.sh

# Install Python dependencies 
COPY . .
RUN pip install -r requirements.txt --extra-index-url https://download.pytorch.org/whl/cpu

# Install R dependencies
RUN Rscript -e "renv::restore()"

# Fix permissions 
RUN chown -R ${USERNAME}:${GROUPNAME} ${HOME}
