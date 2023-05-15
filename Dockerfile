FROM rocker/rstudio:4.2.2

WORKDIR ${HOME}/ESA-Nowcasting-2023

# System libs
RUN apt-get update && \
    apt-get install -y \
            git \
            libcurl4-openssl-dev \
            openjdk-8-jdk \
            wget
            
# Fetch Onyxia's init script
RUN wget https://raw.githubusercontent.com/InseeFrLab/images-datascience/main/scripts/onyxia-init.sh -O /opt/onyxia-init.sh

# Fetch rocker's install scripts
RUN git clone --branch R4.2.2 --depth 1 https://github.com/rocker-org/rocker-versioned2.git /tmp/rocker-versioned2 && \
    cp -r /tmp/rocker-versioned2/scripts/ /rocker_scripts/ && \
    chmod -R +x /rocker_scripts/

# Install Python
RUN /rocker_scripts/install_python.sh

COPY . .

# Install python dependencies
RUN pip install -r requirements.txt

# Reconfigure Java support 
RUN sudo R CMD javareconf

# Install R dependencies
RUN install2.r --error renv && \
    Rscript -e "renv::restore()"
  
