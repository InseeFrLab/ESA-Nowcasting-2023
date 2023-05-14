FROM inseefrlab/onyxia-base

USER root

WORKDIR ${HOME}/ESA-Nowcasting-2023
COPY . .

# Use RStudio's package manager to download packages as binaries
ENV CRAN="https://packagemanager.posit.co/cran/__linux__/jammy/latest"

ARG RSTUDIO_CONF_FILE="/etc/rstudio/rsession.conf"

# Install python
RUN apt-get update && \
    apt-get install -y \
            python3 \
            python3-pip \
            openjdk-8-jdk \
            r-base

# # Install R using rocker's install scripts
RUN git clone --branch R4.2.3 --depth 1 https://github.com/rocker-org/rocker-versioned2.git /tmp/rocker-versioned2 && \
    cp -r /tmp/rocker-versioned2/scripts/ /rocker_scripts/ && \
    chown -R ${USERNAME}:${GROUPNAME} /rocker_scripts/ && \
    chmod -R 700 /rocker_scripts/

# # Set up R (RSPM, OpenBLAS, littler, addtional packages)
# RUN /rocker_scripts/setup_R.sh && \
#     # Re-install system libs that may have been removed by autoremove in rocker scripts
#     /opt/install-system-libs.sh && \
#     # Install useful additional packages
#     install2.r --error \
#         renv \
#         devtools

# # Install python Dependencies
RUN pip install -r requirements.txt

# Reconfigure Java support 
RUN sudo R CMD javareconf

# Install libcurl4
RUN sudo apt install libcurl4-openssl-dev

# Install latest version of quarto
RUN QUARTO_DL_URL=$(wget -qO- https://quarto.org/docs/download/_download.json | grep -oP '(?<=\"download_url\":\s\")https.*linux-amd64.deb') && \
    wget -q ${QUARTO_DL_URL} -O quarto.deb && \
    dpkg -i quarto.deb && \
    quarto check install && \
    rm quarto.deb

# Install R Dependencies
RUN Rscript -e "renv::restore()"

SHELL ["/bin/bash", "-c"]

# Install Rstudio using rocker's install scripts
RUN /rocker_scripts/install_rstudio.sh && \
    /rocker_scripts/install_pandoc.sh && \
    # Set default working directory for R sessions and R projects
    echo "session-default-working-dir=${WORKSPACE_DIR}" >> ${RSTUDIO_CONF_FILE} && \
    echo "session-default-new-project-dir=${WORKSPACE_DIR}" >> ${RSTUDIO_CONF_FILE} && \
    # Fix permissions
    chown -R ${USERNAME}:${GROUPNAME} ${HOME} && \
    # Clean
    rm -rf /var/lib/apt/lists/*


EXPOSE 8787

CMD ["/init"]
