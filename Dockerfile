FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

ADD ./* /app/
WORKDIR /app/

# Install minimal python
RUN apt-get update && \
    apt-get install -y python3-pip && \
    # Install essential Python packages
    pip install -r requirements.txt --extra-index-url https://download.pytorch.org/whl/cpu && \
    # Configure renv to use RSPM to download packages by default
    echo 'options(renv.config.repos.override = getOption("repos"))' >> ${R_HOME}/etc/Rprofile.site && \
    # Install R packages
    Rscript -e "renv::restore()" && \
    # fix for version GLIBCXX_3.4.30
    rm /usr/lib/x86_64-linux-gnu/libstdc++.so.6 && \
    ln -s /opt/mamba/lib/libstdc++.so.6 /usr/lib/x86_64-linux-gnu/libstdc++.so.6
