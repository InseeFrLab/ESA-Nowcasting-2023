FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

# Install packages specified in the renv.lock file
RUN git clone https://github.com/InseeFrLab/ESA-Nowcasting-2023.git && \
    cd ESA-Nowcasting-2023 && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
    
# Rstudio doit se lancer en root donc garder 
#USER root

# Install minimal python
RUN apt-get update && \
    apt-get install -y python3-pip && \
    # Install essential Python packages
    pip install -r requirement.txt && \
    # fix for version GLIBCXX_3.4.30
    rm /usr/lib/x86_64-linux-gnu/libstdc++.so.6 && \
    ln -s /opt/mamba/lib/libstdc++.so.6 /usr/lib/x86_64-linux-gnu/libstdc++.so.6
