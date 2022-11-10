FROM inseefrlab/onyxia-rstudio:latest

COPY . ${HOME}/ESA-Nowcasting-2023

RUN cd ${HOME}/ESA-Nowcasting-2023
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
