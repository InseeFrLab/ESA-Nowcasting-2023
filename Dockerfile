FROM inseefrlab/onyxia-rstudio:latest

COPY ESA-Nowcasting-2023/ ${HOME}/ESA-Nowcasting-2023

RUN ls $HOME/

RUN cd ESA-Nowcasting-2023
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
