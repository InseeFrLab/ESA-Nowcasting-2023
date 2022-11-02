FROM inseefrlab/onyxia-rstudio:latest

COPY ./* ${HOME}/

RUN cd ${HOME}/ && \
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
