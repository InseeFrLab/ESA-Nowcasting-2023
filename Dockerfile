FROM inseefrlab/onyxia-rstudio:latest

#COPY renv/ ${HOME}/renv

RUN install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
