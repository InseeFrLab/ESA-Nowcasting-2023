FROM inseefrlab/onyxia-rstudio:latest

ADD ./* ${HOME}/

RUN ls $HOME/

RUN cd ${HOME}/ && \
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
