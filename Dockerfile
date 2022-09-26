FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

RUN git clone https://github.com/InseeFrLab/ESA-Nowcasting-2023.git && \
    cd ESA-Nowcasting-2023 && \
    install2.r renv && \
    Rscript -e "renv::restore()"
