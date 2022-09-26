FROM inseefrlab/onyxia-rstudio:latest

RUN git clone https://github.com/InseeFrLab/ESA-Nowcasting-2023.git && \
    cd ESA-Nowcasting-2023 && \
    install2.r renv && \
    Rscript -e "renv::restore()"
