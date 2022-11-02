FROM inseefrlab/onyxia-rstudio:latest

# add credentials on build
ARG SSH_PRIVATE_KEY
RUN mkdir /root/.ssh/
RUN echo "${SSH_PRIVATE_KEY}" > /root/.ssh/id_rsa

# make sure your domain is accepted
RUN touch /root/.ssh/known_hosts
RUN ssh-keyscan github.com >> /root/.ssh/known_hosts

RUN git clone git@github.com:InseeFrLab/ESA-Nowcasting-2023.git

FROM inseefrlab/onyxia-rstudio:latest
# copy the repository form the previous image
COPY --from=inseefrlab/onyxia-rstudio:latest /ESA-Nowcasting-2023 /srv/ESA-Nowcasting-2023

RUN cd ESA-Nowcasting-2023 && \
    install2.r renv && \
    Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
