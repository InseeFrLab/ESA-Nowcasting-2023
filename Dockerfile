FROM inseefrlab/onyxia-rstudio:ds-r4.2.3

# Install packages specified in the renv.lock file
RUN git clone https://github.com/InseeFrLab/ESA-Nowcasting-2023.git && \
    cd ESA-Nowcasting-2023 && \
    install2.r renv && \
    #Rscript -e "renv::restore()" && \
    chown -R ${USERNAME}:${GROUPNAME} ${HOME}
    
SHELL ["/bin/bash", "-c"]

ARG PYTHON_VERSION="3.10.4"

ENV MAMBA_DIR="/opt/mamba"
ENV PATH="${MAMBA_DIR}/bin:${PATH}"

# Rstudio doit se lancer en root donc garder 
#USER root

COPY conda-env.yml .

# Install minimal python
RUN wget -q https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-Linux-x86_64.sh -O mambaforge.sh && \
    # Install mambaforge latest version
    /bin/bash mambaforge.sh -b -p "${MAMBA_DIR}" && \
    # Set specified Python version in base Conda env
    mamba install python=="${PYTHON_VERSION}" && \
    # Install essential Python packages
    mamba env update -n base -f conda-env.yml && \
    pip install torch==1.13.0+cpu torchvision==0.14.0+cpu torchaudio==0.13.0 -f https://download.pytorch.org/whl/torch_stable.html
    # Activate custom Conda env by default in shell
    echo ". ${MAMBA_DIR}/etc/profile.d/conda.sh && conda activate" >> ${HOME}/.bashrc && \
    # Fix permissions
    chown -R ${USERNAME}:${GROUPNAME} ${HOME} ${MAMBA_DIR} && \
    # Clean
    rm mambaforge.sh conda-env.yml && \ 
    mamba clean --all -f -y

CMD ["python3"]
