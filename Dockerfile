FROM eddelbuettel/r2u:22.04

RUN echo "Updating deps... $CACHEBUST"
RUN apt-get update
ADD dependencies.txt .
RUN apt-get install -y $(cat dependencies.txt)

ENV NODE_VERSION="18.x"
ENV CRONICLE_VERSION="v0.9.30"

# Install NodeJS 18
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION} | bash -
RUN apt-get install -y nodejs

RUN curl -s https://raw.githubusercontent.com/jhuckaby/Cronicle/${CRONICLE_VERSION}/bin/install.js | node
WORKDIR /opt/cronicle

# Cronicle
ENV CRONICLE_foreground=1
ENV CRONICLE_echo=1
ENV TZ=America/New_York
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"

RUN echo "Setting env vars.."
ARG S3_SECRET
ARG SENDINBLUE_USER
ARG SENDINBLUE_PASS
ENV CRONICLE_manager 1
ENV CRONICLE_secret_key=
ENV CRONICLE_Storage__engine=S3
ENV CRONICLE_Storage__S3__params__Bucket=cronicle
ENV CRONICLE_Storage__AWS__endpoint=http://s3-gate.mortality.watch
ENV CRONICLE_Storage__AWS__forcePathStyle=true
ENV CRONICLE_Storage__AWS__region=us-east-1
ENV CRONICLE_Storage__AWS__credentials__secretAccessKey=${S3_SECRET}
ENV CRONICLE_Storage__AWS__credentials__accessKeyId=minio
ENV CRONICLE_mail_options__auth__user=${SENDINBLUE_USER}
ENV CRONICLE_mail_options__auth__pass=${SENDINBLUE_PASS}
ENV CRONICLE_client__custom_live_log_socket_url="https://cron.mortality.watch"

ADD entrypoint.sh .
ADD config.json .

# Configure SSL version for R downloader
ADD openssl.cnf .
ENV OPENSSL_CONF=/opt/cronicle/openssl.cnf

# Install MinIO Client
RUN curl https://dl.min.io/client/mc/release/linux-amd64/mc --create-dirs -o minio-binaries/mc
RUN chmod +x minio-binaries/mc
RUN echo "export PATH=$PATH:$(pwd)/minio-binaries/" >>~/.bashrc
RUN minio-binaries/mc alias set minio http://s3-gate.mortality.watch minio $S3_SECRET

# R deps
ADD dependencies_r.txt .
ADD install_r_deps.sh .
RUN /opt/cronicle/install_r_deps.sh

EXPOSE 3012
ENTRYPOINT ["/bin/tini", "--"]
CMD ["sh", "/opt/cronicle/entrypoint.sh"]
