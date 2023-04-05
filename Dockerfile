FROM eddelbuettel/r2u:22.04

RUN echo "Updating deps..."
RUN apt-get update
RUN apt-get install -y nodejs npm tini curl git libssl-dev bash jq mariadb-server

RUN curl -s https://raw.githubusercontent.com/jhuckaby/Cronicle/master/bin/install.js | node
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
ENV CRONICLE_Storage__AWS__endpoint=https://s3.mortality.watch
ENV CRONICLE_Storage__AWS__forcePathStyle=true
ENV CRONICLE_Storage__AWS__region=us-east-1
ENV CRONICLE_Storage__AWS__credentials__secretAccessKey=${S3_SECRET}
ENV CRONICLE_Storage__AWS__credentials__accessKeyId=minio
ENV CRONICLE_mail_options__auth__user=${SENDINBLUE_USER}
ENV CRONICLE_mail_options__auth__pass=${SENDINBLUE_PASS}

WORKDIR /opt/cronicle/
ADD entrypoint.sh .
ADD config.json .

# R deps
ADD dependencies.txt .
ADD install_r_deps.sh .
RUN /opt/cronicle/install_r_deps.sh

# Configure SSL version for R downloader
ADD openssl.cnf .
ENV OPENSSL_CONF=/opt/cronicle/openssl.cnf

# Start Maria DB
RUN service mariadb start
RUN mysql -e "set password = password('');"

# Install MinIO Client
RUN curl https://dl.min.io/client/mc/release/linux-amd64/mc --create-dirs -o $HOME/minio-binaries/mc
RUN chmod +x $HOME/minio-binaries/mc
RUN export PATH=$PATH:$HOME/minio-binaries/
RUN $HOME/minio-binaries/mc alias set minio https://s3.mortality.watch minio $S3_SECRET

EXPOSE 3012
ENTRYPOINT ["/bin/tini", "--"]
CMD ["sh", "/opt/cronicle/entrypoint.sh"]
