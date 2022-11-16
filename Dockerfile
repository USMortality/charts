FROM eddelbuettel/r2u:22.04

RUN apt-get update
RUN apt-get install -y nodejs npm tini curl git

RUN curl -s https://raw.githubusercontent.com/jhuckaby/Cronicle/master/bin/install.js | node
WORKDIR /opt/cronicle

# Cronicle
ENV CRONICLE_foreground=1
ENV CRONICLE_echo=1
ENV TZ=America/New_York
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"

ARG S3_SECRET
ENV CRONICLE_manager 1
ENV CRONICLE_secret_key=
ENV CRONICLE_Storage__engine=S3
ENV CRONICLE_Storage__S3__params__Bucket=cronicle
ENV CRONICLE_Storage__AWS__endpoint=https://storage.xls2csv.com
ENV CRONICLE_Storage__AWS__forcePathStyle=true
ENV CRONICLE_Storage__AWS__region=us-east-1
ENV CRONICLE_Storage__AWS__credentials__secretAccessKey=${S3_SECRET}
ENV CRONICLE_Storage__AWS__credentials__accessKeyId=minio

WORKDIR /opt/cronicle/
ADD entrypoint.sh .

# R deps
ADD dependencies.txt .
ADD install_r_deps.sh .
RUN /opt/cronicle/install_r_deps.sh

EXPOSE 3012
ENTRYPOINT ["/bin/tini", "--"]
CMD ["sh", "/opt/cronicle/entrypoint.sh"]
