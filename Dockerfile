FROM cronicle/cronicle:edge

RUN apk update
RUN apk add R R-dev build-base libxml2-dev

ADD _deps.r .
RUN Rscript _deps.r

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

EXPOSE 3012
CMD ["manager"]
