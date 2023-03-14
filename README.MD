# Mortality.Watch Charts
Collection of R scripts, that download, combine, and display several data sources.
It's main purpose is the generation of mortality data for mortality.watch.

# S3
Data and Charts are stored on MinIO/S3 at s3.mortality.watch.

# Usage
Sample usage:
```
./install_r_deps.sh
Rscript update_datasets.r
Rscript population/world.r
```

See https://www.mortality.watch/ for the frontend.

# Configuration

## MinIO/S3
```
dokku config:set minio \
  MINIO_SERVER_URL=https://s3.mortality.watch \
  MINIO_BROWSER_REDIRECT_URL=https://admin-s3.mortality.watch
```

Dokku config:
```
dokku config:set minio \
MINIO_ROOT_USER=minio \
MINIO_ROOT_PASSWORD=XXX
```

## Cronicle/Job Scheduler
Runs on a cronicle job scheduler, daily.

Cronicle config on dokku:
```
dokku config:set cronicle \
  AWS_ACCESS_KEY_ID=minio \
  AWS_SECRET_ACCESS_KEY=XXX \
  AWS_S3_ENDPOINT=mortality.watch \
  AWS_DEFAULT_REGION=s3

dokku config:set cronicle \
  TWITTER_API_KEY=XXX \
  TWITTER_API_SECRET=XXX \
  TWITTER_ACCESS_TOKEN=XXX \
  TWITTER_ACCESS_SECRET=XXX
```

```
dokku proxy:ports-add cronicle http:80:3012
dokku plugin:install https://github.com/michaelshobbs/dokku-hostname.git dokku-hostname

dokku config:set cronicle \
  AWS_ACCESS_KEY_ID=minio \
  AWS_SECRET_ACCESS_KEY=XXXXXXXX \
  AWS_S3_ENDPOINT=mortality.watch \
  AWS_DEFAULT_REGION=s3
```

Each cronicle job is setup like this:
```sh
#!/usr/bin/env bash

[ ! -d \"charts\" ] && git clone https://github.com/USMortality/charts.git
cd charts
git fetch
git reset origin/master --hard
Rscript test.r
```
