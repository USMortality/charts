set -x MINIO_HOST https://storage.xls2csv.com
set -x MINIO_ACCESS_KEY minio
set -x MINIO_SECRET_KEY 

dokku config:set minio \
MINIO_ROOT_USER=minio \
MINIO_ROOT_PASSWORD=

dokku proxy:ports-add cronicle http:80:3012
dokku plugin:install https://github.com/michaelshobbs/dokku-hostname.git dokku-hostname

```sh
#!/usr/bin/env bash

[ ! -d \"charts\" ] && git clone https://github.com/USMortality/charts.git
cd charts
git fetch
git reset origin/master --hard
Rscript test.r
```
