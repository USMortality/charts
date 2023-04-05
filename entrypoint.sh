#!/bin/sh

# Only run setup when setup needs to be done
if [ ! -f /opt/cronicle/.setup_done ]; then
  /opt/cronicle/bin/control.sh setup
  mv /opt/cronicle/config.json /opt/cronicle/conf/config.json

  touch /opt/cronicle/.setup_done
fi

service mariadb start
mysql -e "set password = password('');"

# Run cronicle
node /opt/cronicle/lib/main.js
