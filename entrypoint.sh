#!/bin/sh

# Only run setup when setup needs to be done
if [ ! -f /opt/cronicle/.setup_done ]; then
  /opt/cronicle/bin/control.sh setup
  touch /opt/cronicle/.setup_done
fi

# Run cronicle
node /opt/cronicle/lib/main.js
