#!/bin/bash
echo "Type exit to quit the container."
echo "Type gtlc to use the tool."
echo "The /host directory in the container is $PWD on the host."
docker run -it --rm -v $PWD:/host \
    --env PATH=/bin:/usr/bin:/usr/sbin:/sbin:/gtubi \
    -w /host \
    siek-vachharajani-dls2008 \
    /bin/bash