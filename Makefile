TMP_CONTAINER_NAME= siek-vachharajani-dls2008-tmp

all:
	docker build -t siek-vachharajani-dls2008 .
	docker create -ti --name $(TMP_CONTAINER_NAME) siek-vachharajani-dls2008 bash
	docker cp $(TMP_CONTAINER_NAME):/gtubi/gtlc gtlc
	docker rm $(TMP_CONTAINER_NAME)