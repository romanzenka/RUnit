TOP=../../..
TEST_DIR=$(shell pwd)
INST_PATH=$(shell dirname ${TEST_DIR})
PKG_PATH=$(shell dirname ${INST_PATH})

all: install test

install:
	cd ${TOP}; \
	R CMD INSTALL --clean ${PKG_PATH} && \
	cd ${TEST_DIR}

test:
	R --slave < runalltests.R
