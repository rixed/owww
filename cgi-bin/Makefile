top_srcdir = ..

SOURCES = \
	index.ml

REQUIRES = owww

all: index.cgi

include $(top_srcdir)/make.common

index.cgi: index.opt
	ln -f $< $@

index.opt: index.cmx

