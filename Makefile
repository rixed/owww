top_srcdir = .
PKG_NAME = owww

SOURCES = \
	html.ml \
	view.ml \
	ctrl.ml \
	dispatch.ml \
	input.ml \

REQUIRES = batteries cgi

all: owww.cma owww.cmxa

include $(top_srcdir)/make.common

