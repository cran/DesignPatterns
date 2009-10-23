################################################################################
##
##  Makefile script as short-cut for package content management
##
##
##      Author: Jitao David Zhang <j.zhang@dkfz.de>
##                   - make install  calls R CMD INSTALL
##                   - make check    calls R CMD check (with RUnit)
##                   - make dist     calls R CMD build
##
################################################################################

R=R-devel
PKG=DesignPatterns
PKG_VERSION  := 0.1.0

PKG_ROOT_DIR := $(shell pwd)
PKG_DIST_ROOT_DIR := ../$(PKG).tmp
PKG_HIDDEN_FILES  := Makefile 

install: 
	@echo '====== Installing Package ======'
	@(cd ..; ${R} CMD INSTALL $(PKG))
	@echo '====== Installing finished ======'
	@echo ' '

check:	
	@echo '====== Checking Package ======'
	@(export R_DEVELOP_MODE=TRUE; cd ..; ${R} CMD check $(PKG))
	@echo '====== Checking finished ======'
	@echo ' '

dist:	
	@echo '====== Building Distribution ======'
	cp -rp $(PKG_ROOT_DIR) $(PKG_DIST_ROOT_DIR)
	@(cd ..; $(RM) -r $(PKG_HIDDEN_FILES); R CMD build $(PKG))
	$(RM) -r $(PKG_DIST_ROOT_DIR)
	@echo '====== Building finished ======'
	@echo ' '
	@echo '====== Checking Package ======'
	@(export R_DEVELOP_MODE=TRUE; cd ..; ${R} CMD check $(PKG)_$(PKG_VERSION).tar.gz)
	@echo '====== Checking finished  ======'
	@echo ' '