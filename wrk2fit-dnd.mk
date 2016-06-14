VSN = 20160614

DL ?= _dl
WRK ?= _wrk
OUT ?= _out
VFS ?= _vfs

#
# Deps
#

TKDND_VSN = 2.8
TKDND_WINDOWS = tkdnd$(TKDND_VSN)-win32-ix86.tar.gz
TKDND_WINDOWS_DIR = tkdnd$(TKDND_VSN)-win32

TCLKIT_VSN = 8.6.1
TCLKIT_HOST = tclkit-$(TCLKIT_VSN)-host
TCLKIT_LINUX = tclkit-$(TCLKIT_VSN)-linux-amd64
TCLKIT_WINDOWS = tclkit-$(TCLKIT_VSN)-win32-i586-xcompile

SDX_VSN = 20110317
SDX = sdx.kit

#
# Phony targets
#

.PHONY: all clean distclean

all: $(VFS)/wrk2fit-win32-$(VSN).exe

clean:
	rm -rf $(OUT) $(VFS) $(WRK)

distclean: clean
	rm -rf $(DL)

#
# Download deps
#

$(DL)/$(TKDND_WINDOWS):
	mkdir -p $(shell dirname $@)
	curl -L -o $@-part "http://downloads.sourceforge.net/project/tkdnd/Windows%20Binaries/TkDND%20$(TKDND_VSN)/tkdnd$(TKDND_VSN)-win32-ix86.tar.gz"
	mv $@-part $@

$(DL)/$(TCLKIT_HOST): $(DL)/$(TCLKIT_LINUX)
	cp $< $@

$(DL)/$(TCLKIT_LINUX):
	mkdir -p $(shell dirname $@)
	curl -L -o $@-part "http://www.rkeene.org/devel/kitcreator/kitbuild/0.8.0/$(TCLKIT_LINUX)"
	mv $@-part $@
	chmod +x $@

$(DL)/$(TCLKIT_WINDOWS):
	mkdir -p $(shell dirname $@)
	curl -L -o $@-part "http://www.rkeene.org/devel/kitcreator/kitbuild/0.8.0/$(TCLKIT_WINDOWS)"
	mv $@-part $@
	chmod +x $@

$(DL)/$(SDX):
	mkdir -p $(shell dirname $@)
	curl -L -o $@-part "https://tclkit.googlecode.com/files/sdx-$(SDX_VSN).kit"
	mv $@-part $@

#
# Build admintool VFS
#

$(VFS)/wrk2fit-win32-$(VSN).vfs/.done: \
		wrk2fit-dnd
	mkdir -p $(shell dirname $@)
	for SCRIPT in $^; do \
		cp $$SCRIPT $(shell dirname $@); \
	done
	cd $(shell dirname $@) && mv wrk2fit-dnd main.tcl
	sed -i "3ilappend auto_path lib" $(shell dirname $@)/main.tcl
	touch $@

$(VFS)/wrk2fit-win32-$(VSN).vfs/.done-libs: \
		$(DL)/$(TKDND_WINDOWS)
	mkdir -p $(shell dirname $@)/lib/
	tar -zxvf $(DL)/$(TKDND_WINDOWS) -C $(shell dirname $@)/lib/
	find $(shell dirname $@)/lib -type d -exec chmod 755 {} \;
	find $(shell dirname $@)/lib -type f -exec chmod 644 {} \;
	touch $@

$(VFS)/wrk2fit-win32-$(VSN).exe: \
		$(DL)/$(SDX) \
		$(DL)/$(TCLKIT_HOST) \
		$(DL)/$(TCLKIT_WINDOWS) \
		$(VFS)/wrk2fit-win32-$(VSN).vfs/.done \
		$(VFS)/wrk2fit-win32-$(VSN).vfs/.done-libs
	$(DL)/$(TCLKIT_HOST) $(DL)/$(SDX) \
		wrap $(shell basename $@) \
		-runtime $(DL)/$(TCLKIT_WINDOWS) \
		-vfs $(VFS)/wrk2fit-win32-$(VSN).vfs
	mv $(shell basename $@) $@
	chmod +x $@
