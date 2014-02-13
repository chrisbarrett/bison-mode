CWD          = $(shell pwd)
SCRIPT       = $(CWD)/script
GIT_DIR      = $(CWD)/.git
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
CASK         = cask
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_INIT_EL = $(USER_EMACS_D)/init.el
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
OBJECTS      = $(SRCS:.el=.elc)
PACKAGE_SRCS = $(SRCS) bison-mode-pkg.el
PACKAGE_TAR  = bison-mode-$(VERSION).tar

.PHONY: all
all : env compile dist

# Configure tooling and environment.
.PHONY: env
env : packages

# Byte-compile elisp files.
.PHONY: compile
compile : $(OBJECTS)

# Install packages with Cask.
$(PKG_DIR) : Cask
	$(CASK)
	$(CASK) install
	touch $(PKG_DIR)

# Create a tar that can be installed by package.el
.PHONY: dist
dist : $(PACKAGE_TAR)
$(PACKAGE_TAR) : $(PACKAGE_SRCS)
	rm -rf bison-mode-$(VERSION)
	mkdir -p bison-mode-$(VERSION)
	cp -f $(PACKAGE_SRCS) bison-mode-$(VERSION)
	tar cf $(PACKAGE_TAR) bison-mode-$(VERSION)
	rm -rf bison-mode-$(VERSION)

# Install elisp packages with cask.
.PHONY: packages
packages : $(PKG_DIR)

# Install the package to the user's Emacs dir.
.PHONY: install
install : dist
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(CWD)/$(PACKAGE_TAR)")'

# Uninstall the package.
.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/bison-mode-*

.PHONY: reinstall
reinstall : clean uninstall install

# Restore to pristine state.
.PHONY: clean-all
clean-all : clean clean-pkgdir

# Clean generated files.
.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -f flycheck_bison*
	rm -rf bison-mode-*.tar bison-mode-pkg.el

# Remove packages installed by Cask.
.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKG_DIR)

# Generate files.

bison-mode-pkg.el : Cask
	$(CASK) package

%.elc : %.el $(PKG_DIR)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize -L .\
	-f batch-byte-compile $<
