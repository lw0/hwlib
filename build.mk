ifndef BUILDDIR
  $(error "hwlib Makefile requires BUILDDIR set to absolute path of build artifact directory")
endif

ifndef SRCDIRS
  $(error "hwlib Makefile requires SRCDIRS set to list of absolute paths to scala source directories")
endif

SHELL = /bin/bash

###############################################################################
# Outer Targets
###############################################################################
all: help


.PHONY: help
help:
	@echo "make <target>.gen             - Generate hardware sources for <target>"
	@echo "make <target>.sim {RUN=...}   - Run Verilator simulation of <target>, RUN selects suite"
	@echo "make <target>.xsim            - Run XSim simulation for <target>"
	@echo "make <target>.info            - Dump Variables for <target>"
	@echo
	@echo "<target> is the fully qualified name of a SpinalHDL HwMain toplevel module"


.PHONY: %.gen %.sim %.xsim %.info
%.gen:
	@$(MAKE) -C . -e "TARGET=$*" $(BUILDDIR)/$*.gen
%.sim:
	@$(MAKE) -C . -e "TARGET=$*" $(BUILDDIR)/$*.sim
%.xsim:
	@$(MAKE) -C . -e "TARGET=$*" $(BUILDDIR)/$*.xsim
%.proj:
	@$(MAKE) -C . -e "TARGET=$*" $(BUILDDIR)/$*.proj
%.impl:
	@$(MAKE) -C . -e "TARGET=$*" implement
%.info:
	@$(MAKE) -C . -e "TARGET=$*" info


.PHONY: clean
clean:
ifdef BUILDDIR
	rm -rf $(BUILDDIR)
endif


.PHONY: clean-all
clean-all: clean
	rm -rf ./project
	rm -rf ./target


ifdef TARGET
###############################################################################
# Preparations
###############################################################################

GENDIR        = $(BUILDDIR)/$(TARGET).gen
SIMDIR        = $(BUILDDIR)/$(TARGET).sim
XSIMDIR       = $(BUILDDIR)/$(TARGET).xsim
PROJDIR       = $(BUILDDIR)/$(TARGET).proj
RUNFILE       = $(BUILDDIR)/$(TARGET).runval.mk


TOPLEVEL      = $(shell echo $(TARGET) | rev | cut -d. -f1 | rev)


SOURCES       = $(foreach SRCDIR,$(SRCDIRS), \
                  $(shell find $(SRCDIR) -name ".*" -prune -o -type f -name '*.scala' -print))



###############################################################################
# Parameter Caches
###############################################################################

.PHONY: RUNFILE
RUNFILE:
	@mkdir -p $(dir $(RUNFILE))
	@echo RUNLAST = $(RUN) > $(RUNFILE)
-include $(RUNFILE)
ifneq ($(RUN),$(RUNLAST))
$(SIMDIR): RUNFILE
$(XSIMDIR): RUNFILE
endif


###############################################################################
# Inner Targets
###############################################################################

# make <target>.info
.PHONY: info
info:
	@echo BUILDDIR  = $(BUILDDIR)
	@echo SRCDIRS   = $(SRCDIRS)
	@echo
	@echo GENDIR    = $(GENDIR)
	@echo SIMDIR    = $(SIMDIR)
	@echo XSIMDIR   = $(XSIMDIR)
	@echo RUNFILE   = $(RUNFILE)
	@echo
	@echo TOPLEVEL  = $(TOPLEVEL)
	@echo SOURCES   = $(SOURCES)
	@echo SWSOURCES = $(SWSOURCES)
	@echo
	@echo RUN       = $(RUN)
	@echo RUNLAST   = $(RUNLAST)


# make <target>.gen
$(GENDIR): $(SOURCES)
	@rm -rf $(GENDIR)
	@mkdir -p $(GENDIR)
	@echo "=== GEN $(TARGET) ==="
	@sbt "runMain $(TARGET) gen:$(GENDIR) top:$(TOPLEVEL)" || \
	(rm -rf $(GENDIR); exit 1)


# make <target>.sim
.PHONY: $(SIMDIR)
$(SIMDIR): $(SOURCES)
	@rm -rf $@
	@mkdir -p $@
	@echo "=== SIM $(TARGET) ==="
	@sbt -batch "runMain $(TARGET) sim:$(SIMDIR) suite:$(RUN)"


# make <target>.xsim
.PHONY: $(XSIMDIR)
$(XSIMDIR): $(SOURCES)
	@rm -rf $@
	@mkdir -p $@
	@echo "=== XSIM $(TARGET) ==="
	@sbt -batch "runMain $(TARGET) xsim:$(XSIMDIR) suite:$(RUN)"

# make <target>.proj
$(PROJDIR): $(GENDIR)
	@rm -rf $@
	@mkdir -p $@
	@echo "=== PROJ $(TARGET) ==="
	@cd $(BUILDDIR) ; \
	 vivado -mode batch -source $(GENDIR)/create_project.tcl -tclargs $(PROJDIR)

.PHONY: implement
implement: $(GENDIR) $(PROJDIR)
	@cd $(BUILDDIR) ; \
	 vivado -mode batch -source $(GENDIR)/build_project.tcl -tclargs $(PROJDIR)

endif

