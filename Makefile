-include config.mak

GOAL = firmforth

FIRM_CFLAGS ?= `pkg-config --cflags libfirm`
FIRM_LIBS   ?= `pkg-config --libs libfirm`

CFLAGS += -Wall -W -Werror -O0 -g3 -std=gnu99
CFLAGS += -I .
CFLAGS += $(FIRM_CFLAGS) $(ADDCFLAGS)

LFLAGS = $(FIRM_LIBS)

SOURCES := firmforth.c
OBJECTS = $(SOURCES:%.c=build/%.o)

Q = @

.PHONY : all clean dirs

all: $(GOAL) gitrev.h

gitrev.h: .git/*
	echo "#define GITREV \"$$(git describe --dirty --tags --always)\"" > $@

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

.depend: $(SOURCES)
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

$(GOAL): build/adt $(OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) -rdynamic $(OBJECTS) $(LFLAGS) -o $(GOAL)

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c
	@echo '===> CC $<'
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
