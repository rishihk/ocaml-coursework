MPVERSION=3
MPBASENAME=mp$(MPVERSION)
STUDENTSRC=$(MPBASENAME).ml
SKELETON=$(MPBASENAME)-skeleton.ml

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir

GRADER_NAME=grader

LIBRARY_GRADER=lib/grader.cma
MODULE_STUDENT=student
MODULE_SOLUTION=solution
MODULE_RUBRIC=rubric
MODULE_COMMON=common

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME) 

CHECKER_OBJECT=def.cmo genfuns.cmo parse.cmo lex.cmo cps.cmo check_recursion.cmo
CHECKER_INTERFACES=def.cmi genfuns.cmi parse.cmi lex.cmi cps.cmi check_recursion.cmi

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC) $(SKELETON)
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(CHECKER_OBJECT) $(CHECKER_INTERFACES)

IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo


########################################################################
# if mpXcommon.ml exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).ml)" "$(MODULE_COMMON).ml"
DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC) $(SKELETON) $(MODULE_COMMON).ml 
#DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(MODULE_COMMON).cmo $(MODULE_COMMON).cmi
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo 
$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml 
	$(OCAMLC) -c $(MODULE_COMMON).ml
endif

DISTFILES_OTHER=README $(MPBASENAME).pdf
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? util.o \
		$(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(CHECKER_OBJECT) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(CHECKER_OBJECT) $(OBJECTS)

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(STUDENTSRC) 
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml


############################################################################
# Compiling files needed for special grading of forward and tail recursion #
############################################################################

parse.cmo: 
	$(OCAMLYACC) parse.mly
	$(OCAMLC) -c parse.mli
	$(OCAMLC) -c parse.ml

lex.ml:  parse.cmo
	$(OCAMLLEX) lex.mll

lex.cmo: parse.cmo
	$(OCAMLLEX) lex.mll
	$(OCAMLC) -c lex.ml

def.cmo: 
	$(OCAMLC) -c def.ml

genfuns.cmo:
	$(OCAMLC) -c genfuns.ml

cps.cmo:
	$(OCAMLC) -c cps.ml

check_recursion.cmo:
	$(OCAMLC) -c check_recursion.ml


########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).ml)" "$(MODULE_SOLUTION).ml"
$(MODULE_SOLUTION).cmo: $(MODULE_SOLUTION).ml 
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(CHECKER_OBJECT) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "#" > $(MODULE_RUBRIC).ml
	$(OCAMLC) -c -I lib $(MODULE_RUBRIC).ml
	$(RM) -f $(MODULE_RUBRIC).ml

clean:
	$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)

##########################################################################
#these targets are used by staff
##########################################################################

TESTNAME=$(MPBASENAME)

dist: $(DISTFILES)
	$(RM) -rf $(TESTNAME)
	$(MKDIR) $(TESTNAME)
	$(MKDIR) $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml lib/*.c $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) $(MODULE_STUDENT).ml $(MODULE_COMMON).cm?
