# $Revision: 1.2 $
# The following is a sample makefile 
# for use with the JDE->Build command.
#
# Makefile for jdebug project.
#

JDK_DOS = e:/jdk1.3
JPDA_DOS = $(JDK_DOS)/lib/tools.jar
JDE = //d/jde-dev/jde
JDE_DOS = d:/jde-dev/jde
SRC_DOS = $(JDE_DOS)/java/src
CLASS_DOS = $(JDE_DOS)/java/classes
CLASSPATH_DOS = .;$(SRC_DOS);$(JPDA_DOS);$(CLASS_DOS)
COMPILER = javac
VM = java
MAKE = make

VPATH = ../../../src/jde/debugger

COPTIONS = -g -deprecation -d $(CLASS_DOS)  -classpath "$(CLASSPATH_DOS)" -sourcepath $(SRC_DOS)

CLASSES = Debug.class \
	  Etc.class \
	  LispForm.class \
	  Rep.class \
	  Jdebug.class \
	  Protocol.class \
	  Main.class \
	  Application.class \
	  EventHandler.class \
	  ObjectStore.class \
	  ApplicationCommands.class \
	  GeneralCommands.class \
	  ThreadCommands.class \
#	  BreakpointCommands.class \
	  JDEException.class \
	  JDENumberFormatException.class \
	  spec/EventRequestSpec.class \
	  spec/EventRequestSpecList.class \
	  spec/BreakpointSpec.class \
	  spec/LineBreakpointSpec.class \
	  spec/MethodBreakpointSpec.class \
	  spec/WatchpointSpec.class \
	  spec/AccessWatchpointSpec.class \
	  spec/ModificationWatchpointSpec.class \
	  spec/ExceptionSpec.class \
	  spec/ReferenceTypeSpec.class \
	  spec/SourceNameReferenceTypeSpec.class \
	  spec/PatternReferenceTypeSpec.class \
	  expr/ASCII_UCodeESC_CharStream.class \
	  expr/ExpressionParser.class \
	  expr/ExpressionParserConstants.class \
	  expr/ExpressionParserTokenManager.class \
	  expr/LValue.class \
	  expr/ParseException.class \
	  expr/Token.class \
	  expr/TokenMgrError.class

#.SUFFIXES: .java .class
%.class: %.java
	$(COMPILER) $(COPTIONS) $?

jdebug : $(CLASSES)

all:
	$(MAKE) -f $(JDE)/java/src/jde/debugger/command/makefile \
		-C $(JDE)/java/src/jde/debugger/command all
	$(MAKE) jdebug

# Rebuild (if necessary) 
run: $(CLASSES)
	$(VM) -classpath "$(CLASSPATH)" jde.debugger.Main

# Remove all class files from the project directory.
clean:
	rm *.class

# javadoc...
javadoc:
	javadoc -private jde.debugger jde.debugger.spec jde/debugger/expr/ExpressionParser.java -windowtitle "jdebug Documentation"

# Implicit rule for making Java class files from Java
# source files.
#.java.class:
#	$(COMPILER) $(COPTIONS) $?
