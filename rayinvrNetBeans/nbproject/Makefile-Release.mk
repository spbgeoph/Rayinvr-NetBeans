#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=MinGW_pure-Windows
CND_DLIB_EXT=dll
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/adjpt.o \
	${OBJECTDIR}/src/atrc.o \
	${OBJECTDIR}/src/blkdat.o \
	${OBJECTDIR}/src/calmod.o \
	${OBJECTDIR}/src/hdw.o \
	${OBJECTDIR}/src/inv.o \
	${OBJECTDIR}/src/main.o \
	${OBJECTDIR}/src/misc.o \
	${OBJECTDIR}/src/nopltlib.o \
	${OBJECTDIR}/src/plt.o \
	${OBJECTDIR}/src/pltlib.o \
	${OBJECTDIR}/src/pltsub.o \
	${OBJECTDIR}/src/rngkta.o \
	${OBJECTDIR}/src/trc.o \
	${OBJECTDIR}/src/trcaux.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/rayinvrnetbeans.exe

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/rayinvrnetbeans.exe: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/rayinvrnetbeans ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/src/adjpt.o: src/adjpt.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/trc.o -o ${OBJECTDIR}/src/adjpt.o src/adjpt.f95

${OBJECTDIR}/src/atrc.o: src/atrc.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/pltlib.o -o ${OBJECTDIR}/src/atrc.o src/atrc.f95

${OBJECTDIR}/src/blkdat.o: src/blkdat.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/blkdat.o src/blkdat.f95

${OBJECTDIR}/src/calmod.o: src/calmod.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/inv.o ${OBJECTDIR}/src/misc.o -o ${OBJECTDIR}/src/calmod.o src/calmod.f95

${OBJECTDIR}/src/hdw.o: src/hdw.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/inv.o ${OBJECTDIR}/src/misc.o -o ${OBJECTDIR}/src/hdw.o src/hdw.f95

${OBJECTDIR}/src/inv.o: src/inv.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/inv.o src/inv.f95

${OBJECTDIR}/src/main.o: src/main.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/calmod.o ${OBJECTDIR}/src/pltlib.o ${OBJECTDIR}/src/pltsub.o ${OBJECTDIR}/src/trc.o ${OBJECTDIR}/src/atrc. -o ${OBJECTDIR}/src/main.o src/main.f95

${OBJECTDIR}/src/misc.o: src/misc.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/misc.o src/misc.f95

${OBJECTDIR}/src/nopltlib.o: src/nopltlib.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/nopltlib.o src/nopltlib.f95

${OBJECTDIR}/src/plt.o: src/plt.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/pltlib.o ${OBJECTDIR}/src/pltsub.o -o ${OBJECTDIR}/src/plt.o src/plt.f95

${OBJECTDIR}/src/pltlib.o: src/pltlib.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/pltlib.o src/pltlib.f95

${OBJECTDIR}/src/pltsub.o: src/pltsub.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 ${OBJECTDIR}/src/pltlib.o -o ${OBJECTDIR}/src/pltsub.o src/pltsub.f95

${OBJECTDIR}/src/rngkta.o: src/rngkta.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/rngkta.o src/rngkta.f95

${OBJECTDIR}/src/trc.o: src/trc.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/trc.o src/trc.f95

${OBJECTDIR}/src/trcaux.o: src/trcaux.f95
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/src/trcaux.o src/trcaux.f95

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
