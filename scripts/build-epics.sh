#!/bin/bash

set -e
set -x

SCRIPTHOME="$(cd "$(dirname "$0")" && pwd)"

#cd "$(dirname "$0")"
epics_dir=${EPICS_DIR:-./epics}
if [[ -e "$epics_dir" ]]; then
    echo "Target directory '$epics_dir' already exists.  Please remove it manually."
    exit 1
fi
mkdir "$epics_dir"
cd "$epics_dir"

make="make "
if [[ ! -z "$CPP" ]]; then
  echo "Using CPP=$CPP"
  make="$make CPP='$CPP'"
fi
if [[ ! -z "$CC" ]]; then
  echo "Using CC=$CC"
  make="$make CC='$CC'"
fi
if [[ ! -z "$CXX" ]]; then
  echo "Using CXX=$CXX"
  make="$make CXX='$CXX'"
  make="$make CCC='$CXX'"
fi

# Run an arbitrary command to patch a file in-place.  The command should read
# stdin and write stdout.  This is particularly useful to work around
# differences between GNU and OSX sed's -i flag.
function quick-patch {
  local file="$1"
  local orig="${1}.orig"
  shift
  mv "$file" "$orig"
  "$@" <"$orig" >"$file"
}

function build-epics-base {

  curl -OLf 'https://www.aps.anl.gov/epics/download/base/baseR3.14.12.4.tar.gz'
  tar xf baseR3.14.12.4.tar.gz
  mv base-3.14.12.4 base

  pushd base

  if [[ "$(uname)" == "Darwin" ]]; then
    export EPICS_HOST_ARCH="darwin-x86"
  else
    export EPICS_HOST_ARCH="$(./startup/EpicsHostArch.pl)"
  fi

  echo "EPICS_HOST_ARCH=$EPICS_HOST_ARCH"

  # Patch the definition of $(CPP) to pass -P (omit #line markers).  EPICS
  # likes to use $(CPP) to preprocess things that aren't C code and then pass
  # the output to files that don't understand #line.
  quick-patch configure/CONFIG.gnuCommon \
      sed -e "/^CPP =/s/-E$/-E -P/"

  # Don't build epicsExceptionTest.  On recent GCC it gives a compile error due
  # to (deliberately) allocating an array that's too large.
  quick-patch src/libCom/test/Makefile  sed -e '/epicsExceptionTest/s/^/#/'

  # Fix GCC/glibc compilation error due to integer expressions passed to
  # floating-point functions.  (Normally this is a bug, as in `isinf(1/0)`, but
  # the EPICS authors expected automatic conversion to float for the code in
  # question and it is safe.)  This was reported on the official mailing
  # list [1], and the fix here is very similar to the official one.
  #
  # [1]: https://epics.anl.gov/tech-talk/2016/msg00734.php
  patch -p2 -i "$SCRIPTHOME/epics-base-fixFloatExpressions.patch"

  if [[ -n "$EPICS_BASE_PATCH" ]]; then
    patch -p1 -i "$EPICS_BASE_PATCH"
  fi

  # You might be tempted to add "-j8" to this line. DO NOT. The EPICS devs
  # have had a notoriously hard time making parallel builds consistent.
  $make -j1

  popd

  export EPICS_BASE="$(pwd)/base"

}

function build-support-libs {

  # NOTE 2018-02-15: old link seems dead?
  # curl -OLf 'http://www.aps.anl.gov/bcda/synApps/tar/synApps_5_5.tar.gz'
  curl -OLf 'https://www3.aps.anl.gov/bcda/synApps/tar/synApps_5_5.tar.gz'
  tar xf synApps_5_5.tar.gz

  mv synApps_5_5/support .
  pushd support

  # configure...
  quick-patch configure/RELEASE \
      sed -e "s%^EPICS_BASE=.*\$%EPICS_BASE=$EPICS_BASE%" \
          -e "s%^SUPPORT=.*\$%SUPPORT=$(pwd)%"

  # patch...
  quick-patch asyn-4-13/asyn/asynDriver/asynDriver.h \
      sed -e 's/__VAR_ARGS__/__VA_ARGS__/'

  # blacklist some modules...
  quick-patch Makefile \
    awk '
      /MOTOR/ { next; }
      /CAMAC/ { next; }
      /DXP/ { next; }
      /XXX/ { next; }
      /AREA_DETECTOR/ { next; }
      { print $0; }
      '

  # Fix makeReleaseConsistent.pl for Perl 5.6
  patch -p2 -i "$SCRIPTHOME/epics-support-makeReleaseConsistent.patch"

  if [[ -n "$EPICS_SUPPORT_PATCH" ]]; then
    patch -p1 -i "$EPICS_SUPPORT_PATCH"
  fi

  $make -j1 release
  $make -j1

  popd

}

build-epics-base
build-support-libs

set +x

echo "********************************************************************"
if [[ -n "$EPICS_BASE_PATCH" ]]; then
  echo "Note: applied patch to 'base': $EPICS_BASE_PATCH"
fi
if [[ -n "$EPICS_SUPPORT_PATCH" ]]; then
  echo "Note: applied patch to 'support': $EPICS_SUPPORT_PATCH"
fi

TMP="$(mktemp)"
echo "EPICS_HOST_ARCH=$EPICS_HOST_ARCH" | tee "$TMP"
echo "EPICS_BASE=$EPICS_BASE" | tee -a "$TMP"
echo "SUPPORT=$(pwd)/support" | tee -a "$TMP"
mv "$TMP" '../vars.sh'
