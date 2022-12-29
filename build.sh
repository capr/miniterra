#!/bin/sh
# Build script for Windows, Linux and OSX. Makes both static and dynamic libs.
cd "${0%build.sh}" || exit 1

build() {
	mkdir -p bin/$P
	${X}gcc -std=c11 -pedantic -Wall -O3 -c $C lx.c
	${X}gcc *.o -shared -o bin/$P/$D $L
	rm -f      bin/$P/$A
	${X}ar rcs bin/$P/$A *.o
	rm *.o
}

if [ "$OSTYPE" = "msys" ]; then
	P=windows L="-s -static-libgcc" D=lx.dll A=lx.a build
elif [ "${OSTYPE#darwin}" != "$OSTYPE" ]; then
	[ `uname` = Linux ] && export X=x86_64-apple-darwin11-  # cross-compile on Linux
	P=osx C="-arch x86_64" L="-arch x86_64 -install_name @rpath/liblx.dylib" \
		D=liblx.dylib A=liblx.a build
else
	P=linux C=-fPIC L="-s -static-libgcc" D=liblx.so A=liblx.a build
fi
