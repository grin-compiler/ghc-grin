all:
	clang -target arm-linux-gnueabihf -S -c tmp.c -o arm.s
	clang -target aarch64-linux-gnueabihf -S -c tmp.c -o aarch64.s
	clang -target arm64-apple-ios -S -c tmp.c -o aarch64-ios.s
	clang -target armv7-apple-ios -S -c tmp.c -o arm-ios.s
	x86_64-w64-mingw32-gcc -S -c tmp.c -o x86_64-mingw32.s
	clang -target i386-unknown-linux -S -c tmp.c -o x86-linux.s
	clang -target x86_64-apple-macos -S -c tmp.c -o x86_64-mac.s
	clang -target i386-unknown-linux -S -c tmp.c -o x86-linux.s
	clang -target x86_64-unknown-linux -S -c tmp.c -o x86_64-linux.s
	clang -target sparc-unknown-linux -S -c tmp.c -o sparc-linux.s
	clang -target sparc64-unknown-linux -S -c tmp.c -o sparc64-linux.s
	clang -target mips-unknown-linux -S -c tmp.c -o mips-linux.s
	clang -target mips64-unknown-linux -S -c tmp.c -o mips64-linux.s
	clang -target powerpc-unknown-linux -S -c tmp.c -o powerpc-linux.s
	clang -target powerpc64-unknown-linux -S -c tmp.c -o powerpc64-linux.s
	clang -target powerpc64le-unknown-linux -S -c tmp.c -o powerpc64le-linux.s
	hppa-unknown-linux-gnu-gcc -S -c tmp.c -o hppa-linux.s
	m68k-unknown-linux-gnu-gcc -S -c tmp.c -o m68k-linux.s
	alpha-unknown-linux-gnu-gcc -S -c tmp.c -o alpha-linux.s
	ia64-unknown-linux-gnu-gcc -S -c tmp.c -o ia64-linux.s
	nios2-unknown-linux-gnu-gcc -S -c tmp.c -o nios2-linux.s
	s390-unknown-linux-gnu-gcc -S -c tmp.c -o s390-linux.s
	s390x-unknown-linux-gnu-gcc -S -c tmp.c -o s390x-linux.s
	sh4-unknown-linux-gnu-gcc -S -c tmp.c -o sh4-linux.s
