struct S { int unused; };

#define X 1
#define Y -1

// if BOM is 1, we end up with two 32bit integers
// where the upper 4 byte ended up in the lower 4.
extern unsigned long long ___hsc2hs_BOM___;
unsigned long long ___hsc2hs_BOM___ = 0x100000000;

extern unsigned long long x___hsc2hs_sign___;
extern unsigned long long x;
unsigned long long x___hsc2hs_sign___ = ((struct S *)X) < 0;
unsigned long long x = (unsigned long long)((struct S *)X);

extern unsigned long long y___hsc2hs_sign___;
extern unsigned long long y;
unsigned long long y___hsc2hs_sign___ = ((struct S *)Y) < 0;
unsigned long long y = (unsigned long long)((struct S *)Y);

extern unsigned long long z___hsc2hs_sign___;
extern unsigned long long z;
unsigned long long z___hsc2hs_sign___ = Y < 0;
unsigned long long z = (unsigned long long)Y;

extern char * t;
char * t = "Hello World\" 12345";
