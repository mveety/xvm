/* Intel 386+ u.h for clang/gcc */

#define nil ((void*)0)

// default to amd64
#ifndef INTEL386 | AMD64
#define AMD64
#endif

#ifdef INTEL386 | AMD64
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef signed char schar;
typedef signed short sshort;
typedef signed int sint;
typedef signed long slong;
typedef signed char s8int;
typedef signed short s16int;
typedef signed int s32int;
typedef signed long s64int;
typedef unsigned char u8int;
typedef unsigned short u16int;
typedef unsigned int u32int;
typedef unsigned long u64int;
#endif

#ifdef INTEL386
typedef u32int uintptr;
typedef s32int intptr;
#endif
#ifdef AMD64
typedef u64int uintptr;
typedef s64int intptr;
#endif

