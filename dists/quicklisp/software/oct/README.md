Oct is a portable Lisp implementation of quad-double arithmetic. This
gives about 65 digits of precision. Quad-double arithmetic uses four
double-float numbers to represent an extended precision number.

The implementation is modeled on the quad-double package by Yozo
Hida. This package is in C++, but we have translated parts of it and
extended it to use Lisp. The intent is to provide all of the CL
arithmetic functions with a quad-double implementation.

Further information will be provided at a later date. This is
currently a work in progress, but the current code has the basic
functionality implemented and includes all of the special functions
specified by CL. There are, undoubtedly, many bugs.
