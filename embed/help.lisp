;;; help topic infos
(set-help-topic-info 'array
		     "Arrays"
		     "This section concerns functions related to arrays, which are dynamic indexed sequences of values.")
(set-help-topic-info 'binary
		     "Binary Manipulation"
		     "This section lists functions for manipulating binary data in memory and on disk.")
(set-help-topic-info 'boxed
		     "Boxed Data Structures"
		     "Boxed values are used for dealing with foreign data structures in Lisp.")
(set-help-topic-info 'char
		     "Characters and UTF-8 Glyphs"
		     "This section concerns functions related to character representations.")
(set-help-topic-info 'concurrency
		     "Concurrency and Parallel Programming"
		     "There are several mechanisms for doing parallel and concurrent programming in Z3S5 Lisp. Synchronization primitives are also listed in this section. Generally, users are advised to remain vigilant about potential race conditions.")
(set-help-topic-info 'console
		     "Console Input & Output"
		     "These functions access the operating system console (terminal) mostly for string output.")
(set-help-topic-info 'conversion
		     "Data Type Conversion"
		     "This section lists various ways in which one data type can be converted to another.")
(set-help-topic-info 'data
		     "Special Data Structures"
		     "This section lists some more specialized data structures and helper functions for them.")
(set-help-topic-info 'dict
		     "Dictionaries"
		     "Dictionaries are thread-safe key-value repositories held in memory. They are internally based on hash tables and have fast access.")
(set-help-topic-info 'fileio
		     "File Input & Output"
		     "These functions allow direct access for reading and writing to files. This module requires the `fileio` build tag.")
(set-help-topic-info 'float
		     "Floating Point Arithmetics Package"
		     "The package `fl` provides floating point arithmetics functions. They require the given number not to exceed a value that can be held by a 64 bit float in the range 2.2E-308 to 1.7E+308.")
(set-help-topic-info 'help
		     "Help System"
		     "This section lists functions related to the built-in help system.")
(set-help-topic-info 'io
		     "Input & Output"
		     "This section concerns functions related to input and output.")
(set-help-topic-info 'ling
		     "Soundex, Metaphone, etc."
		     "The package `ling` provides various phonemic transcription functions like Soundex and Metaphone that are commonly used for fuzzy search and similarity comparisons between strings.")
(set-help-topic-info 'lisp
		     "Lisp - Traditional Lisp Functions"
		     "This section comprises a large number of list processing functions as well the standard control flow macros and functions you'd expect in a Lisp system.")
(set-help-topic-info 'numeric
		     "Numeric Functions"
		     "This section describes functions that provide standard arithmetics for non-floating point numbers such as integers. Notice that Z3S5 Lisp uses automatic bignum support but only for select standard operations like multiplication, addition, and subtraction.")
(set-help-topic-info 'semver
		     "Semver Semantic Versioning"
		     "The `semver` package provides functions to deal with the validation and parsing of semantic versioning strings.")
(set-help-topic-info 'seq
		     "Sequence Functions"
		     "Sequences are either strings, lists, or arrays. Sequences functions are generally abstractions for more specific functions of these data types, and therefore may be a bit slower than their native counterparts. It is still recommended to use them liberally, since they make programs more readable.")
(set-help-topic-info 'sound
		     "Sound Support"
		     "Only a few functions are provided for sound support.")
(set-help-topic-info 'str
		     "String Manipulation"
		     "These functions all manipulate strings in one way or another.")
(set-help-topic-info 'system
		     "System Functions"
		     "These functions concern the inner workings of the Lisp interpreter. Your warranty might be void if you abuse them!")
(set-help-topic-info 'time
		     "Time & Date"
		     "This section lists functions that are time and date-related. Most of them use `(now)` and turn it into more human-readable form.")
(set-help-topic-info 'ui
		     "User Interface"
		     "This section lists miscellaneous user interface commands such as color for terminals.")
(set-help-topic-info 'uncategorized
		     "Other / Not Categorized"
		     "This section lists functions and symbols that have no associated help topic.")
(set-help-topic-info 'db
		     "Databases"
		     "These functions allow for Sqlite3 database access. The module needs to be enabled with the \"db\" build tag. It also provides access to key-value databases with prefix 'kvdb and the automated remember-recall system, both of which are implemented in Z3S5 Lisp on top of the 'db module. To use the remember system, it needs to be initialized first by calling `(init-remember)`.")
(set-help-topic-info 'zimage
		     "Runtime System Images"
		     "The following functions provide functionality for saving, loading, and running of runtime system images to and from disk.")
(set-help-topic-info 'lib
		     "Library System"
		     "This miscellaneous mini-library system allows importing programs with a prefix by source-transforming them.")
(set-help-topic-info 'oop
		     "Object-oriented Programming"
		     "The OOP system uses arrays to store objects and also offers a more lightweight array-based structure system. It is not built for performance but may be useful to prevent writing object-oriented wrapper data structures again and again. This is also the reason why it was decided to embed the OOP system with a fixed API rather than providing it as an include file, allowing for interoperable object-oriented programming without having to worry about whether the extension is loaded. It's very simple and lightweight.")

;;; help for builtin functions (intrinsics)

(defhelp car
    (use "(car li) => any")
  (info "Get the first element of a list or pair #li, an error if there is not first element.")
  (type proc)
  (arity 1)
  (topic (lisp))
  (see (list list? pair?)))

(defhelp cdr
    (use "(cdr li) => any")
  (info "Get the rest of a list #li. If the list is proper, the cdr is a list. If it is a pair, then it may be an element. If the list is empty, nil is returned.")
  (type proc)
  (arity 1)
  (topic (lisp))
  (see (car list list? pair?)))

(defhelp cons
    (use "(cons a b) => pair")
  (info "Cons two values into a pair. If #b is a list, the result is a list. Otherwise the result is a pair.")
  (type proc)
  (arity 2)
  (topic (lisp))
  (see (cdr car list? pair?)))

(defhelp atom?
    (use "(atom? x) => bool")
  (info "Return true if #x is an atomic value, nil otherwise. Atomic values are numbers and symbols.")
  (type proc)
  (arity 1)
  (topic (lisp))
  (see (sym?)))

(defhelp bind
    (use "(bind sym value)")
  (info "Bind #value to the global symbol #sym. In contrast to setq both values need quoting.")
  (type proc)
  (arity 2)
  (topic (system))
  (see (setq)))

(defhelp eq?
    (use "(eq? x y) => bool")
  (info "Return true if #x and #y are equal, nil otherwise. In contrast to other LISPs, eq? checks for deep equality of arrays and dicts. However, lists are compared by checking whether they are the same cell in memory. Use #equal? to check for deep equality of lists and other objects.")
  (type proc)
  (arity 2)
  (topic (lisp))
  (see (equal?)))

(defhelp list
    (use "(list [args] ...) => li")
  (info "Create a list from all #args. The arguments must be quoted.")
  (type proc)
  (arity -1)
  (topic (lisp))
  (see (cons)))

(defhelp rplaca
    (use "(rplaca li a) => li")
  (info "Destructively mutate #li such that its car is #a, return the list afterwards.")
  (type proc)
  (arity 2)
  (topic (lisp))
  (see (rplacd)))

(defhelp replacd
    (use "(rplacd li1 li2) => li")
  (info "Destructively replace the cdr of #li1 with #li2 and return the result afterwards.")
  (type proc)
  (arity 2)
  (topic (lisp))
  (see (rplaca)))

(defhelp len
    (use "(len seq) => int")
  (info "Return the length of #seq. Works for lists, strings, arrays, and dicts.")
  (type proc)
  (arity 1)
  (topic (seq))
  (see (seq?)))

(defhelp str?
    (use "(str? s) => bool")
  (info "Return true if #s is a string, nil otherwise.")
  (type proc)
  (arity 1)
  (see (num? atom? sym? closure? intrinsic? macro?)))

(defhelp num?
    (use "(num? n) => bool")
  (info "Return true if #n is a number (exact or inexact), nil otherwise.")
  (type proc)
  (arity 1)
  (topic (lisp))
  (see (str? atom? sym? closure? intrinsic? macro?)))

(defhelp sym?
    (use "(sym? sym) => bool")
  (info "Return true if #sym is a symbol, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (lisp))
  (see (str? atom?)))

(defhelp closure?
    (use "(closure? x) => bool")
  (info "Return true if #x is a closure, nil otherwise. Use #function? for texting whether #x can be executed.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (functional? macro? intrinsic? functional-arity functional-has-rest?)))

(defhelp intrinsic?
    (use "(intrinsic? x) => bool")
  (info "Return true if #x is an intrinsic built-in function, nil otherwise. Notice that this function tests the value and not that a symbol has been bound to the intrinsic.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (functional? macro? closure?))
  (warn "What counts as an intrinsic or not may change from version to version. This is for internal use only."))

(defhelp macro?
    (use "(macro? x) => bool")
  (info "Return true if #x is a macro, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (functional? intrinsic? closure? functional-arity functional-has-rest?)))

(defhelp boxed?
    (use "(boxed? x) => bool")
  (info "Return true if #x is a boxed value, nil otherwise. Boxed values are special objects that are special in the system and sometimes cannot be garbage collected.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (type-of num? str? sym? list? array? macro? closure? intrinsic? eof?)))

(defhelp functional-arity
    (use "(functional-arity proc) => int")
  (info "Return the arity of a functional #proc.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (functional? functional-has-rest?)))

(defhelp functional-has-rest?
    (use "(functional-has-rest? proc) => bool")
  (info "Return true if the functional #proc has a &rest argument, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (system))
  (see (functional? functional-arity)))

(defhelp eql?
    (use "(eql? x y) => bool")
  (info "Returns true if #x is equal to #y, nil otherwise. This is currently the same as equal? but the behavior might change.")
  (type proc)
  (arity 2)
  (topic (lisp))
  (see (equal?))
  (warn "Deprecated."))

(defhelp <
    (use "(< x y) => bool")
  (info "Return true if #x is smaller than #y.")
  (type proc)
  (arity 2)
  (topic (numeric))
  (see (<= >= >)))

(defhelp %
    (use "(% x y) => num")
  (info "Compute the remainder of dividing number #x by #y.")
  (type proc)
  (arity 2)
  (topic (numeric))
  (see (mod /)))

(defhelp mod
    (use "(mod x y) => num")
  (info "Compute #x modulo #y.")
  (type proc)
  (arity 2)
  (topic (numeric))
  (see (% /)))

(defhelp +
    (use "(+ [args] ...) => num")
  (info "Sum up all #args. Special cases: (+) is 0 and (+ x) is x.")
  (type proc)
  (arity -1)
  (topic (numeric))
  (see (- * /)))

(defhelp *
    (use "(* [args] ...) => num")
  (info "Multiply all #args. Special cases: (*) is 1 and (* x) is x.")
  (type proc)
  (arity -1)
  (topic (numeric))
  (see (+ - /)))

(defhelp -
    (use "(- x [y1] [y2] ...) => num")
  (info "Subtract #y1, #y2, ..., from #x. Special case: (- x) is -x.")
  (type proc)
  (arity -2)
  (topic (numeric))
  (see (+ * /)))

(defhelp /
    (use "(/ x y1 [y2] ...) => float")
  (info "Divide #x by #y1, then by #y2, and so forth. The result is a float.")
  (type proc)
  (arity -2)
  (topic (numeric))
  (see (+ * -)))

(defhelp truncate
    (use "(truncate x [y]) => int")
  (info "Round down to nearest integer of #x. If #y is present, divide #x by #y and round down to the nearest integer.")
  (type proc)
  (arity -2)
  (topic (numeric))
  (see (div / int)))

(defhelp div
    (use "(div n k) => int")
  (info "Integer division of #n by #k.")
  (type proc)
  (arity 2)
  (topic (numeric))
  (see (truncate / int)))

(defhelp int
    (use "(int n) => int")
  (info "Return #n as an integer, rounding down to the nearest integer if necessary.")
  (type proc)
  (arity 1)
  (topic (numeric))
  (see (float))
  (warn "If the number is very large this may result in returning the maximum supported integer number rather than the number as integer."))

(defhelp float
    (use "(float n) => float")
  (info "Convert #n to a floating point value.")
  (type proc)
  (topic (numeric float))
  (arity 1)
  (see (int)))

(defhelp fl.abs
    (use "(fl.abs x) => fl")
  (info "Return the absolute value of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (float *)))

(defhelp fl.acos
    (use "(fl.acos x) => fl")
  (info "Return the arc cosine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.cos)))

(defhelp fl.asin
    (use "(fl.asin x) => fl")
  (info "Return the arc sine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.acos)))

(defhelp fl.asinh
    (use "(fl.asinh x) => fl")
  (info "Return the inverse hyperbolic sine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.cosh)))

(defhelp fl.atan
    (use "(fl.atan x) => fl")
  (info "Return the arctangent of #x in radians.")
  (type proc)
  (topic (float))
  (arity 1)
  (see (fl.atanh fl.tan)))

(defhelp fl.atan2
    (use "(fl.atan2 x y) => fl")
  (info "Atan2 returns the arc tangent of #y / #x, using the signs of the two to determine the quadrant of the return value.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.atan)))

(defhelp fl.atanh
    (use "(fl.atanh x) => fl")
  (info "Return the inverse hyperbolic tangent of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.atan)))

(defhelp fl.cbrt
    (use "(fl.cbrt x) => fl")
  (info "Return the cube root of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.sqrt)))

(defhelp fl.ceil
    (use "(fl.ceil x) => fl")
  (info "Round #x up to the nearest integer, return it as a floating point number.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.floor truncate int fl.round fl.trunc)))

(defhelp fl.cos
    (use "(fl.cos x) => fl")
  (info "Return the cosine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.sin)))

(defhelp fl.cosh
    (use "(fl.cosh x) => fl")
  (info "Return the hyperbolic cosine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.cos)))

(defhelp fl.dim
    (use "(fl.dim x y) => fl")
  (info "Return the maximum of x, y or 0.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (max)))

(defhelp fl.erf
    (use "(fl.erf x) => fl")
  (info "Return the result of the error function of #x.")
  (type proc)
  (arity 1)
  (topic (float numeric))
  (see (fl.erfc fl.dim)))

(defhelp fl.erfc
    (use "(fl.erfc x) => fl")
  (info "Return the result of the complementary error function of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.erfcinv fl.erf)))

(defhelp fl.erfcinv
    (use "(fl.erfcinv x) => fl")
  (info "Return the inverse of (fl.erfc #x).")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.erfc)))

(defhelp fl.erfinv
    (use "(fl.erfinv x) => fl")
  (info "Return the inverse of (fl.erf #x).")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.erf)))

(defhelp fl.exp
    (use "(fl.exp x) => fl")
  (info "Return e^#x, the base-e exponential of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.exp)))

(defhelp fl.exp2
    (use "(fl.exp2 x) => fl")
  (info "Return 2^#x, the base-2 exponential of #x.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.exp)))

(defhelp fl.expm1
    (use "(fl.expm1 x) => fl")
  (info "Return e^#x-1, the base-e exponential of (sub1 #x). This is more accurate than (sub1 (fl.exp #x)) when #x is very small.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.exp)))

(defhelp fl.fma
    (use "(fl.fma x y z) => fl")
  (info "Return the fused multiply-add of #x, #y, #z, which is #x * #y + #z.")
  (type proc)
  (topic (float numeric))
  (arity 3)
  (see (* +)))

(defhelp fl.floor
    (use "(fl.floor x) => fl")
  (info "Return #x rounded to the nearest integer below as floating point number.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.ceil truncate int)))

(defhelp fl.frexp
    (use "(fl.frexp x) => li")
  (info "Break #x into a normalized fraction and an integral power of two. It returns a list of (frac exp) containing a float and an integer satisfying #x == #frac × 2^#exp where the absolute value of #frac is in the interval [0.5, 1).")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.exp)))

(defhelp fl.gamma
    (use "(fl.gamma x) => fl")
  (info "Compute the Gamma function of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.lgamma)))

(defhelp fl.hypot
    (use "(fl.hypot x y) => fl")
  (info "Compute the square root of x^2 and y^2.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.sqrt)))

(defhelp fl.ilogb
    (use "(fl.ilogb x) => fl")
  (info "Return the binary exponent of #x as a floating point number.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.exp2)))

(defhelp fl.inf
    (use "(fl.inf x) => fl")
  (info "Return positive 64 bit floating point infinity +INF if #x >= 0 and negative 64 bit floating point finfinity -INF if #x < 0.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.is-nan?)))

(defhelp fl.is-nan?
    (use "(fl.is-nan? x) => bool")
  (info "Return true if #x is not a number according to IEEE 754 floating point arithmetics, nil otherwise.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.inf)))

(defhelp fl.j0
    (use "(fl.j0 x) => fl")
  (info "Apply the order-zero Bessel function of the first kind to #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.j1 fl.jn fl.y0 fl.y1 fl.yn)))

(defhelp fl.j1
    (use "(fl.j1 x) => fl")
  (info "Apply the the order-one Bessel function of the first kind #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.j0 fl.jn fl.y0 fl.y1 fl.yn)))

(defhelp fl.jn
    (use "(fl.jn n x) => fl")
  (info "Apply the Bessel function of order #n to #x. The number #n must be an integer.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.j1 fl.j0 fl.y0 fl.y1 fl.yn)))

(defhelp fl.ldexp
    (use "(fl.ldexp x n) => fl")
  (info "Return the inverse of fl.frexp, #x * 2^#n.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.frexp)))

(defhelp fl.lgamma
    (use "(fl.lgamma x) => li")
  (info "Return a list containing the natural logarithm and sign (-1 or +1) of the Gamma function applied to #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.gamma)))

(defhelp fl.log
    (use "(fl.log x) => fl")
  (info "Return the natural logarithm of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.log10 fl.log2 fl.logb fl.log1p)))

(defhelp fl.log10
    (use "(fl.log10 x) => fl")
  (info "Return the decimal logarithm of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.log fl.log2 fl.logb fl.log1p)))

(defhelp fl.log1p
    (use "(fl.log1p x) => fl")
  (info "Return the natural logarithm of #x + 1. This function is more accurate than (fl.log (add1 x)) if #x is close to 0.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.log fl.log2 fl.logb fl.log10)))

(defhelp fl.log2
    (use "(fl.log2 x) => fl")
  (info "Return the binary logarithm of #x. This is important for calculating entropy, for example.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.log fl.log10 fl.log1p fl.logb)))

(defhelp fl.logb
    (use "(fl.logb x) => fl")
  (info "Return the binary exponent of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.log fl.log10 fl.log1p fl.logb fl.log2)))

(defhelp fl.max
    (use "(fl.max x y) => fl")
  (info "Return the larger value of two floating point arguments #x and #y.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.min max min)))

(defhelp fl.min
    (use "(fl.min x y) => fl")
  (info "Return the smaller value of two floating point arguments #x and #y.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.min max min)))

(defhelp fl.mod
    (use "(fl.mod x y) => fl")
  (info "Return the floating point remainder of #x / #y.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.remainder)))

(defhelp fl.modf
    (use "(fl.modf x) => li")
  (info "Return  integer and fractional floating-point numbers that sum to #x. Both values have the same sign as #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.mod)))

(defhelp fl.nan
    (use "(fl.nan) => fl")
  (info "Return the IEEE 754 not-a-number value.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.is-nan? fl.inf)))

(defhelp fl.next-after
    (use "(fl.next-after x) => fl")
  (info "Return the next representable floating point number after #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.is-nan? fl.nan fl.inf)))

(defhelp fl.pow
    (use "(fl.pow x y) => fl")
  (info "Return #x to the power of #y according to 64 bit floating point arithmetics.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.pow10)))

(defhelp fl.pow10
    (use "(fl.pow10 n) => fl")
  (info "Return 10 to the power of integer #n as a 64 bit floating point number.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.pow)))

(defhelp fl.remainder
    (use "(fl.remainder x y) => fl")
  (info "Return the IEEE 754 floating-point remainder of #x / #y.")
  (type proc)
  (topic (float numeric))
  (arity 2)
  (see (fl.mod)))

(defhelp fl.round
    (use "(fl.round x) => fl")
  (info "Round #x to the nearest integer floating point number according to floating point arithmetics.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.round-to-even fl.truncate int float)))

(defhelp fl.round-to-even
    (use "(fl.round-to-even x) => fl")
  (info "Round #x to the nearest even integer floating point number according to floating point arithmetics.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.round fl.truncate int float)))

(defhelp fl.signbit
    (use "(fl.signbit x) => bool")
  (info "Return true if #x is negative, nil otherwise.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.abs)))

(defhelp fl.sin
    (use "(fl.sin x) => fl")
  (info "Return the sine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.cos)))

(defhelp fl.sinh
    (use "(fl.sinh x) => fl")
  (info "Return the hyperbolic sine of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.sin)))

(defhelp fl.sqrt
    (use "(fl.sqrt x) => fl")
  (info "Return the square root of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.pow)))

(defhelp fl.tan
    (use "(fl.tan x) => fl")
  (info "Return the tangent of #x in radian.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.tanh fl.sin fl.cos)))

(defhelp fl.tanh
    (use "(fl.tanh x) => fl")
  (info "Return the hyperbolic tangent of #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.tan flsinh fl.cosh)))

(defhelp fl.trunc
    (use "(fl.trunc x) => fl")
  (info "Return the integer value of #x as floating point number.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (truncate int fl.floor)))

(defhelp fl.y0
    (use "(fl.y0 x) => fl")
  (info "Return the order-zero Bessel function of the second kind applied to #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.y1 fl.yn fl.j0 fl.j1 fl.jn)))

(defhelp fl.y1
    (use "(fl.y1 x) => fl")
  (info "Return the order-one Bessel function of the second kind applied to #x.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.y0 fl.yn fl.j0 fl.j1 fl.jn)))

(defhelp fl.yn
    (use "(fl.yn n x) => fl")
  (info "Return the Bessel function of the second kind of order #n applied to #x. Argument #n must be an integer value.")
  (type proc)
  (topic (float numeric))
  (arity 1)
  (see (fl.y0 fl.y1 fl.j0 fl.j1 fl.jn)))

(defhelp prin1
    (use "(prin1 s)")
  (info "Print #s to the host OS terminal, where strings are quoted.")
  (type proc)
  (topic (console))
  (arity 1)
  (see (princ terpri out outy)))

(defhelp princ
    (use "(princ s)")
  (info "Print #s to the host OS terminal without quoting strings.")
  (type proc)
  (topic (console))
  (arity 1)
  (see (prin1 terpri out outy)))

(defhelp terpri
    (use "(terpri)")
  (info "Advance the host OS terminal to the next line.")
  (type proc)
  (topic (console))
  (arity 0)
  (see (princ out outy)))

(defhelp gensym
    (use "(gensym) => sym")
  (info "Return a new symbol guaranteed to be unique during runtime.")
  (type proc)
  (topic (system lisp))
  (arity 0)
  (see (nonce)))

(defhelp make-symbol
    (use "(make-symbol s) => sym")
  (info "Create a new symbol based on string #s.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (str->sym)))

(defhelp intern
    (use "(intern s) => sym")
  (info "Create a new interned symbol based on string #s.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (gensym str->sym make-symbol)))

(defhelp apply
    (use "(apply proc arg) => any")
  (info "Apply function #proc to argument list #arg.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (functional?)))

(defhelp exit
    (use "(exit [n])")
  (info "Immediately shut down the system and return OS host error code #n. The shutdown is performed gracefully and exit hooks are executed.")
  (type proc)
  (topic (system))
  (arity -1)
  (see ()))

(defhelp dump-bindings
    (use "(dump-bindings) => li")
  (info "Return a list of all top-level symbols with bound values, including those intended for internal use.")
  (type proc)
  (topic (system))
  (arity 0)
  (see (dump)))

(defhelp intrinsic
    (use "(intrinsic sym) => any")
  (info "Attempt to obtain the value that is intrinsically bound to #sym. Use this function to express the intention to use the pre-defined builtin value of a symbol in the base language.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (bind unbind))
  (warn "This function currently only returns the binding but this behavior might change in future."))

(defhelp bitxor
    (use "(bitxor n m) => int")
  (info "Return the bitwise exclusive or value of integers #n and #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitand bitor bitclear bitshl bitshr)))

(defhelp bitand
    (use "(bitand n m) => int")
  (info "Return the bitwise and of integers #n and #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitxor bitor bitclear bitshl bitshr)))

(defhelp bitor
    (use "(bitor n m) => int")
  (info "Return the bitwise or of integers #n and #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitxor bitand bitclear bitshl bitshr)))

(defhelp bitclear
    (use "(bitclear n m) => int")
  (info "Return the bitwise and-not of integers #n and #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitxor bitand bitor bitshl bitshr)))

(defhelp bitshl
    (use "(bitshl n m) => int")
  (info "Return the bitwise left shift of #n by #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitxor bitor bitand bitclear bitshr)))

(defhelp bitshr
    (use "(bitshr n m) => int")
  (info "Return the bitwise right shift of #n by #m.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (bitxor bitor bitand bitclear bitshl)))

(defhelp future
    (use "(future ...) => future")
  (info "Turn the body of this form into a promise for a future value. The body is executed in parallel and the final value can be retrieved by using (force f) on the future returned by this macro.")
  (type special)
  (topic (concurrency))
  (arity -1)
  (see (force task)))

(defhelp force
    (use "(force fut) => any")
  (info "Obtain the value of the computation encapsulated by future #fut, halting the current task until it has been obtained. If the future never ends computation, e.g. in an infinite loop, the program may halt indefinitely.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (future task make-mutex)))

(defhelp make-mutex
    (use "(make-mutex) => mutex")
  (info "Create a new mutex.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (mutex-lock mutex-unlock mutex-rlock mutex-runlock)))

(defhelp mutex-lock
    (use "(mutex-lock m)")
  (info "Lock the mutex #m for writing. This may halt the current task until the mutex has been unlocked by another task.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (mutex-unlock make-mutex mutex-rlock mutex-runlock)))

(defhelp mutex-unlock
    (use "(mutex-unlock m)")
  (info "Unlock the mutex #m for writing. This releases ownership of the mutex and allows other tasks to lock it for writing.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (mutex-lock make-mutex mutex-rlock mutex-runlock)))

(defhelp mutex-rlock
    (use "(mutex-rlock m)")
  (info "Lock the mutex #m for reading. This will allow other tasks to read from it, too, but may block if another task is currently locking it for writing.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (mutex-runlock mutex-lock mutex-unlock make-mutex)))

(defhelp mutex-runlock
    (use "(mutex-runlock m)")
  (info "Unlock the mutex #m from reading.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (mutex-lock mutex-unlock mutex-rlock make-mutex)))

(defhelp cinc!
    (use "(cinc! sym) => int")
  (info "Increase the integer value stored in top-level symbol #sym by 1 and return the new value. This operation is synchronized between tasks and futures.")
  (type macro)
  (topic (concurrency))
  (arity 1)
  (see (cdec! cwait ccmp cst!)))

(defhelp cdec!
    (use "(cdec! sym) => int")
  (info "Decrease the integer value stored in top-level symbol #sym by 1 and return the new value. This operation is synchronized between tasks and futures.")
  (type macro)
  (topic (concurrency))
  (arity 1)
  (see (cinc! cwait ccmp cst!)))

(defhelp cwait
    (use "(cwait sym value timeout)")
  (info "Wait until integer counter #sym has #value or #timeout milliseconds have passed. If #imeout is 0, then this routine might wait indefinitely. This operation is synchronized between tasks and futures.")
  (type proc)
  (topic (concurrency))
  (arity 3)
  (see (cinc! cdec! ccmp cst!)))

(defhelp ccmp
    (use "(ccmp sym value) => int")
  (info "Compare the integer value of #sym with the integer #value, return 0 if #sym = #value, -1 if #sym < #value, and 1 if #sym > #value. This operation is synchronized between tasks and futures.")
  (type macro)
  (topic (concurrency))
  (arity 2)
  (see (cinc! cdec! cwait cst!)))

(defhelp cst!
    (use "(cst! sym value)")
  (info "Set the value of #sym to integer #value. This operation is synchronized between tasks and futures.")
  (type proc)
  (topic (concurrency))
  (arity 2)
  (see (cinc! cdec! ccmp cwait)))

(defhelp collect-garbage
    (use "(collect-garbage [sort])")
  (info "Force a garbage-collection of the system's memory. If #sort is 'normal, then only a normal incremental garbage colllection is performed. If #sort is 'total, then the garbage collection is more thorough and the system attempts to return unused memory to the host OS. Default is 'normal.")
  (type proc)
  (topic (system))
  (arity -1)
  (see (memstats))
  (warn "There should rarely be a use for this. Try to use less memory-consuming data structures instead."))

(defhelp memstats
    (use "(memstats) => dict")
  (info "Return a dict with detailed memory statistics for the system.")
  (type proc)
  (topic (system))
  (arity 0)
  (see (collect-garbage)))

(defhelp out
    (use "(out expr)")
  (info "Output #expr on the console with current default background and foreground color.")
  (type proc)
  (topic (ui console))
  (arity 1)
  (see (outy synout synouty output-at)))

(defhelp color
    (use "(color sel) => (r g b a)")
  (info "Return the color based on #sel, which may be 'text for the text color, 'back for the background color, 'textarea for the color of the text area, 'gfx for the current graphics foreground color, and 'frame for the frame color. In standard Z3S5 Lisp only 'text and 'back are available as selectors and implementations are free to ignore these.")
  (type proc)
  (topic (ui console))
  (arity 1)
  (see (set-color reset-color the-color with-colors)))

(defhelp set-color
    (use "(set-color sel colorlist)")
  (info "Set the color according to #sel to the color #colorlist of the form '(r g b a). See #color for information about #sel.")
  (type proc)
  (topic (ui console))
  (arity 1)
  (see (color reset-color the-color with-colors)))

(defhelp reset-color
    (use "(reset-color)")
  (info "Reset the 'text and 'back colors of the display to default values. These values are not specified in the color database and depend on the runtime implementation. Other colors like 'gfx or 'frame are not affected.")
  (type proc)
  (topic (ui console))
  (arity 0)
  (see (set-color color the-color with-colors)))

(defhelp rnd
    (use "(rnd prng) => num")
  (info "Return a random value in the interval [0, 1] from pseudo-random number generator #prng. The #prng argument must be an integer from 0 to 9 (inclusive).")
  (type proc)
  (topic (numeric))
  (arity 0)
  (see (rand rndseed)))

(defhelp rand
    (use "(rand prng lower upper) => int")
  (info "Return a random integer in the interval [#lower #upper], both inclusive, from pseudo-random number generator #prng. The #prng argument must be an integer from 0 to 9 (inclusive).")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (rnd rndseed)))

(defhelp rndseed
    (use "(rndseed prng n)")
  (info "Seed the pseudo-random number generator #prng (0 to 9) with 64 bit integer value #n. Larger values will be truncated. Seeding affects both the rnd and the rand function for the given #prng.")
  (type proc)
  (topic (numeric))
  (arity 1)
  (see (rnd rand)))

(defhelp char->str
    (use "(char->str n) => str")
  (info "Return a string containing the unicode char based on integer #n.")
  (type proc)
  (topic (conversion))
  (arity 1)
  (see (str->char)))

(defhelp str->char
    (use "(str->char s) => int")
  (info "Return the integer that represents the unicode value of the first unicode rune in #s.")
  (type proc)
  (topic (conversion))
  (arity 1)
  (see (char->str)))

(defhelp error
    (use "(error [msgstr] [expr] ...)")
  (info "Raise an error, where #msgstr and the optional expressions #expr... work as in a call to fmt.")
  (type proc)
  (topic (system))
  (arity -1)
  (see (fmt with-final)))

(defhelp now
    (use "(now) => li")
  (info "Return the current datetime in UTC format as a list of values in the form '((year month day weekday iso-week) (hour minute second nanosecond unix-nano-second)).")
  (type proc)
  (topic (time))
  (arity 0)
  (see (now-ns datestr time date->epoch-ns epoch-ns->datelist)))

(defhelp now-ns
    (use "(now-ns) => int")
  (info "Return the current time in Unix nanoseconds.")
  (type proc)
  (topic (time))
  (arity 0)
  (see (now time)))

(defhelp time
    (use "(time proc) => int")
  (info "Return the time in nanoseconds that it takes to execute the procedure with no arguments #proc.")
  (type proc)
  (topic (time system))
  (arity 1)
  (see (now-ns now)))

(defhelp date->epoch-ns
    (use "(date->epoch-ns Y M D h m s ns) => int")
  (info "Return the Unix epoch nanoseconds based on the given year #Y, month #M, day #D, hour #h, minute #m, seconds #s, and nanosecond fraction of a second #ns, as it is e.g. returned in a (now) datelist.")
  (type proc)
  (topic (time))
  (arity 7)
  (see (epoch-ns->datelist datestr->datelist datestr datestr* day-of-week week-of-date now)))

(defhelp epoch-ns->datelist
    (use "(epoch-ns->datelist ns) => li")
  (info "Return the date list in UTC time corresponding to the Unix epoch nanoseconds #ns.")
  (type proc)
  (topic (time))
  (arity 1)
  (see (date->epoch-ns datestr->datelist datestr datestr* day-of-week week-of-date now)))

(defhelp day-of-week
    (use "(day-of-week Y M D) => int")
  (info "Return the day of week based on the date with year #Y, month #M, and day #D. The first day number 0 is Sunday, the last day is Saturday with number 6.")
  (type proc)
  (topic (time))
  (arity 3)
  (see (week-of-date datestr->datelist date->epoch-ns epoch-ns->datelist datestr datestr* now)))

(defhelp week-of-date
    (use "(week-of-date Y M D) => int")
  (info "Return the week of the date in the year given by year #Y, month #M, and day #D.")
  (type proc)
  (topic (time))
  (arity 3)
  (see (day-of-week datestr->datelist date->epoch-ns epoch-ns->datelist datestr datestr* now)))

(defhelp valid?
    (use "(valid? obj) => bool")
  (info "Return true if #obj is a valid object, nil otherwise. What exactly object validity means is undefined, but certain kind of objects such as graphics objects may be marked invalid when they can no longer be used because they have been disposed off by a subsystem and cannot be automatically garbage collected. Generally, invalid objects ought no longer be used and need to be discarded.")
  (type proc)
  (topic (boxed))
  (arity 1)
  (see (blob?)))

(defhelp blob?
    (use "(blob? obj) => bool")
  (info "Return true if #obj is a binary blob, nil otherwise.")
  (type proc)
  (topic (binary boxed))
  (arity 1)
  (see (blob->ascii85 blob->base64 blob->hex blob->str blob-free blob-chksum blob-equal? valid?)))

(defhelp enq
    (use "(enq proc)")
  (info "Put #proc on a special internal queue for sequential execution and execute it when able. #proc must be a prodedure that takes no arguments. The queue can be used to synchronizing i/o commands but special care must be taken that #proc terminates, or else the system might be damaged.")
  (type proc)
  (topic (concurrency))
  (arity 1)
  (see (task future synout synouty))
  (warn "Calls to enq can never be nested, neither explicitly or implicitly by calling enq anywhere else in the call chain!"))

(defhelp void
    (use "(void [any] ...)")
  (info "Always returns void, no matter what values are given to it. Void is a special value that is not printed in the console.")
  (type proc)
  (topic (lisp))
  (arity -1)
  (see (void?)))

(defhelp void?
    (use "(void? datum)")
  (info "Return true if #datum is the special symbol void, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (void)))

(defhelp fmt
    (use "(fmt s [args] ...) => str")
  (info "Format string #s that contains format directives with arbitrary many #args as arguments. The number of format directives must match the number of arguments. The format directives are the same as those for the esoteric and arcane programming language \"Go\", which was used on Earth for some time.")
  (type proc)
  (topic (str console ui))
  (arity -2)
  (see (out)))

(defhelp str+
    (use "(str+ [s] ...) => str")
  (info "Append all strings given to the function.")
  (type proc)
  (topic (str))
  (arity -1)
  (see (str?)))

(defhelp strbuild
    (use "(strbuild s n) => str")
  (info "Build a string by repeating string #s #n times.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (str+)))

(defhelp strsplit
    (use "(strsplit s del) => array")
  (info "Return an array of strings obtained from #s by splitting #s at each occurrence of string #del.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (str?)))

(defhelp str->chars
    (use "(str->chars s) => array")
  (info "Convert the UTF-8 string #s into an array of UTF-8 rune integers. An error may occur if the string is not a valid UTF-8 string.")
  (type proc)
  (topic (conversion str))
  (arity 1)
  (see (runes->str str->char char->str)))

(defhelp chars->str
    (use "(chars->str a) => str")
  (info "Convert an array of UTF-8 rune integers #a into a UTF-8 encoded string.")
  (type proc)
  (topic (conversion str))
  (arity 1)
  (see (str->runes str->char char->str)))

(defhelp strcnt
    (use "(strcnt s del) => int")
  (info "Returnt the number of non-overlapping substrings #del in #s.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (strsplit str-index)))

(defhelp array
    (use "(array [arg1] ...) => array")
  (info "Create an array containing the arguments given to it.")
  (type proc)
  (topic (array))
  (arity -1)
  (see (array? build-array)))

(defhelp build-array
    (use "(build-array n init) => array")
  (info "Create an array containing #n elements with initial value #init.")
  (type proc)
  (topic (array))
  (arity 2)
  (see (array array? array-slice array-append array-copy)))

(defhelp array-slice
    (use "(array-slice arr low high) => array")
  (info "Slice the array #arr starting from #low (inclusive) and ending at #high (exclusive) and return the slice. This function is destrcutive and mutates the slice. Use array-copy if you need a copy.")
  (type proc)
  (topic (array))
  (arity 3)
  (see (array-ref array-len array-append build-array array array-copy)))

(defhelp array-append
    (use "(array-append arr elem) => array")
  (info "Append #elem to the array #arr. This function is destructive and mutates the array. Use array-copy if you need a copy.")
  (type proc)
  (topic (array))
  (arity 2)
  (see (array-ref array-len build-array array-slice array array-copy)))

(defhelp array-len
    (use "(array-len arr) => int")
  (info "Return the length of array #arr.")
  (type proc)
  (topic (array))
  (arity 1)
  (see (len)))

(defhelp array?
    (use "(array? obj) => bool")
  (info "Return true of #obj is an array, nil otherwise.")
  (type proc)
  (topic (array))
  (arity 1)
  (see (seq? array)))

(defhelp array-ref
    (use "(array-ref arr n) => any")
  (info "Return the element of #arr at index #n. Arrays are 0-indexed.")
  (type proc)
  (topic (array))
  (arity 1)
  (see (array? array nth seq?)))

(defhelp array-set
    (use "(array-set arr idx value)")
  (info "Set the value at index #idx in #arr to #value. Arrays are 0-indexed. This mutates the array.")
  (type proc)
  (topic (array))
  (arity 3)
  (see (array? array)))

(defhelp array-map!
    (use "(array-map! arr proc)")
  (info "Traverse array #arr in unspecified order and apply #proc to each element. This mutates the array.")
  (type proc)
  (topic (array))
  (arity 2)
  (see (array-walk array-pmap! array? map seq?)))

(defhelp array-pmap!
    (use "(array-pmap! arr proc)")
  (info "Apply #proc in unspecified order in parallel to array #arr, mutating the array to contain the value returned by #proc each time. Because of the calling overhead for parallel execution, for many workloads array-map! might be faster if #proc is very fast. If #proc is slow, then array-pmap! may be much faster for large arrays on machines with many cores.")
  (type proc)
  (topic (array concurrency))
  (arity 2)
  (see (array-map! array-walk array? map seq?)))

(defhelp array-walk
    (use "(array-walk arr proc)")
  (info "Traverse the array #arr from first to last element and apply #proc to each element for side-effects. Function #proc takes the index and the array element at that index as argument. If #proc returns nil, then the traversal stops and the index is returned. If #proc returns non-nil, traversal continues. If #proc never returns nil, then the index returned is -1. This function does not mutate the array.")
  (type proc)
  (topic (array))
  (arity 2)
  (see (array-map! array-pmap! array? map seq?)))

(defhelp array->list
    (use "(array->list arr) => li")
  (info "Convert array #arr into a list.")
  (type proc)
  (topic (conversion array))
  (arity 1)
  (see (list->array array)))

(defhelp array-copy
    (use "(array-copy arr) => array")
  (info "Return a copy of #arr.")
  (type proc)
  (topic (array))
  (arity 1)
  (see (array array? array-map! array-pmap!)))

(defhelp array-reverse
    (use "(array-reverse arr) => array")
  (info "Create a copy of #arr that reverses the order of all of its elements.")
  (type proc)
  (topic (array))
  (arity 1)
  (see (reverse list-reverse str-reverse)))

(defhelp list-reverse
    (use "(list-reverse li) => li")
  (info "Create a reversed copy of #li.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (reverse array-reverse str-reverse)))

(defhelp str-reverse
    (use "(str-reverse s) => str")
  (info "Reverse string #s.")
  (type proc)
  (topic (str))
  (arity 1)
  (see (reverse array-reverse list-reverse)))

(defhelp instr
    (use "(instr s1 s2) => int")
  (info "Return the index of the first occurrence of #s2 in #s1 (from left), or -1 if #s1 does not contain #s2.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (str? index)))

(defhelp str-replace
    (use "(str-replace s t1 t2 n) => str")
  (info "Replace the first #n instances of substring #t1 in #s by #t2.")
  (type proc)
  (topic (str))
  (arity 4)
  (see (str-replace* str-count-substr)))

(defhelp str-replace*
    (use "(str-replace* s t1 t2) => str")
  (info "Replace all non-overlapping substrings #t1 in #s by #t2.")
  (type proc)
  (topic (str))
  (arity 3)
  (see (str-replace str-count-substr)))

(defhelp str-count-substr
    (use "(str-count-substr s1 s2) => int")
  (info "Count the number of non-overlapping occurrences of substring #s2 in string #s1.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (str-replace str-replace* instr)))

(defhelp dict
    (use "(dict [li]) => dict")
  (info "Create a dictionary. The option #li must be a list of the form '(key1 value1 key2 value2 ...). Dictionaries are unordered, hence also not sequences. Dictionaries are safe for concurrent access.")
  (type proc)
  (topic (dict))
  (arity -1)
  (see (array list)))

(defhelp dict?
    (use "(dict? obj) => bool")
  (info "Return true if #obj is a dict, nil otherwise.")
  (type proc)
  (topic (dict))
  (arity 1)
  (see (dict)))

(defhelp set
    (use "(set d key value)")
  (info "Set #value for #key in dict #d.")
  (type proc)
  (topic (dict))
  (arity 3)
  (see (dict get get-or-set)))

(defhelp set*
    (use "(set* d li)")
  (info "Set in dict #d the keys and values in list #li. The list #li must be of the form (key-1 value-1 key-2 value-2 ... key-n value-n). This function may be slightly faster than using individual #set operations.")
  (type proc)
  (topic (dict))
  (arity 2)
  (see (dict set)))

(defhelp get-or-set
    (use "(get-or-set d key value)")
  (info "Get the value for #key in dict #d if it already exists, otherwise set it to #value.")
  (type proc)
  (topic (dict))
  (arity 3)
  (see (dict? get set)))

(defhelp delete
    (use "(delete d key)")
  (info "Remove the value for #key in dict #d. This also removes the key.")
  (type proc)
  (topic (dict))
  (arity 2)
  (see (dict? get set)))

(defhelp has-key?
    (use "(has-key? d key) => bool")
  (info "Return true if #d has key #key, nil otherwise.")
  (type proc)
  (topic (dict))
  (arity 2)
  (see (dict? get set delete)))

(defhelp dict-copy
    (use "(dict-copy d) => dict")
  (info "Return a copy of dict #d.")
  (type proc)
  (topic (dict))
  (arity 1)
  (see (dict dict?)))

(defhelp dict-map!
    (use "(dict-map! d proc)")
  (info "Apply procedure #proc which takes the key and value as arguments to each key, value pair in dict #d and set the respective value in #d to the result of #proc. Keys are not changed.")
  (type proc)
  (topic (dict))
  (arity 2)
  (see (dict dict? dict-foreach)))

(defhelp dict-foreach
    (use "(dict-foreach d proc)")
  (info "Call #proc for side-effects with the key and value for each key, value pair in dict #d.")
  (type proc)
  (topic (dict))
  (arity 2)
  (see (dict-map! dict? dict)))

(defhelp dict->array
    (use "(dict-array d) => array")
  (info "Return an array that contains all key, value pairs of #d. A key comes directly before its value, but otherwise the order is unspecified.")
  (type proc)
  (topic (conversion dict))
  (arity 1)
  (see (dict->list dict)))

(defhelp dict->list
    (use "(dict->list d) => li")
  (info "Return a list of the form '(key1 value1 key2 value2 ...), where the order of key, value pairs is unspecified.")
  (type proc)
  (topic (conversion dict))
  (arity 1)
  (see (dict->array dict)))

(defhelp dict-empty?
    (use "(dict-empty? d) => bool")
  (info "Return true if dict #d is empty, nil otherwise. As crazy as this may sound, this can have O(n) complexity if the dict is not empty, but it is still going to be more efficient than any other method.")
  (type proc)
  (topic (dict))
  (arity 1)
  (see (dict)))

(defhelp strlen
    (use "(strlen s) => int")
  (info "Return the length of #s.")
  (type proc)
  (topic (str))
  (arity 1)
  (see (len seq? str?)))

(defhelp str-ref
    (use "(str-ref s n) => n")
  (info "Return the unicode char as integer at position #n in #s. Strings are 0-indexed.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (nth)))

(defhelp str-empty?
    (use "(str-empty? s) => bool")
  (info "Return true if the string #s is empty, nil otherwise.")
  (type proc)
  (topic (str))
  (arity 1)
  (see (strlen)))

(defhelp sym->str
    (use "(sym->str sym) => str")
  (info "Convert a symbol into a string.")
  (type proc)
  (topic (conversion lisp))
  (arity 1)
  (see (str->sym intern make-symbol)))

(defhelp str->sym
    (use "(str->sym s) => sym")
  (info "Convert a string into a symbol.")
  (type proc)
  (topic (conversion str))
  (arity 1)
  (see (sym->str intern make-symbol)))

(defhelp strmap
    (use "(strmap s proc) => str")
  (info "Map function #proc, which takes a number and returns a number, over all unicode characters in #s and return the result as new string.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (map)))

(defhelp strcase
    (use "(strcase s sel) => str")
  (info "Change the case of the string #s according to selector #sel and return a copy. Valid values for #sel are 'lower for conversion to lower-case, 'upper for uppercase, 'title for title case and 'utf-8 for utf-8 normalization (which replaces unprintable characters with \"?\").")
  (type proc)
  (topic (str))
  (arity 2)
  (see (strmap)))

(defhelp nonce
    (use "(nonce) => str")
  (info "Return a unique random string. This is not cryptographically secure but the string satisfies reasonable GUID requirements.")
  (type proc)
  (topic (system))
  (arity 0)
  (see (externalize internalize)))

(defhelp semver.build
    (use "(semver.build s) => str")
  (info "Return the build part of a semantic versioning string.")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.canonical semver.major semver.major-minor)))

(defhelp semver.canonical
    (use "(semver.canonical s) => str")
  (info "Return a canonical semver string based on a valid, yet possibly not canonical version string #s.")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.major)))

(defhelp semver.compare
    (use "(semver.compare s1 s2) => int")
  (info "Compare two semantic version strings #s1 and #s2. The result is 0 if #s1 and #s2 are the same version, -1 if #s1 < #s2 and 1 if #s1 > #s2.")
  (type proc)
  (topic (semver))
  (arity 2)
  (see (semver.major semver.major-minor)))

(defhelp semver.is-valid?
    (use "(semver.is-valid? s) => bool")
  (info "Return true if #s is a valid semantic versioning string, nil otherwise.")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.major semver.major-minor semver.compare)))

(defhelp semver.major
    (use "(semver.major s) => str")
  (info "Return the major part of the semantic versioning string.")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.major-minor semver.build)))

(defhelp semver.major-minor
    (use "(semver.major-minor s) => str")
  (info "Return the major.minor prefix of a semantic versioning string. For example, (semver.major-minor \"v2.1.4\") returns \"v2.1\".")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.major semver.build)))

(defhelp semver.max
    (use "(semver.max s1 s2) => str")
  (info "Canonicalize #s1 and #s2 and return the larger version of them.")
  (type proc)
  (topic (semver))
  (arity 2)
  (see (semver.compare)))

(defhelp semver.prerelease
    (use "(semver.prerelease s) => str")
  (info "Return the prerelease part of a version string, or the empty string if there is none. For example, (semver.prerelease \"v2.1.0-pre+build\") returns \"-pre\".")
  (type proc)
  (topic (semver))
  (arity 1)
  (see (semver.build semver.major semver.major-minor)))

(defhelp array-sort
    (use "(array-sort arr proc) => arr")
  (info "Destructively sorts array #arr by using comparison proc #proc, which takes two arguments and returns true if the first argument is smaller than the second argument, nil otherwise. The array is returned but it is not copied and modified in place by this procedure. The sorting algorithm is not guaranteed to be stable.")
  (type proc)
  (topic (array))
  (arity 2)
  (see (sort)))

(defhelp sort
    (use "(sort li proc) => li")
  (info "Sort the list #li by the given less-than procedure #proc, which takes two arguments and returns true if the first one is less than the second, nil otheriwse.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (array-sort)))

(defhelp strless
    (use "(strless s1 s2) => bool")
  (info "Return true if string #s1 < #s2 in lexicographic comparison, nil otherwise.")
  (type proc)
  (topic (str))
  (arity 2)
  (see (sort array-sort strcase)))

(defhelp list-ref
    (use "(list-ref li n) => any")
  (info "Return the element with index #n of list #li. Lists are 0-indexed.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (array-ref nth)))

(defhelp list-slice
    (use "(list-slice li low high) => li")
  (info "Return the slice of the list #li starting at index #low (inclusive) and ending at index #high (exclusive).")
  (type proc)
  (topic (lisp))
  (arity 3)
  (see (slice array-slice)))

(defhelp str->array
    (use "(str->array s) => array")
  (info "Return the string #s as an array of unicode glyph integer values.")
  (type proc)
  (topic (conversion str array))
  (arity 1)
  (see (array->str)))

(defhelp array->str
    (use "(array-str arr) => s")
  (info "Convert an array of unicode glyphs as integer values into a string. If the given sequence is not a valid UTF-8 sequence, an error is thrown.")
  (type proc)
  (topic (conversion array str))
  (arity 1)
  (see (str->array)))

(defhelp str->char
    (use "(str->char s)")
  (info "Return the first character of #s as unicode integer.")
  (type proc)
  (topic (conversion str char))
  (arity 1)
  (see (char->str)))

(defhelp str-slice
    (use "(str-slice s low high) => s")
  (info "Return a slice of string #s starting at character with index #low (inclusive) and ending at character with index #high (exclusive).")
  (type proc)
  (topic (str))
  (arity 3)
  (see (slice)))

(defhelp peek
    (use "(peek b pos end sel) => num")
  (info "Read a numeric value determined by selector #sel from binary blob #b at position #pos with endianness #end. Possible values for endianness are 'little and 'big, and possible values for #sel must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).")
  (type proc)
  (topic (binary))
  (arity 4)
  (see (poke read-binary)))

(defhelp poke
    (use "(poke b pos end sel n)")
  (info "Write numeric value #n as type #sel with endianness #end into the binary blob #b at position #pos. Possible values for endianness are 'little and 'big, and possible values for #sel must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).")
  (type proc)
  (topic (binary))
  (arity 5)
  (see (peek write-binary)))

(defhelp eval
    (use "(eval expr) => any")
  (info "Evaluate the expression #expr in the Z3S5 Machine Lisp interpreter and return the result. The evaluation environment is the system's environment at the time of the call.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (break apply)))

(defhelp beep
    (use "(beep sel)")
  (info "Play a built-in system sound. The argument #sel may be one of '(error start ready click okay confirm info).")
  (type proc)
  (topic (sound ui))
  (arity 1)
  (see (set-volume)))

(defhelp set-volume
    (use "(set-volume fl)")
  (info "Set the master volume for all sound to #fl, a value between 0.0 and 1.0.")
  (type proc)
  (topic (sound ui))
  (arity 1)
  (see (beep)))

(defhelp sleep
    (use "(sleep ms)")
  (info "Halt the current task execution for #ms milliseconds.")
  (type proc)
  (topic (system concurrency))
  (arity 1)
  (see (sleep-ns time now now-ns)))

(defhelp sleep-ns
    (use "(sleep-ns n")
  (info "Halt the current task execution for #n nanoseconds.")
  (type proc)
  (topic (system concurrency))
  (arity 1)
  (see (sleep time now now-ns)))

(defhelp add-hook-internal
    (use "(add-hook-internal hook proc) => int")
  (info "Add a procedure #proc to hook with numeric ID #hook and return this procedures hook ID. The function does not check whether the hook exists.")
  (type proc)
  (topic (system))
  (arity 2)
  (see (add-hook))
  (warn "Internal use only."))

(defhelp run-hook-internal
    (use "(run-hook-internal hook [args] ...)")
  (info "Run all hooks for numeric hook ID #hook with #args... as arguments.")
  (type proc)
  (topic (system))
  (arity -2)
  (see (run-hook))
  (warn "Internal use only."))

(defhelp remove-hook-internal
    (use "(remove-hook-internal hook id)")
  (info "Remove the hook with ID #id from numeric #hook.")
  (type proc)
  (topic (system))
  (arity 2)
  (see (remove-hook))
  (warn "Internal use only."))

(defhelp read-eval-reply
    (use "(read-eval-reply)")
  (info "Start a new read-eval-reply loop.")
  (type proc)
  (topic (system lisp))
  (arity 0)
  (see (end-input sys))
  (warn "Internal use only. This function might not do what you expect it to do."))

(defhelp defmacro
    (use "(defmacro name args body ...)")
  (info "Define a macro #name with argument list #args and #body. Macros are expanded at compile-time.")
  (type macro)
  (topic (lisp))
  (arity -3)
  (see (macro)))

(defhelp caar
    (use "(caar x) => any")
  (info "Equivalent to (car (car #x)).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cadr
    (use "(cadr x) => any")
  (info "Equivalent to (car (cdr #x)).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cdar
    (use "(cdar x) => any")
  (info "Equivalent to (cdr (car #x)).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cddr
    (use "(cddr x) => any")
  (info "Equivalent to (cdr (cdr #x)).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar caaar caadr cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp caaar
    (use "(caaar x) => any")
  (info "Equivalent to (car (car (car #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caadr cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp caadr
    (use "(caadr x) => any")
  (info "Equivalent to (car (car (cdr #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar cadar caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cadar
    (use "(cadar x) => any")
  (info "Equivalent to (car (cdr (car #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr caddr cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp caddr
    (use "(caddr x) => any")
  (info "Equivalent to (car (cdr (cdr #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr cadar cdaar cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cdaar
    (use "(cdaar x) => any")
  (info "Equivalent to (cdr (car (car #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdadr cddar cdddr nth 1st 2nd 3rd)))

(defhelp cdadr
    (use "(cdadr x) => any")
  (info "Equivalent to (cdr (car (cdr #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cddar cdddr nth 1st 2nd 3rd)))

(defhelp cddar
    (use "(cddar x) => any")
  (info "Equivalent to (cdr (cdr (car #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cdddr nth 1st 2nd 3rd)))

(defhelp cdddr
    (use "(cdddr x) => any")
  (info "Equivalent to (cdr (cdr (cdr #x))).")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar nth 1st 2nd 3rd)))

(defhelp not
    (use "(not x) => bool")
  (info "Return true if #x is nil, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (and or)))

(defhelp cons?
    (use "(cons? x) => bool")
  (info "return true if #x is not an atom, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (atom?)))

(defhelp print
    (use "(print x)")
  (info "Output #x on the host OS console and end it with a newline.")
  (type proc)
  (topic (console))
  (arity 1)
  (see (prin1 princ)))

(defhelp identity
    (use "(identity x)")
  (info "Return #x.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (apply equal?)))

(defhelp setq
    (use "(setq sym1 value1 ...)")
  (info "Set #sym1 (without need for quoting it) to #value, and so forth for any further symbol, value pairs.")
  (type special)
  (topic (lisp))
  (arity -2)
  (see (bind unbind)))

(defhelp defun
    (use "(defun ident (params ...) body ...)")
  (info "Define a function with name #ident, a possibly empty list of #params, and the remaining #body expressions. This is a macro for (setq ident (lambda (params ...) body ...)) and binds the lambda-form to the given symbol. Like lambdas, the #params of #defun allow for a #&rest keyword before the last parameter name. This binds all remaining arguments of a variadic function call to this parameter as a list.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (setq defmacro)))

(defhelp =
    (use "(= x y) => bool")
  (info "Return true if number #x equals number #y, nil otherwise.")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (eql? equal?)))

(defhelp null?
    (use "(null? li) => bool")
  (info "Return true if #li is nil, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (not list? cons?)))

(defhelp bool?
    (use "(bool? datum) => bool")
  (info "Return true if #datum is either true or nil. Note: This predicate only exists for type-completeness and you should never use it as part of testing whether something is true or false - per convention, a value is true if it is non-nil and not when it is true, which is the special boolean value this predicate tests in addition to nil.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (null? not)))

(defhelp setcar
    (use "(setcar li elem) => li")
  (info "Mutate #li such that its car is #elem. Same as rplaca.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (rplaca rplacd setcdr)))

(defhelp setcdr
    (use "(setcdr li1 li2) => li")
  (info "Mutate #li1 such that its cdr is #li2. Same as rplacd.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (rplacd rplaca setcar)))

(defhelp >
    (use "(> x y) => bool")
  (info "Return true if #x is larger than #y, nil otherwise.")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (< >= <= /=)))

(defhelp >=
    (use "(>= x y) => bool")
  (info "Return true if #x is larger than or equal to #y, nil otherwise.")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (> < <= /=)))

(defhelp <=
    (use "(<= x y) => bool")
  (info "Return true if #x is smaller than or equal to #y, nil otherwise.")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (> < >= /=)))

(defhelp /=
    (use "(/= x y) => bool")
  (info "Return true if number #x is not equal to #y, nil otherwise.")
  (type proc)
  (topic (numeric))
  (arity 2)
  (see (> >= < <=)))

(defhelp equal?
    (use "(equal? x y) => bool")
  (info "Return true if #x and #y are equal, nil otherwise. The equality is tested recursively for containers like lists and arrays.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (eq? eql?)))

(defhelp if
    (use "(if cond expr1 expr2) => any")
  (info "Evaluate #expr1 if #cond is true, otherwise evaluate #expr2.")
  (type macro)
  (topic (lisp))
  (arity 3)
  (see (cond when unless)))

(defhelp when
    (use "(when cond expr ...) => any")
  (info "Evaluate the expressions #expr if #cond is true, returns void otherwise.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (if cond unless)))

(defhelp unless
    (use "(unless cond expr ...) => any")
  (info "Evaluate expressions #expr if #cond is not true, returns void otherwise.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (if when cond)))

(defhelp let
    (use "(let args body ...) => any")
  (info "Bind each pair of symbol and expression in #args and evaluate the expressions in #body with these local bindings. Return the value of the last expression in #body.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (letrec)))

(defhelp letrec
    (use "(letrec args body ...) => any")
  (info "Recursive let binds the symbol, expression pairs in #args in a way that makes prior bindings available to later bindings and allows for recursive definitions in #args, then evaluates the #body expressions with these bindings.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (let)))

(defhelp append
    (use "(append li1 li2 ...) => li")
  (info "Concatenate the lists given as arguments.")
  (type proc)
  (topic (lisp))
  (arity -2)
  (see (cons)))

(defhelp and
    (use "(and expr1 expr2 ...) => any")
  (info "Evaluate #expr1 and if it is not nil, then evaluate #expr2 and if it is not nil, evaluate the next expression, until all expressions have been evaluated. This is a shortcut logical and.")
  (type macro)
  (topic (lisp))
  (arity -1)
  (see (or)))

(defhelp mapcar
    (use "(mapcar li proc) => li")
  (info "Return the list obtained from applying #proc to each elements in #li.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (map foreach)))

(defhelp or
    (use "(or expr1 expr2 ...) => any")
  (info "Evaluate the expressions until one of them is not nil. This is a logical shortcut or.")
  (type macro)
  (topic (lisp))
  (arity -1)
  (see (and)))

(defhelp list?
    (use "(list? obj) => bool")
  (info "Return true if #obj is a list, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (cons? atom? null?)))

(defhelp memq
    (use "(memq key li)")
  (info "Return the cdr of #li starting with #key if #li contains an element eq? to #key, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (member eq?)))

(defhelp member
    (use "(member key li) => li")
  (info "Return the cdr of #li starting with #key if #li contains an element equal? to #key, nil otherwise.")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (assoc equal?)))

(defhelp assq
    (use "(assq key alist) => li")
  (info  "Return the sublist of #alist that starts with #key if there is any, nil otherwise. Testing is done with eq?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (assoc assoc1 eq? alist? equal?)))

(defhelp assoc
    (use "(assoc key alist) => li")
  (info "Return the sublist of #alist that starts with #key if there is any, nil otherwise. Testing is done with equal?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)")
  (type proc)
  (topic (lisp))
  (arity 2)
  (see (assoc assoc1 alist? eq? equal?)))

(defhelp nreverse
    (use "(nreverse li) => li")
  (info "Destructively reverse #li.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (reverse)))

(defhelp list-last
    (use "(list-last li) => any")
  (info "Return the last element of #li.")
  (type proc)
  (topic (lisp))
  (arity 1)
  (see (reverse nreverse car 1st last)))

(defhelp nconc
    (use "(nconc li1 li2 ...) => li")
  (info "Concatenate #li1, #li2, and so forth, like with append, but destructively modifies #li1.")
  (type proc)
  (topic (lisp))
  (arity -1)
  (see (append)))

(defhelp while
    (use "(while test body ...) => any")
  (info "Evaluate the expressions in #body while #test is not nil.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (letrec dotimes dolist)))

(defhelp dolist
    (use "(dolist (name list [result]) body ...) => li")
  (info "Traverse the list #list in order, binding #name to each element subsequently and evaluate the #body expressions with this binding. The optional #result is the result of the traversal, nil if it is not provided.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (letrec foreach map)))

(defhelp dotimes
    (use "(dotimes (name count [result]) body ...) => any")
  (info "Iterate #count times, binding #name to the counter starting from 0 until the counter has reached count-1, and evaluate the #body expressions each time with this binding. The optional #result is the result of the iteration, nil if it is not provided.")
  (type macro)
  (topic (lisp))
  (arity -2)
  (see (letrec dolist while)))

 
(defhelp cond
    (use "(cond ((test1 expr1 ...) (test2 expr2 ...) ...) => any")
  (info "Evaluate the tests sequentially and execute the expressions after the test when a test is true. To express the else case, use (t exprn ...) at the end of the cond-clauses to execute #exprn...")
  (type special)
  (topic (lisp))
  (arity -1)
  (see (if when unless)))

(defhelp systask
    (use "(systask body ...)")
  (info "Evaluate the expressions of #body in parallel in a system task, which is similar to a future but cannot be forced.")
  (type special)
  (topic (concurrency))
  (arity -1)
  (see (future task)))

(defhelp lambda
    (use "(lambda args body ...) => closure")
  (info "Form a function closure (lambda term) with argument list in #args and body expressions #body.")
  (type special)
  (topic (lisp))
  (arity -2)
  (see (defun functional? macro? closure?)))

(defhelp macro
    (use "(macro args body ...) => macro")
  (info "Like a lambda term but the #body expressions are macro-expanded at compile time instead of runtime.")
  (type special)
  (topic (lisp))
  (arity -2)
  (see (defun lambda funcional? macro? closure?)))

(defhelp progn
    (use "(progn expr1 expr2 ...) => any")
  (info "Sequentially execute the expressions #expr1, #expr2, and so forth, and return the value of the last expression.")
  (type special)
  (topic (lisp))
  (arity -1)
  (see (defun lambda cond)))

(defhelp quote
    (use "(quote x)")
  (info "Quote symbol #x, so it evaluates to #x instead of the value bound to it. Syntactic shortcut is '.")
  (type special)
  (topic (lisp))
  (arity 1)
  (see (quasiquote)))

(defhelp quasiquote
    (use "(quasiquote li)")
  (info "Quote #li, except that values in #li may be unquoted (~evaluated) when prefixed with \",\" and embedded lists can be unquote-spliced by prefixing them with unquote-splice \",@\". An unquoted expression's value is inserted directly, whereas unquote-splice inserts the values of a list in-sequence into the embedding list. Quasiquote is used in combination with gensym to define non-hygienic macros. In Z3S5 Lisp, \",\" and \",@\" are syntactic markers and there are no corresponding unquote and unquote-splice functions. The shortcut for quasiquote is \"`\".")
  (type special)
  (topic (lisp))
  (arity 1)
  (see (quote gensym macro defmacro)))

(defhelp str->expr
    (use "(str->expr s [default]) => any")
  (info "Convert a string #s into a Lisp expression. If #default is provided, it is returned if an error occurs, otherwise an error is raised.")
  (type proc)
  (topic (conversion system str lisp))
  (arity -1)
  (see (expr->str str->expr* openstr externalize internalize)))

(defhelp expr->str
    (use "(expr->str expr) => str")
  (info "Convert a Lisp expression #expr into a string. Does not use a stream port.")
  (type proc)
  (topic (conversion system lisp str))
  (arity 1)
  (see (str->expr str->expr* openstr internalize externalize)))

(defhelp str->expr*
    (use "(str->expr* s [default]) => li")
  (info "Convert a string #s into a list consisting of the Lisp expressions in #s. If #default is provided, then this value is put in the result list whenever an error occurs. Otherwise an error is raised. Notice that it might not always be obvious what expression in #s triggers an error, since this hinges on the way the internal expession parser works.")
  (type proc)
  (topic (conversion system str lisp))
  (arity -1)
  (see (str->expr expr->str openstr internalize externalize)))

(defhelp blob->str
    (use "(blob->str b [start] [end]) => str")
  (info "Convert blob #b into a string. Notice that the string may contain binary data that is not suitable for displaying and does not represent valid UTF-8 glyphs. If the optional #start and #end are provided, then only bytes from #start (inclusive) to #end (exclusive) are converted.")
  (type proc)
  (topic (conversion binary))
  (arity -2)
  (see (str->blob valid? blob?)))

(defhelp str->blob
    (use "(str->blob s) => blob")
  (info "Convert string #s into a blob.")
  (type proc)
  (topic (conversion binary str))
  (arity 1)
  (see (blob->str)))

(defhelp blob->hex
    (use "(blob->hex b [start] [end]) => str")
  (info "Convert the blob #b to a hexadecimal string of byte values. If the optional #start and #end are provided, then only bytes from #start (inclusive) to #end (exclusive) are converted.")
  (type proc)
  (topic (conversion binary))
  (arity -2)
  (see (hex->blob str->blob valid? blob? blob->base64 blob->ascii85)))

(defhelp hex->blob
    (use "(hex->blob str) => blob")
  (info "Convert hex string #str to a blob. This will raise an error if #str is not a valid hex string.")
  (type proc)
  (topic (conversion binary))
  (arity 1)
  (see (blob->hex base64->blob ascii85->blob str->blob)))

(defhelp blob->base64
    (use "(blob->base64 b [start] [end]) => str")
  (info "Convert the blob #b to a base64 encoded string. If the optional #start and #end are provided, then only bytes from #start (inclusive) to #end (exclusive) are converted.")
  (type proc)
  (topic (conversion binary))
  (arity -2)
  (see (base64->blob valid? blob? blob->str blob->hex blob->ascii85)))

(defhelp base64->blob
    (use "(base64->blob str) => blob")
  (info "Convert the base64 encoded string #str to a binary blob. This will raise an error if #str is not a valid base64 encoded string.")
  (type proc)
  (topic (conversion binary))
  (arity 1)
  (see (blob->base64 hex->blob ascii85->blob str->blob)))

(defhelp blob->ascii85
    (use "(blob->ascii85 b [start] [end]) => str")
  (info "Convert the blob #b to an ascii85 encoded string. If the optional #start and #end are provided, then only bytes from #start (inclusive) to #end (exclusive) are converted.")
  (type proc)
  (topic (conversion binary))
  (arity -2)
  (see (blob->hex blob->str blob->base64 valid? blob?)))

(defhelp ascii85->blob
    (use "(ascii85->blob str) => blob")
  (info "Convert the ascii85 encoded string #str to a binary blob. This will raise an error if #str is not a valid ascii85 encoded string.")
  (type proc)
  (topic (conversion binary))
  (arity 1)
  (see (blob->ascii85 base64->blob str->blob hex->blob)))

(defhelp blob-equal?
    (use "(blob-equal? b1 b2) => bool")
  (info "Return true if #b1 and #b2 are equal, nil otherwise. Two blobs are equal if they are either both invalid, both contain no valid data, or their contents contain exactly the same binary data.")
  (type proc)
  (topic (binary))
  (arity 2)
  (see (str->blob blob->str blob-free)))

(defhelp blob-chksum
    (use "(blob-chksum b [start] [end]) => blob")
  (info "Return the checksum of the contents of blob #b as new blob. The checksum is cryptographically secure. If the optional #start and #end are provided, then only the bytes from #start (inclusive) to #end (exclusive) are checksummed.")
  (type proc)
  (topic (binary))
  (arity -2)
  (see (fchksum blob-free)))

(defhelp blob-free
    (use "(blob-free b)")
  (info "Frees the binary data stored in blob #b and makes the blob invalid.")
  (type proc)
  (topic (binary))
  (arity 1)
  (see (make-blob valid? str->blob blob->str blob-equal?)))

(defhelp make-blob
    (use "(make-blob n) => blob")
  (info "Make a binary blob of size #n initialized to zeroes.")
  (type proc)
  (topic (binary))
  (arity 1)
  (see (blob-free valid? blob-equal?)))

(defhelp feature?
    (use "(feature? sym) => bool")
  (info "Return true if the Lisp feature identified by symbol #sym is available, nil otherwise.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (*reflect* on-feature)))

(defhelp *reflect*
    (use "*reflect* => li")
  (info "The list of feature identifiers as symbols that this Lisp implementation supports.")
  (type symbol)
  (topic (system))
  (arity 0)
  (see (feature? on-feature)))

(defhelp on-feature
    (use "(on-feature sym body ...) => any")
  (info "Evaluate the expressions of #body if the Lisp feature #sym is supported by this implementation, do nothing otherwise.")
  (type macro)
  (topic (system))
  (arity -2)
  (see (feature? *reflect*)))

(defhelp testing
    (use "(testing name)")
  (info "Registers the string #name as the name of the tests that are next registered with expect.")
  (type macro)
  (topic (system))
  (arity 1)
  (see (expect expect-err expect-ok run-selftest)))

(defhelp expect
    (use "(expect value given)")
  (info "Registers a test under the current test name that checks that #value is returned by #given. The test is only executed when (run-selftest) is executed.")
  (type macro)
  (topic (system))
  (arity 2)
  (see (expect-err expect-ok run-selftest testing)))

(defhelp expect-err
    (use "(expect-err expr ...)")
  (info "Registers a test under the current test name that checks that #expr produces an error.")
  (type macro)
  (topic (system))
  (arity -2)
  (see (expect expect-ok run-selftest testing)))

(defhelp expect-ok
    (use "(expect-err expr ...)")
  (info "Registers a test under the current test name that checks that #expr does not produce an error.")
  (type macro)
  (topic (system))
  (arity -2)
  (see (expect expect-ok run-selftest testing)))

(defhelp expect-true
       (use "(expect-true expr ...)")
  (info "Registers a test under the current test name that checks that #expr is true (not nil).")
  (type macro)
  (topic (system))
  (arity -2)
  (see (expect expect-ok run-selftest testing)))

(defhelp expect-false
       (use "(expect-false expr ...)")
  (info "Registers a test under the current test name that checks that #expr is nil.")
  (type macro)
  (topic (system))
  (arity -2)
  (see (expect expect-ok run-selftest testing)))

(defhelp run-selftest
    (use "(run-selftest [silent?]) => any")
  (info "Run a diagnostic self-test of the Z3S5 Machine. If #silent? is true, then the self-test returns a list containing a boolean for success, the number of tests performed, the number of successes, the number of errors, and the number of failures. If #silent? is not provided or nil, then the test progress and results are displayed. An error indicates a problem with the testing, whereas a failure means that an expected value was not returned.")
  (type proc)
  (topic (system))
  (arity -2)
  (see (expect testing)))

(defhelp expand-macros
    (use "(expand-macros expr) => expr")
  (info "Expands the macros in #expr. This is an ordinary function and will not work on already compiled expressions such as a function bound to a symbol. However, it can be used to expand macros in expressions obtained by #read.")
  (type proc)
  (arity 1)
  (topic (system lisp))
  (see (internalize externalize load-library)))

(defhelp protect
    (use "(protect [sym] ...)")
  (info "Protect symbols #sym ... against changes or rebinding. The symbols need to be quoted. This operation requires the permission 'allow-protect to be set.")
  (type proc)
  (topic (system))
  (arity -1)
  (see (protected? unprotect dict-protect dict-unprotect dict-protected? permissions permission? setq bind interpret)))

(defhelp unprotect
    (use "(unprotect [sym] ...)")
  (info "Unprotect symbols #sym ..., allowing mutation or rebinding them. The symbols need to be quoted. This operation requires the permission 'allow-unprotect to be set, or else an error is caused.")
  (type proc)
  (topic (system))
  (arity -1)
  (see (protect protected? dict-unprotect dict-protected? permissions permission? setq bind interpret)))

(defhelp protected?
    (use "(protected? sym)")
  (info "Return true if #sym is protected, nil otherwise.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (protect unprotect dict-unprotect dict-protected? permission permission? setq bind interpret)))

(defhelp dict-protect
    (use "(dict-protect d)")
  (info "Protect dict #d against changes. Attempting to set values in a protected dict will cause an error, but all values can be read and the dict can be copied. This function requires permission 'allow-protect.")
  (type proc)
  (topic (system dict))
  (arity 1)
  (see (dict-unprotect dict-protected? protect unprotect protected? permissions permission?))
  (warn "Protected dicts are full readable and can be copied, so you may need to use protect to also prevent changes to the toplevel symbol storing the dict!"))

(defhelp dict-protected?
    (use "(dict-protected? d)")
  (info "Return true if the dict #d is protected against mutation, nil otherwise.")
  (type proc)
  (topic (system dict))
  (arity 1)
  (see (dict-protect dict-unprotect protect unprotect protected? permissions permission?)))

(defhelp dict-unprotect
    (use "(dict-unprotect d)")
  (info "Unprotect the dict #d so it can be mutated again. This function requires permission 'allow-unprotect.")
  (type proc)
  (topic (system dict))
  (arity 1)
  (see (dict-protect dict-protected? protect unprotect protected? permissions permission?)))

(defhelp when-permission
    (use "(when-permission perm body ...) => any")
  (info "Execute the expressions in #body if and only if the symbolic permission #perm is available.")
  (type macro)
  (topic (system))
  (arity -2)
  (see (permission?)))

(defhelp permissions
    (use "(permissions)")
  (info "Return a list of all active permissions of the current interpreter. Permissions are: #load-prelude - load the init file on start; #load-user-init - load the local user init on startup, file if present; #allow-unprotect - allow the user to unprotect protected symbols (for redefining them); #allow-protect - allow the user to protect symbols from redefinition or unbinding; #interactive - make the session interactive, this is particularly used during startup to determine whether hooks are installed and feedback is given. Permissions have to generally be set or removed in careful combination with #revoke-permissions, which redefines symbols and functions.")
  (type proc)
  (topic (system))
  (arity 0)
  (see (set-permissions permission? when-permission sys)))

(defhelp set-permissions
    (use "(set-permissions li)")
  (info "Set the permissions for the current interpreter. This will trigger an error when the permission cannot be set due to a security violation. Generally, permissions can only be downgraded (made more stringent) and never relaxed. See the information for #permissions for an overview of symbolic flags.")
  (arity 1)
  (topic (system))
  (see (permissions permission? when-permission sys)))

(defhelp permission?
    (use "(permission? sym [default]) => bool")
  (info "Return true if the permission for #sym is set, nil otherwise. If the permission flag is unknown, then #default is returned. The default for #default is nil.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (permissions set-permissions when-permission sys)))

(defhelp stropen
    (use "(stropen s) => streamport")
  (info "Open the string #s as input stream.")
  (type proc)
  (topic (str io))
  (arity 1)
  (see (open close)))

(defhelp close
    (use "(close p)")
  (info "Close the port #p. Calling close twice on the same port should be avoided.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (open stropen)))

(defhelp file-port?
    (use "(file-port? p) => bool")
  (info "Return true if #p is a file port, nil otherwise.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (port? str-port? open stropen)))

(defhelp str-port?
    (use "(str-port? p) => bool")
  (info "Return true if #p is a string port, nil otherwise.")
  (type proc)
  (topic (fileio str io))
  (arity 1)
  (see (port? file-port? stropen open)))

(defhelp read
    (use "(read p) => any")
  (info "Read an expression from input port #p.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (input write)))

(defhelp write
    (use "(write p datum) => int")
  (info "Write #datum to output port #p and return the number of bytes written.")
  (type proc)
  (topic (fileio io))
  (arity 2)
  (see (write-binary write-binary-at read close open)))

(defhelp write-string
    (use "(write-string p s) => int")
  (info "Write string #s to output port #p and return the number of bytes written. LF are *not* automatically converted to CR LF sequences on windows.")
  (type proc)
  (topic (fileio io))
  (arity 2)
  (see (write write-binary write-binary-at read close open)))

(defhelp read-string
    (use "(read-string p delstr) => str")
  (info "Reads a string from port #p until the single-byte delimiter character in #delstr is encountered, and returns the string including the delimiter. If the input ends before the delimiter is encountered, it returns the string up until EOF. Notice that if the empty string is returned then the end of file must have been encountered, since otherwise the string would contain the delimiter.")
  (type proc)
  (topic (fileio io))
  (arity 2)
  (see (read read-binary write-string write read close open)))

(defhelp read-binary
    (use "(read-binary p buff n) => int")
  (info "Read #n or less bytes from input port #p into binary blob #buff. If #buff is smaller than #n, then an error is raised. If less than #n bytes are available before the end of file is reached, then the amount k of bytes is read into #buff and k is returned. If the end of file is reached and no byte has been read, then 0 is returned. So to loop through this, read into the buffer and do something with it while the amount of bytes returned is larger than 0.")
  (type proc)
  (topic (fileio io))
  (arity 3)
  (see (write-binary read close open)))

(defhelp write-binary
    (use "(write-binary p buff n offset) => int")
  (info "Write #n bytes starting at #offset in binary blob #buff to the stream port #p. This function returns the number of bytes actually written.")
  (type proc)
  (topic (fileio io))
  (arity 4)
  (see (write-binary-at read-binary write close open)))

(defhelp write-binary-at
    (use "(write-binary-at p buff n offset fpos) => int")
  (info "Write #n bytes starting at #offset in binary blob #buff to the seekable stream port #p at the stream position #fpos. If there is not enough data in #p to overwrite at position #fpos, then an error is caused and only part of the data might be written. The function returns the number of bytes actually written.")
  (type proc)
  (topic (fileio io))
  (arity 5)
  (see (read-binary write-binary write close open)))

(defhelp open
    (use "(open file-path [modes] [permissions]) => int")
  (info "Open the file at #file-path for reading and writing, and return the stream ID. The optional #modes argument must be a list containing one of '(read write read-write) for read, write, or read-write access respectively, and may contain any of the following symbols: 'append to append to an existing file, 'create for creating the file if it doesn't exist, 'exclusive for exclusive file access, 'truncate for truncating the file if it exists, and 'sync for attempting to sync file access. The optional #permissions argument must be a numeric value specifying the Unix file permissions of the file. If these are omitted, then default values '(read-write append create) and 0640 are used.")
  (type proc)
  (topic (fileio io))
  (arity -2)
  (see (stropen close read write)))

(defhelp dir
    (use "(dir [path]) => li")
  (info "Obtain a directory list for #path. If #path is not specified, the current working directory is listed.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (dir? open close read write)))

(defhelp dir?
    (use "(dir? path) => bool")
  (info "Check if the file at #path is a directory and return true, nil if the file does not exist or is not a directory.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (file-exists? dir open close read write)))

(defhelp fdelete
    (use "(fdelete path)")
  (info "Removes the file or directory at #path.")
  (type proc)
  (topic (fileio io))
  (arity 1)
  (see (file-exists? dir? dir))
  (warn "This function also deletes directories containing files and all of their subdirectories!"))

(defhelp ling.soundex
    (use "(ling.soundex s) => str")
  (info "Compute the Soundex representation of string #s.")
  (type proc)
  (topic (ling str))
  (arity 1)
   (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.metaphone
    (use "(ling.metaphone s) => str")
  (info "Compute the Metaphone representation of string #s.")
  (type proc)
  (topic (ling str))
  (arity 1)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.soundex)))

(defhelp ling.nysiis
    (use "(ling.nysiis s) => str")
  (info "Compute the Nysiis representation of string #s.")
  (type proc)
  (topic (ling str))
  (arity 1)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.metaphone ling.soundex)))
(defhelp ling.porter
    (use "(ling.porter s) => str")
  (info "Compute the stem of word string #s using the Porter stemming algorithm.")
  (type proc)
  (topic (ling str))
  (arity 1)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.match-rating-codex
    (use "(ling.match-rating-codex s) => str")
  (info "Compute the Match-Rating-Codex of string #s.")
  (type proc)
  (topic (ling str))
  (arity 1)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.damerau-levenshtein
    (use "(ling.damerau-levenshtein s1 s2) => num")
  (info "Compute the Damerau-Levenshtein distance between #s1 and #s2.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming 
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.hamming
    (use "(ling-hamming s1 s2) => num")
  (info "Compute the Hamming distance between #s1 and #s2.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.jaro
    (use "(ling.jaro s1 s2) => num")
  (info "Compute the Jaro distance between #s1 and #s2.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.jaro-winkler
    (use "(ling.jaro-winkler s1 s2) => num")
  (info "Compute the Jaro-Winkler distance between #s1 and #s2.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.levenshtein
    (use "(ling.levenshtein s1 s2) => num")
  (info "Compute the Levenshtein distance between #s1 and #s2.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp ling.match-rating-compare
    (use "(ling.match-rating-compare s1 s2) => bool")
  (info "Returns true if #s1 and #s2 are equal according to the Match-rating Comparison algorithm, nil otherwise.")
  (type proc)
  (topic (ling str))
  (arity 2)
  (see (ling.match-rating-compare ling.levenshtein ling.jaro-winkler ling.jaro ling.hamming ling.damerau-levenshtein
				  ling.match-rating-codex ling.porter ling.nysiis ling.metaphone ling.soundex)))

(defhelp run-selftest
    (use "(run-selftest)")
  (info "Run a self test of the Z3S5 Lisp system and report errors to standard output.")
  (type proc)
  (arity 0)
  (topic (system))
  (see (help testing)))

(when (member 'db *reflect*)
  (defhelp db.open
      (use "(db.open fi) => db")
    (info "Opens an sqlite3 DB or creates a new, empty database at file path #fi.")
    (type proc)
    (arity 1)
    (topic (db io))
    (see (db.close db.exec db.query)))

  (defhelp db.close
      (use "(db.close db)")
    (info "Close the database #db.")
    (type proc)
    (arity 1)
    (topic (db io))
    (see (db.open db.open* db.exec db.query)))

  (defhelp db.open*
      (use "(db.open* sel) => db")
    (info "Open a temporary database if #sel is 'temp or an in-memory database if #sel is 'mem.")
    (type proc)
    (topic (db io))
    (arity 1)
    (see (db.open db.close db.exec db.query)))

  (defhelp db.exec
      (use "(db.exec db stmt [args] ...)")
    (info "Execute the SQL statement #stmt in database #db, binding any optional #args to the open variable slots in it. This function does not return anything, use db.query to execute a query that returns rows as result.")
    (type proc)
    (topic (db io))
    (arity -3)
    (see (db.query db.open db.close db.open*)))

  (defhelp db.query
      (use "(db.query db stmt [args] ...) => db-result")
    (info "Query #db with SQL statement #stmt, binding any optional #args to the open variable slots in it. This function returns a #db-result that can be used to loop through rows with db.step and obtain columns in them using the various accessor methods.")
    (type proc)
    (topic (db io))
    (arity -3)
    (see (db.exec db.step db.int db.cname db.float db.str db.expr db.blob)))

  (defhelp db.step
      (use "(db.step db-result) => bool")
    (info "Obtain the next result row in #db-result and return true, or return nil of there is no more row in the result.")
    (type proc)
    (topic (db io))
    (arity 1)
    (see (db.query db.row db.rows)))

  (defhelp db.close-result
      (use "(db.close-result db-result)")
    (info "Close the #db-result. It is invalid afterwards. This should be done to avoid memory leaks after the result has been used.")
    (type proc)
    (topic (db io))
    (arity 1)
    (see (db.reset db.step db.close)))

  (defhelp db.int
      (use "(db.int db-result n) => int")
    (info "Get the content of column #n in #db-result as integer.")
    (type proc)
    (topic (db))
    (arity 2)
    (see (db.float db.str db.blob)))

  (defhelp db.str
      (use "(db.str db-result n) => str")
    (info "Get the content of column #n in #db-result as string.")
    (type proc)
    (topic (db))
    (arity 2)
    (see (db.blob db.int db.float)))

  (defhelp db.float
      (use "(db.float db-result n) => fl")
    (info "Get the content of column #n in #db-result as float.")
    (type proc)
    (topic (db))
    (arity 2)
    (see (db.int db.str)))

  (defhelp db.blob
      (use "(db.blob db-result n) => fl")
    (info "Get the content of column #n in #db-result as blob. A blob is a boxed memory area holding binary data.")
    (type proc)
    (topic (db binary))
    (arity 2)
    (see (db.str)))

  (defhelp db.result-column-count
      (use "(db.result-column-count db-result) => int")
    (info "Get the number of columns in the rows of #db-result.")
    (type proc)
    (topic (db))
    (arity 1)
    (see (db.result-columns)))

  (defhelp db.result-columns
      (use "(db.result-columns db-result) => li")
    (info "Get a list of column specifications for #db-result, each consisting of a list with the column name and the column type as string, as these were provided to the query. Since queries support automatic type conversions, this need not reflect the column types in the database schema.")
    (type proc)
    (topic (db))
    (arity 1)
    (see (db.result-column-count)))

  (defhelp db.row
      (use "(db.row db-result) => li")
    (info "Return all columns of the current row in #db-result as list. They have the respective base types INT, FLOAT, BLOB, and TEXT.")
    (type proc)
    (topic (db))
    (arity 1)
    (see (db.rows)))
  )

(defhelp error?
    (use "(error? datum) => bool")
  (info "Return true if #datum is a special error value, nil otherwise.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (*last-error* error->str error eof? valid?)))

(defhelp error->str
    (use "(error->str datum) => str")
  (info "Convert a special error value to a string.")
  (type proc)
  (topic (system))
  (arity 1)
  (see (*last-error* error error?)))

(defhelp declare-volatile
    (use "(declare-volatile sym)")
  (info "Declares #sym, which has to be quoted, as a volatile toplevel symbol. Volatile toplevel symbols are neither saved to nor loaded from zimages.")
  (type proc)
  (arity 1)
  (topic (zimage system))
  (see (save-zimage load-zimage declare-unprotected)))

;;; Help end

;;;  Copyright (c) 2019-2022 Erich Rast
;;;
;;;  The above copyright notice and this permission notice shall be included in
;;;  all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;  DEALINGS IN THE SOFTWARE.
