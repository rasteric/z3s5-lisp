package z3s5

import (
	"bytes"
	"encoding/ascii85"
	"encoding/base64"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/nukata/goarith"
	"github.com/rasteric/hooks"
	"github.com/teris-io/shortid"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/mod/semver"
	"golang.org/x/text/unicode/norm"
)

// Define_Base defines basic Lisp functions that should work on any platform.
func (interp *Interp) Define_Base() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("base"), reflect})

	// The normal functions.
	interp.Def("car", 1, func(a []any) any {
		if a[0] == Nil {
			return Nil
		}
		return a[0].(*Cell).Car
	})

	interp.Def("cdr", 1, func(a []any) any {
		if a[0] == Nil {
			return Nil
		}
		return a[0].(*Cell).Cdr
	})

	interp.Def("cons", 2, func(a []any) any {
		return &Cell{a[0], a[1]}
	})

	interp.Def("atom?", 1, func(a []any) any {
		if j, ok := a[0].(*Cell); ok && j != Nil {
			return Nil
		}
		return true
	})

	// (_bound? sym) returns true if the symbol is bound, nil otherwise.
	interp.Def("_bound?", 1, func(a []any) any {
		s, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("_bound?: expected a symbol as argument, given %v", Str(a[0])))
		}
		_, ok = interp.GetGlobalVar(s)
		if !ok {
			return Nil
		}
		return true
	})

	// (bind a b) bind a to b in the global toplevel environment, where a and b are evaluated (unlike setq)
	interp.Def("bind", 2, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("bind: expected a symbol as argument, given %v", Str(a[0])))
		}
		interp.SetGlobalVar(sym, a[1])
		return Void
	})

	// (unbind a) unbind a in the global toplevel environment, where a is evaluated
	interp.Def("unbind", 1, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("unbind: expected a symbol as argument, given %v", Str(a[0])))
		}
		interp.UnbindGlobalVar(sym)
		return Void
	})

	// (_protect sym) protect symbol against global changes of its value or rebinding
	interp.Def("_protect", 1, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("_protect: expected a bound symbol as argument, given %v", Str(a[0])))
		}
		if !interp.pc.Perm().AllowProtect {
			panic(fmt.Sprintf("_protect: security violation: no 'allow-protect permission for '%v", Str(sym)))
		}
		v, ok := interp.globals.Load(sym)
		if !ok {
			panic(fmt.Sprintf("_protect: tried to protect an unbound symbol '%v", Str(sym)))
		}
		holder := v.(ValueHolder)
		if holder.Protected {
			return Void
		}
		holder.Protected = true
		interp.globals.Store(sym, holder)
		return Void
	})

	// (_unprotect sym) unprotect symbol
	interp.Def("_unprotect", 1, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("_unprotect: expected a bound symbol as argument, given %v", Str(a[0])))
		}
		if !interp.pc.Perm().AllowUnprotect {
			panic(fmt.Sprintf("_unprotect: no 'allow-unprotect permission for '%v", Str(sym)))
		}
		v, ok := interp.globals.Load(sym)
		if !ok {
			panic(fmt.Sprintf("_unprotect: tried to unprotect an unbound symbol '%v", Str(sym)))
		}
		holder := v.(ValueHolder)
		if !holder.Protected {
			return Void
		}
		holder.Protected = false
		interp.globals.Store(sym, holder)
		return Void
	})

	// (protected? sym) => bool
	interp.Def("protected?", 1, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(fmt.Sprintf("protected?: expected a symbol as argument, given %v", Str(a[0])))
		}
		v, ok := interp.globals.Load(sym)
		if !ok {
			return Nil
		}
		return AsLispBool(v.(ValueHolder).Protected)
	})

	interp.Def("eq?", 2, func(a []any) any {
		if _, ok := a[0].([]any); ok {
			return AsLispBool(Same(a[0], a[1]))
		}
		if _, ok := a[0].(map[any]any); ok {
			return AsLispBool(Same(a[0], a[1]))
		}
		if a[0] == a[1] { // Cells are compared by address.
			return true
		}
		return Nil
	})

	interp.Def("list", -1, func(a []any) any {
		return a[0]
	})

	interp.Def("rplaca", 2, func(a []any) any {
		a[0].(*Cell).Car = a[1]
		return a[1]
	})

	interp.Def("rplacd", 2, func(a []any) any {
		a[0].(*Cell).Cdr = a[1]
		return a[1]
	})

	interp.Def("len", 1, func(a []any) any {
		switch x := a[0].(type) {
		case *Cell:
			return goarith.AsNumber(x.Len())
		case string: // Each multi-bytes character counts 1.
			return goarith.AsNumber(utf8.RuneCountInString(x))
		case []any:
			return goarith.AsNumber(len(a[0].([]any)))
		case *Dict:
			c := 0
			a[0].(*Dict).Data.Range(func(k, v any) bool {
				c++
				return true
			})
			return goarith.AsNumber(c)
		case *Boxed:
			if a[0].(*Boxed).Sort == BoxedBlob {
				return goarith.AsNumber(len(a[0].(*Boxed).Datum.([]byte)))
			}
		}
		panic(NewEvalError("list, string, array, blob, or dict expected", a[0]))
	})

	interp.Def("str?", 1, func(a []any) any {
		if _, ok := a[0].(string); ok {
			return true
		}
		return Nil
	})

	interp.Def("num?", 1, func(a []any) any {
		if goarith.AsNumber(a[0]) != nil {
			return true
		}
		return Nil
	})

	interp.Def("sym?", 1, func(a []any) any {
		if _, ok := a[0].(*Sym); ok {
			return true
		}
		return Nil
	})

	interp.Def("bool?", 1, func(a []any) any {
		if _, ok := a[0].(bool); ok {
			return true
		}
		if s, ok := a[0].(*Cell); ok {
			if s == Nil {
				return true
			}
		}
		return Nil
	})

	interp.Def("boxed?", 1, func(a []any) any {
		if _, ok := a[0].(*Boxed); ok {
			return true
		}
		return Nil
	})

	interp.Def("closure?", 1, func(a []any) any {
		if _, ok := a[0].(*Closure); ok {
			return true
		}
		return Nil
	})

	interp.Def("intrinsic?", 1, func(a []any) any {
		if _, ok := a[0].(*BuiltInFunc); ok {
			return true
		}
		return Nil
	})

	interp.Def("macro?", 1, func(a []any) any {
		if _, ok := a[0].(*Macro); ok {
			return true
		}
		return Nil
	})

	interp.Def("functional-arity", 1, func(a []any) any {
		f, ok := a[0].(*BuiltInFunc)
		if ok {
			return goarith.AsNumber(f.fixedArgs())
		}
		f2, ok := a[0].(*Closure)
		if ok {
			return goarith.AsNumber(f2.fixedArgs())
		}
		f3, ok := a[0].(*Macro)
		if ok {
			return goarith.AsNumber(f3.fixedArgs())
		}
		panic(fmt.Sprintf("functional-arity: expected procedure, given %v", Str(a[0])))
	})

	interp.Def("functional-has-rest?", 1, func(a []any) any {
		f, ok := a[0].(*Func)
		if ok {
			return AsLispBool(f.hasRest())
		}
		f2, ok := a[0].(*Closure)
		if ok {
			return AsLispBool(f2.hasRest())
		}
		f3, ok := a[0].(*Macro)
		if ok {
			return AsLispBool(f3.hasRest())
		}
		panic(fmt.Sprintf("functional-has-rest?: expected procedure, given %v", Str(a[0])))
	})

	interp.Def("eql?", 2, func(a []any) any {
		if interp.equal(a[0], a[1]) {
			return true
		}
		return Nil
	})

	interp.Def("<", 2, func(a []any) any {
		if goarith.AsNumber(a[0]).Cmp(goarith.AsNumber(a[1])) < 0 {
			return true
		}
		return Nil
	})

	interp.Def("%", 2, func(a []any) any {
		_, q := goarith.AsNumber(a[0]).QuoRem(goarith.AsNumber(a[1]))
		return q
	})

	interp.Def("mod", 2, func(a []any) any {
		x, y := goarith.AsNumber(a[0]), goarith.AsNumber(a[1])
		xs, ys := x.Cmp(Number0), y.Cmp(Number0)
		_, q := x.QuoRem(y)
		if (xs < 0 && ys > 0) || (xs > 0 && ys < 0) {
			return q.Add(y)
		}
		return q
	})

	interp.Def("+", -1, func(a []any) any {
		return a[0].(*Cell).FoldL(Number0,
			func(x, y any) any {
				return goarith.AsNumber(x).Add(goarith.AsNumber(y))
			})
	})

	interp.Def("*", -1, func(a []any) any {
		return a[0].(*Cell).FoldL(Number1,
			func(x, y any) any {
				return goarith.AsNumber(x).Mul(goarith.AsNumber(y))
			})
	})

	interp.Def("-", -2, func(a []any) any {
		if a[1] == Nil {
			return Number0.Sub(goarith.AsNumber(a[0]))
		} else {
			return a[1].(*Cell).FoldL(goarith.AsNumber(a[0]),
				func(x, y any) any {
					return goarith.AsNumber(x).Sub(goarith.AsNumber(y))
				})
		}
	})

	interp.Def("/", -3, func(a []any) any {
		q := goarith.AsNumber(a[0]).RQuo(goarith.AsNumber(a[1]))
		return a[2].(*Cell).FoldL(q,
			func(x, y any) any {
				return goarith.AsNumber(x).RQuo(goarith.AsNumber(y))
			})
	})

	interp.Def("truncate", -2, func(a []any) any {
		x, y := goarith.AsNumber(a[0]), a[1].(*Cell)
		if y == Nil {
			q, _ := x.QuoRem(Number1)
			return q
		} else if y.Cdr == Nil {
			q, _ := x.QuoRem(goarith.AsNumber(y.Car))
			return q
		} else {
			panic("one or two arguments expected")
		}
	})

	interp.Def("div", 2, func(a []any) any {
		n := interp.ExpectInts("div", a, 0, 2)
		return goarith.AsNumber(n[0] / n[1])
	})

	interp.Def("int", 1, func(a []any) any {
		n, _ := goarith.AsNumber(a[0]).Int()
		return goarith.AsNumber(n)
	})

	interp.Def("float", 1, func(a []any) any {
		return goarith.AsNumber(ToFloat64(a[0]))
	})

	gensymCounterSym := NewSym("*gensym-counter*")
	interp.SetGlobalVar(gensymCounterSym, Number1)
	interp.Def("gensym", 0, func(a []any) any {
		v, _ := interp.GetGlobalVar(gensymCounterSym)
		x := goarith.AsNumber(v)
		interp.SetGlobalVar(gensymCounterSym, x.Add(Number1))
		return &Sym{fmt.Sprintf("G%s", x.String()), false}
	})

	interp.Def("make-symbol", 1, func(a []any) any {
		return &Sym{a[0].(string), false}
	})

	interp.Def("intern", 1, func(a []any) any {
		return NewSym(a[0].(string))
	})

	interp.Def("apply", 2, func(a []any) any {
		args := a[1].(*Cell).MapCar(QqQuote)
		return interp.Eval(&Cell{a[0], args}, Nil)
	})

	interp.Def("exit", -1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		var n int
		var exact bool
		if len(arr) > 0 {
			n, exact = goarith.AsNumber(arr[0]).Int()
			if !exact {
				panic("int expected")
			}
		} else {
			n = 0
		}
		interp.pc.RequestShutdown(n)
		return Void // reached but main PC loop will shut down ASAP
	})

	interp.Def("dump-bindings", 0, func(a []any) any {
		r := Nil
		interp.globals.Range(func(k, v any) bool {
			r = &Cell{k, r}
			return true
		})
		return r
	})

	// (intrinsic 'sym) => builin-function returns the builtin function bound to sym
	// This does currently no attempt to circumvent redefinitions, but may later do it.
	interp.Def("intrinsic", 1, func(a []any) any {
		return interp.Eval(a[0], Nil)
	})

	// (bitxor n m) returns the bitwise xor of integers n, m.
	interp.Def("bitxor", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n ^ m
	})

	// (bitand n m) returns the bitwise and of integers n, m.
	interp.Def("bitand", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n & m
	})

	// (bitor n m) returns the bitwise or of integers n, m.
	interp.Def("bitor", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n | m
	})

	// (bitclear n m) returns the bitwise and-not of integers n, m.
	interp.Def("bitclear", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n &^ m
	})

	// (bitshl n m) returns the bitwise left shift of n by m.
	interp.Def("bitshl", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n << m
	})

	// (bitshr n m) returns the bitwise right shift of n by m.
	interp.Def("bitshr", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`int expected`)
		}
		m, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(`int expected`)
		}
		return n >> m
	})

	// (force future) Wait until the "promise" of Future is delivered.
	interp.Def("force", 1, func(a []any) any {
		if fu, ok := a[0].(*Future); ok {
			fu.Lock.Lock()
			defer fu.Lock.Unlock()
			if fu.Chan != nil {
				fu.Result = <-fu.Chan
				fu.Chan = nil
			}
			if err := fu.Result.Cdr; err != nil {
				fu.Result.Cdr = nil
				panic(err) // Transmit the error.
			}
			return fu.Result.Car
		} else {
			return a[0]
		}
	})

	// (make-mutex) creates a new mutex
	interp.Def("make-mutex", 0, func(a []any) any {
		m := sync.RWMutex{}
		return &m
	})

	interp.Def("mutex?", 1, func(a []any) any {
		_, ok := a[0].(*sync.RWMutex)
		return AsLispBool(ok)
	})

	// (mutex-lock m) lock the mutex
	interp.Def("mutex-lock", 1, func(a []any) any {
		m := a[0].(*sync.RWMutex)
		m.Lock()
		return Void
	})

	// (mutex-unlock m) unlock the mutex
	interp.Def("mutex-unlock", 1, func(a []any) any {
		m := a[0].(*sync.RWMutex)
		m.Unlock()
		return Void
	})

	// (mutex-rlock m) read lock the mutex
	interp.Def("mutex-rlock", 1, func(a []any) any {
		m := a[0].(*sync.RWMutex)
		m.RLock()
		return Void
	})

	// (mutex-unlock m) read unlock the mutex
	interp.Def("mutex-runlock", 1, func(a []any) any {
		m := a[0].(*sync.RWMutex)
		m.RUnlock()
		return Void
	})

	// (cinc! var) => int add1 to the variable and return its value
	interp.Def("cinc!", -2, func(a []any) any {
		interp.lock.Lock()
		defer interp.lock.Unlock()
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(`cinc! can only be applied to a variable`)
		}
		// Todo: needs synchronization! (race condition)
		n, ok := interp.GetGlobalVar(sym)
		if !ok {
			panic(`cinc!: the variable needs to be bound to an integer value`)
		}
		m := goarith.AsNumber(n).Add(Number1)
		interp.SetGlobalVar(sym, m)
		return m
	})

	// (cdec! var) => int sub1 to the variable and return its value
	interp.Def("cdec!", -2, func(a []any) any {
		interp.lock.Lock()
		defer interp.lock.Unlock()
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(`cdec! can only be applied to a global variable`)
		}
		// Todo: needs synchronization! (race condition)
		n, ok := interp.GetGlobalVar(sym)
		if !ok {
			panic(`cinc!: the variable needs to be bound to an integer value`)
		}
		m := goarith.AsNumber(n).Sub(Number1)
		interp.SetGlobalVar(sym, m)
		return m
	})

	// (cwait var value timeout) wait until counter var has value or timeout has passed; 0 timeout = indefinitely long
	interp.Def("cwait", 3, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(`cwait: first argument must be a global variable`)
		}
		timeout, ok := goarith.AsNumber(a[2]).Int()
		if !ok {
			panic(fmt.Sprintf("cwait: timeout value must be an exact integer, given %v", Str(a[2])))
		}
		t0 := time.Now()
		duration := time.Duration(timeout) * time.Millisecond
		hasTimeout := duration > 0
		for {
			v, ok := interp.globals.Load(sym)
			if !ok {
				break
			}
			n, ok := v.(ValueHolder)
			if ok {
				if goarith.AsNumber(n).Cmp(goarith.AsNumber(a[1])) == 0 {
					break
				}
			}
			if hasTimeout && time.Since(t0) > duration {
				break
			}
			time.Sleep(1 * time.Millisecond)
		}
		return Void
	})

	// (cmp var value) compare contents of variable to int value, return -1 if var<value, 0 if var=value, 1 if var>value
	interp.Def("ccmp", -2, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(`ccmp: the first argument must be a global variable`)
		}
		m, ok := interp.GetGlobalVar(sym)
		if !ok {
			panic(`ccmp: the variable needs to be bound to an integer value`)
		}
		return goarith.AsNumber(m).Cmp(a[1].(*Cell).Car.(goarith.Number))
	})

	// (cst! var value) set var to int value
	interp.Def("cst!", 2, func(a []any) any {
		sym, ok := a[0].(*Sym)
		if !ok {
			panic(`cst!: the first argument must be a global variable`)
		}
		_, ok = goarith.AsNumber(a[1]).Int()
		if !ok {
			panic(fmt.Sprintf("cst!: the second argument must be an integer value, given %v", Str(a[1])))
		}
		interp.SetGlobalVar(sym, a[1])
		return Void
	})

	// (collect-garbage) collects garbage
	interp.Def("collect-garbage", -1, func(a []any) any {
		if len(a) == 0 || a[0] == nil || a[0] == Nil {
			runtime.GC()
		}
		s := "normal"
		li := a[0].(*Cell)
		if li != nil {
			sym := li.Car.(*Sym)
			s = sym.String()
		}
		switch s {
		case "normal":
			runtime.GC()
		case "total":
			debug.FreeOSMemory()
		default:
			panic(fmt.Sprintf("collect-garbage: expected symbol in '(normal total), given '%v", s))
		}
		return Void
	})

	// (memstats) => list return a list of memory statistics
	interp.Def("memstats", 0, func(a []any) any {
		var m runtime.MemStats
		runtime.ReadMemStats(&m)
		r := sync.Map{}
		r.Store("alloc", goarith.AsNumber(int64(m.Alloc)))
		r.Store("total-alloc", goarith.AsNumber(int64(m.TotalAlloc)))
		r.Store("sys", goarith.AsNumber(int64(m.Sys)))
		r.Store("lookups", goarith.AsNumber(int64(m.Lookups)))
		r.Store("mallocs", goarith.AsNumber(int64(m.Mallocs)))
		r.Store("frees", goarith.AsNumber(int64(m.Frees)))
		r.Store("heap-alloc", goarith.AsNumber(int64(m.HeapAlloc)))
		r.Store("heap-sys", goarith.AsNumber(int64(m.HeapSys)))
		r.Store("heap-idle", goarith.AsNumber(int64(m.HeapIdle)))
		r.Store("heap-in-use", goarith.AsNumber(int64(m.HeapInuse)))
		r.Store("heap-released", goarith.AsNumber(int64(m.HeapReleased)))
		r.Store("heap-objects", goarith.AsNumber(int64(m.HeapObjects)))
		r.Store("stack-in-use", goarith.AsNumber(int64(m.StackInuse)))
		r.Store("stack-sys", goarith.AsNumber(int64(m.StackSys)))
		r.Store("mspan-in-use", goarith.AsNumber(int64(m.MSpanInuse)))
		r.Store("mspan-sys", goarith.AsNumber(int64(m.MSpanSys)))
		r.Store("mcache-in-use", goarith.AsNumber(int64(m.MCacheInuse)))
		r.Store("mcache-sys", goarith.AsNumber(int64(m.MCacheSys)))
		r.Store("buckhash-sys", goarith.AsNumber(int64(m.BuckHashSys)))
		r.Store("gc-sys", goarith.AsNumber(int64(m.GCSys)))
		r.Store("other-sys", goarith.AsNumber(int64(m.OtherSys)))
		r.Store("next-gc", goarith.AsNumber(int64(m.NextGC)))
		r.Store("last-gc", goarith.AsNumber(int64(m.LastGC)))
		r.Store("pause-total-ns", goarith.AsNumber(int64(m.PauseTotalNs)))
		r.Store("num-gc", goarith.AsNumber(int64(m.NumGC)))
		r.Store("num-forced-gc", goarith.AsNumber(int64(m.NumForcedGC)))
		r.Store("gc-cpu-fraction", goarith.AsNumber(m.GCCPUFraction))
		r.Store("enable-gc", AsLispBool(m.EnableGC))
		r.Store("debug-gc", AsLispBool(m.DebugGC))
		return &Dict{Data: &r, Protected: false}
	})

	// (rnd prng) => random number in [0 1]
	interp.Def("rnd", 1, func(a []any) any {
		interp.lock.Lock()
		defer interp.lock.Unlock()
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact || (n < 0) || (n > 9) {
			panic(fmt.Sprintf("rnd: expected pseudo-random number generator 0 to 9 as argument, given %v", Str(a[0])))
		}
		return goarith.AsNumber(interp.prng[n].Float64())
	})

	// (rand prng lower upper) => random int in [lower upper]
	interp.Def("rand", 3, func(a []any) any {
		interp.lock.Lock()
		defer interp.lock.Unlock()
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact || (n < 0) || (n > 9) {
			panic(fmt.Sprintf("rand: expected pseudo-random number generator 0 to 9 as first argument, given %v", Str(a[0])))
		}
		ints := interp.ExpectInts("rand", a, 1, 2)
		return goarith.AsNumber(interp.prng[n].Intn(ints[1]+1-ints[0]) + ints[0])
	})

	// (rndseed prng n) seed the random number generator
	interp.Def("rndseed", 2, func(a []any) any {
		interp.lock.Lock()
		defer interp.lock.Unlock()
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact || (n < 0) || (n > 9) {
			panic(fmt.Sprintf("rndseed: expected pseudo-random number generator 0 to 9 as first argument, given %v",
				Str(a[0])))
		}
		interp.prng[n].Seed(ToInt64("rndseed", a[1]))
		return Void
	})

	// (char->str ch) => string convert utf rune int into string
	interp.Def("char->str", 1, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic("char->str: expected int that represents a unicode rune")
		}
		return string(rune(n))
	})

	// (error msgstr expr ...) report an error, where expr... are used as arguments to msgstr like int fmt.
	interp.Def("error", -1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		if len(arr) == 0 {
			panic("unknown error")
		}
		if len(arr) == 1 {
			panic(arr[0].(string))
		}
		s := make([]any, len(arr)-1)
		for i := range s {
			switch arr[i+1].(type) {
			case goarith.Number:
				s[i] = goarith.AsNumber(arr[i+1])
			default:
				s[i] = Str2(arr[i+1], false)
			}
		}
		panic(fmt.Sprintf(arr[0].(string), s...))
	})

	// (now) returns the now datetime in UTC as list.
	interp.Def("now", 0, func(a []any) any {
		now := time.Now().UTC()
		return ConvertTimeToDateList(now)
	})

	// (epoch-ns->datelist ns) => li converts from Unix epoch nanoseconds to UTC time as list
	interp.Def("epoch-ns->datelist", 1, func(a []any) any {
		n := ToInt64("epoch-ns->datelist", a[0])
		d := time.Unix(0, n).UTC()
		return ConvertTimeToDateList(d)
	})

	// (now-ns) returns the nanoseconds in Unix time format
	interp.Def("now-ns", 0, func(a []any) any {
		return goarith.AsNumber(time.Now().UnixNano())
	})

	// (time proc) returns the time in nanoseconds executing proc takes
	interp.Def("time", 1, func(a []any) any {
		proc := a[0].(*Closure)
		t0 := time.Now()
		interp.Eval(&Cell{proc, Nil}, Nil)
		t1 := time.Now()
		return goarith.AsNumber(t1.Sub(t0).Nanoseconds())
	})

	// (date->epoch-ns y m d h m s n) => int compute the Unix epoch nanoseconds for given date
	interp.Def("date->epoch-ns", 7, func(a []any) any {
		n := interp.ExpectInts("date->epoch-ns", a, 0, 7)
		d := time.Date(n[0], time.Month(n[1]), n[2], n[3], n[4], n[5], n[6], time.UTC)
		return goarith.AsNumber(d.UnixNano())
	})

	// (day-of-week y m d) => int compute the weekday given the date
	interp.Def("day-of-week", 3, func(a []any) any {
		n := interp.ExpectInts("day-of-week", a, 0, 3)
		d := time.Date(n[0], time.Month(n[1]), n[2], 12, 0, 0, 0, time.UTC)
		return goarith.AsNumber(int(d.Weekday()))
	})

	// (week-of-date y m d) => int compute the iso week of the given date
	interp.Def("week-of-date", 3, func(a []any) any {
		n := interp.ExpectInts("day-of-week", a, 0, 3)
		d := time.Date(n[0], time.Month(n[1]), n[2], 12, 0, 0, 0, time.UTC)
		_, w := d.ISOWeek()
		return goarith.AsNumber(w)
	})

	// (valid? obj) => bool return true if the boxed object is valid, nil otherwise. If the object is not boxed,
	// then true is returned.
	interp.Def("valid?", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			return true
		}
		return AsLispBool(obj.Valid)
	})

	// (enq proc) calls proc in a thread-safe sequential queue
	interp.Def("enq", 1, func(a []any) any {
		interp.pc.Enqueue(func() {
			_, err := interp.SafeEval(&Cell{a[0].(*Closure), Nil}, Nil)
			if err != nil {
				panic(err)
			}
		})
		return Void
	})

	// (void ...) does nothing and returns Void.
	interp.Def("void", -1, func(a []any) any {
		return Void
	})

	// (void? datum) => bool return true if the datum is void, nil otherwise.
	interp.Def("void?", 1, func(a []any) any {
		s, ok := a[0].(*Sym)
		if !ok {
			return Nil
		}
		if s == Void {
			return true
		}
		return Nil
	})

	// (fmt s args ...) => str formats the string according to given arguments.
	interp.Def("fmt", -1, func(a []any) any {
		if a[0] == Nil {
			return ""
		}
		arr := ListToArray(a[0].(*Cell))
		if len(arr) == 1 {
			return arr[0]
		}
		s := make([]any, len(arr)-1)
		for i := range s {
			switch arr[i+1].(type) {
			case goarith.Number:
				s[i] = goarith.AsNumber(arr[i+1])
			default:
				s[i] = Str2(arr[i+1], false)
			}
		}
		return fmt.Sprintf(arr[0].(string), s...)
	})

	// (_external datum) => str return an external representation of datum if possible
	interp.Def("_external", 1, func(a []any) any {
		return interp.Externalize(nil, a[0])
	})

	// (error? datum) => bool return true if the datum is an error, nil otherwise
	interp.Def("error?", 1, func(a []any) any {
		_, ok := a[0].(error)
		if ok {
			return true
		}
		return Nil
	})

	// (error->str datum) => str convert a special error value to a string
	interp.Def("error->str", 1, func(a []any) any {
		return Str(a[0])
	})

	// (_external? datum) => bool return true if an external representation of datum is possible
	interp.Def("_external?", 1, func(a []any) any {
		return AsLispBool(interp.CanExternalize(a[0]))
	})

	// (str+ s ...) => str appends all the given strings.
	interp.Def("str+", -1, func(a []any) any {
		if a[0] == Nil {
			return ""
		}
		arr := ListToArray(a[0].(*Cell))
		s := ""
		for _, q := range arr {
			s += q.(string)
		}
		return s
	})

	// (strbuild s n) => str builds a string repeating s n times.
	interp.Def("strbuild", 2, func(a []any) any {
		s := a[0].(string)
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("strbuild: expected an exact int")
		}
		r := ""
		for i := 0; i < n; i++ {
			r += s
		}
		return r
	})

	// (strsplit s del) => array of string split by delimiter del.
	interp.Def("strsplit", 2, func(a []any) any {
		arr := strings.Split(a[0].(string), a[1].(string))
		arr2 := make([]any, len(arr))
		for i := range arr {
			arr2[i] = arr[i]
		}
		return arr2
	})

	// (strcnt s del) => n return the number of non-overlapping occurrences of del in s
	interp.Def("strcnt", 2, func(a []any) any {
		return goarith.AsNumber(strings.Count(a[0].(string), a[1].(string)))
	})

	// (array ...) => array returns its arguments as an array (Go slice).
	interp.Def("array", -1, func(a []any) any {
		return ListToArray(a[0].(*Cell))
	})

	// (array-slice arr low high) => array returns the array slice.
	interp.Def("array-slice", 3, func(a []any) any {
		n := interp.ExpectInts("slice", a, 1, 2)
		return a[0].([]any)[n[0]:n[1]]
	})

	// (array-len arr) => int returns the length of the array.
	interp.Def("array-len", 1, func(a []any) any {
		return goarith.AsNumber(len(a[0].([]any)))
	})

	// (array-append arr v) => array
	interp.Def("array-append", 2, func(a []any) any {
		arr := a[0].([]any)
		arr = append(arr, a[1])
		return arr
	})

	// (array+ array1 ...) => array
	interp.Def("array+", -1, func(a []any) any {
		li := a[0].(*Cell)
		tmp := li.Car.([]any)
		arr := make([]any, len(tmp))
		copy(arr, tmp)
		li = li.CdrCell()
		for li != Nil {
			arr = append(arr, li.Car.([]any)...)
			li = li.CdrCell()
		}
		return arr
	})

	// (build-array n init-value) => array
	interp.Def("build-array", 2, func(a []any) any {
		n, ok := goarith.AsNumber(a[0]).Int()
		if !ok {
			panic(fmt.Sprintf("build-array: expected an exact integer as length, given %v", Str(a[1])))
		}
		arr := make([]any, n)
		for i := range arr {
			arr[i] = a[1]
		}
		return arr
	})

	// (arrayp arr) ==> bool returns true if arr is an array, false otherwise.
	interp.Def("array?", 1, func(a []any) any {
		if _, ok := a[0].([]any); ok {
			return true
		}
		return Nil
	})

	// (array-ref arr n) ==> any returns the nth element of array (0-indexed).
	interp.Def("array-ref", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("array-ref: expected valid int index")
		}
		return a[0].([]any)[n]
	})

	// (array-set arr n value) set the nth element of array (0-indexed) to value.
	interp.Def("array-set", 3, func(a []any) any {
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("array-set: expected valid int index")
		}
		a[0].([]any)[n] = a[2]
		return Void
	})

	// (array-map! arr proc)
	interp.Def("array-map!", 2, func(a []any) any {
		arr := a[0].([]any)
		proc := a[1].(*Closure)
		li2 := &Cell{Nil, Nil}
		li := &Cell{proc, li2}
		for i := range arr {
			li2.Car = arr[i]
			arr[i] = interp.Eval(li, Nil)
		}
		return Void
	})

	// (array-pmap! arr proc)
	interp.Def("array-pmap!", 2, func(a []any) any {
		arr := a[0].([]any)
		proc := a[1].(*Closure)
		var wg sync.WaitGroup
		for i := range arr {
			wg.Add(1)
			go func(i int, v any) {
				defer wg.Done()
				arr[i] = interp.Eval(&Cell{proc, &Cell{v, Nil}}, Nil)
			}(i, arr[i])
		}
		wg.Wait()
		return Void
	})

	// (array-walk arr proc)
	interp.Def("array-walk", 2, func(a []any) any {
		arr := a[0].([]any)
		proc := a[1].(*Closure)
		result := -1
		for i := range arr {
			li := &Cell{goarith.AsNumber(i), &Cell{arr[i], Nil}}
			qq := li.MapCar(QqQuote)
			x := interp.Eval(&Cell{proc, qq}, Nil)
			if c, ok := x.(*Cell); ok && c == Nil {
				result = i
				break
			}
		}
		return goarith.AsNumber(result)
	})

	// (array->list arr) returns the array as list.
	interp.Def("array->list", 1, func(a []any) any {
		return ArrayToList(a[0].([]any))
	})

	// (array-copy arr) => array returns a shallow copy of the array.
	interp.Def("array-copy", 1, func(a []any) any {
		arr := a[0].([]any)
		tmp := make([]any, len(arr))
		copy(tmp, arr)
		return tmp
	})

	// (array-reverse arr) => array reverse the array, return a shallow copy.
	interp.Def("array-reverse", 1, func(a []any) any {
		return ReverseArray(a[0].([]any))
	})

	// (list-reverse li) => create a reverse copy of li.
	interp.Def("list-reverse", 1, func(a []any) any {
		return a[0].(*Cell).Reverse()
	})

	// (str-reverse s) => string return the reverse string.
	interp.Def("str-reverse", 1, func(a []any) any {
		return ReverseStr(a[0].(string))
	})

	// (instr s1 s2) => int index of substring s2 in s1, or -1
	interp.Def("instr", 2, func(a []any) any {
		return goarith.AsNumber(strings.Index(a[0].(string), a[1].(string)))
	})

	// (dict [li]) => map returns a new dict based on '(k1 v1 k2 v2 ...) as init-expr.
	interp.Def("dict", -1, func(a []any) any {
		var m Dict = Dict{Data: &sync.Map{}, Protected: false}
		if li, ok := a[0].(*Cell); !ok || li == Nil {
			return &m
		}
		li := a[0].(*Cell).Car.(*Cell)
		data := ListToArray(li)
		ll := len(data)
		if ll == 0 {
			return &m
		}
		if ll%2 != 0 {
			panic(fmt.Sprintf(`dict: given list must have an even number of items, given: %v`, Str(data)))
		}
		for i := 0; i < ll-1; i += 2 {
			m.Data.Store(data[i], data[i+1])
		}
		return &m
	})

	// (dict? any) => bool
	interp.Def("dict?", 1, func(a []any) any {
		_, ok := a[0].(*Dict)
		if ok {
			return true
		}
		return Nil
	})

	// (_get dict key default) => any returns the value stored in dict for key, default otherwise.
	interp.Def("_get", 3, func(a []any) any {
		m := a[0].(*Dict)
		if datum, ok := m.Data.Load(a[1]); ok {
			return datum
		}
		return a[2]
	})

	// (set dict key value) set the value in dict.
	interp.Def("set", 3, func(a []any) any {
		m := a[0].(*Dict)
		if m.Protected {
			panic(fmt.Sprintf("set: attempt to set value for key '%v in protected dict %v!", Str(a[1]), Str(a[0])))
		}
		m.Data.Store(a[1], a[2])
		return Void
	})

	// (set* dict li) set the values in dict from li, which must contain key1 value1 key2 value2 ... keyn valuen.
	interp.Def("set*", 2, func(a []any) any {
		m := a[0].(*Dict)
		if m.Protected {
			panic(fmt.Sprintf("set*: attempt to set values in protected dict %v!", Str(a[0])))
		}
		li := a[1].(*Cell)
		var key any
		for li != Nil {
			if key == nil {
				key = li.Car
			} else {
				m.Data.Store(key, li.Car)
				key = nil
			}
			li = li.CdrCell()
		}
		if key != nil {
			panic(fmt.Sprintf("set*: malformed key value list, given %v", Str(a[1])))
		}
		return Void
	})

	// (get-or-set dict key value) => any sets the value if there is none yet, otherwise gets it
	interp.Def("get-or-set", 3, func(a []any) any {
		m := a[0].(*Dict)
		if m.Protected {
			panic(fmt.Sprintf("get-or-set: attempt to set value for key '%v in protected dict %v!", Str(a[1]), Str(a[0])))
		}
		result, loaded := m.Data.LoadOrStore(a[1], a[2])
		return &Cell{AsLispBool(loaded), &Cell{result, Nil}}
	})

	// (delete dict key) remove the value in dict (if it exists).
	interp.Def("delete", 2, func(a []any) any {
		m := a[0].(*Dict)
		if m.Protected {
			panic(fmt.Sprintf("delete: attempt to delete key '%v from protected dict %v!", Str(a[1]), Str(a[0])))
		}
		m.Data.Delete(a[1])
		return Void
	})

	// (has-key? dict key) => bool
	interp.Def("has-key?", 2, func(a []any) any {
		m := a[0].(*Dict)
		if _, ok := m.Data.Load(a[1]); ok {
			return true
		}
		return Nil
	})

	// (dict-protect dict) protect dict against changes
	interp.Def("dict-protect", 1, func(a []any) any {
		m := a[0].(*Dict)
		if !interp.pc.Perm().AllowProtect {
			panic("dict-protect: security violation - permission 'allow-protect not available")
		}
		m.Protected = true
		return Void
	})

	// (dict-unprotect dict) unprotect dict against changes
	interp.Def("dict-unprotect", 1, func(a []any) any {
		m := a[0].(*Dict)
		if !interp.pc.Perm().AllowUnprotect {
			panic("dict-unprotect: security violation - permission 'allow-unprotect not available")
		}
		m.Protected = false
		return Void
	})

	// (dict-protected? dict) => bool
	interp.Def("dict-protected?", 1, func(a []any) any {
		return AsLispBool(a[0].(*Dict).Protected)
	})

	// (dict-copy dict) => dict
	interp.Def("dict-copy", 1, func(a []any) any {
		m1, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-copy: not a dictionary: %v", Str(a[0])))
		}
		return CopyDict(m1)
	})

	// (dict-map! dict proc)
	interp.Def("dict-map!", 2, func(a []any) any {
		m, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-map!: not a dictionary: %v", Str(a[0])))
		}
		if m.Protected {
			panic(fmt.Sprintf("dict-map!: attempt to mutate protected dict %v!", Str(a[0])))
		}
		m.Data.Range(func(k, v any) bool {
			args := &Cell{k, &Cell{v, Nil}}
			qq := args.MapCar(QqQuote)
			result := interp.Eval(&Cell{a[1], qq}, Nil)
			m.Data.Store(k, result)
			return true
		})
		return Void
	})

	// (dict-foreach dict proc)
	interp.Def("dict-foreach", 2, func(a []any) any {
		m, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-foreach: not a dictionary: %v", Str(a[0])))
		}
		m.Data.Range(func(k, v any) bool {
			args := &Cell{k, &Cell{v, Nil}}
			qq := args.MapCar(QqQuote)
			interp.Eval(&Cell{a[1], qq}, Nil)
			return true
		})
		return Void
	})

	// (dict-empty? d) => bool true if d is empty, nil otherwise
	interp.Def("dict-empty?", 1, func(a []any) any {
		m, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-empty?: not a dictionary: %v", Str(a[0])))
		}
		found := false
		m.Data.Range(func(k, v any) bool {
			found = true
			return false
		})
		return AsLispBool(!found)
	})

	// (dict-key-subset? d1 d2) => bool return true if d1's key are a subset of d2's keys, nil otherwise
	interp.Def("dict-key-subset?", 2, func(a []any) any {
		m1, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-key-subset?: not a dictionary: %v", Str(a[0])))
		}
		m2, ok := a[1].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-key-subset?: not a dictionary: %v", Str(a[1])))
		}
		success := true
		m1.Data.Range(func(k, v any) bool {
			if _, ok := m2.Data.Load(k); ok {
				return true
			} else {
				success = false
				return false
			}
		})
		return AsLispBool(success)
	})

	// (dict-key-equal? d1 d2) => bool return true if d1 and d2 have the same keys, nil otherwise
	interp.Def("dict-key-equal?", 2, func(a []any) any {
		m1, ok := a[0].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-key-subset?: not a dictionary: %v", Str(a[0])))
		}
		m2, ok := a[1].(*Dict)
		if !ok {
			panic(fmt.Sprintf("dict-key-subset?: not a dictionary: %v", Str(a[1])))
		}
		success := true
		m1.Data.Range(func(k, v any) bool {
			if _, ok := m2.Data.Load(k); ok {
				return true
			} else {
				success = false
				return false
			}
		})
		if !success {
			return Nil
		}
		m2.Data.Range(func(k, v any) bool {
			if _, ok := m1.Data.Load(k); ok {
				return true
			} else {
				success = false
				return false
			}
		})
		return AsLispBool(success)
	})

	// (dict->array dict) return the array for dict.
	interp.Def("dict->array", 1, func(a []any) any {
		return DictToArray(a[0])
	})

	// (dict->list dict) => list returns the dict as list of consecutive pairs of key value.
	interp.Def("dict->list", 1, func(a []any) any {
		return ArrayToList(DictToArray(a[0]))
	})

	// (strlen s) returns the length of the string.
	interp.Def("strlen", 1, func(a []any) any {
		return goarith.AsNumber(len(a[0].(string)))
	})

	// (str-empty? s) => bool return true if the string is empty, nil otherwise
	interp.Def("str-empty?", 1, func(a []any) any {
		return AsLispBool(a[0].(string) == "")
	})

	// (str-replace s t1 t2 n) => str replace the first n occurrences of t1 in s by t2
	interp.Def("str-replace", 4, func(a []any) any {
		n, exact := goarith.AsNumber(a[3]).Int()
		if !exact {
			panic(fmt.Sprintf("str-replace: expected exact integer for number of replacements, given %v", Str(a[3])))
		}
		return strings.Replace(a[0].(string), a[1].(string), a[2].(string), n)
	})

	// (str-replace* s t1 t2) => str replace all occurrences of t1 in s by t2
	interp.Def("str-replace*", 3, func(a []any) any {
		return strings.ReplaceAll(a[0].(string), a[1].(string), a[2].(string))
	})

	// (str-count-substr s t1) => n return the number of non-overlapping substrings t1 in s
	interp.Def("str-count-substr", 2, func(a []any) any {
		return goarith.AsNumber(strings.Count(a[0].(string), a[1].(string)))
	})

	// (str-ref s n) => int returns the n-th char in string (0-indexed).
	interp.Def("str-ref", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("str-ref: expected valid int index")
		}
		return []rune(a[0].(string))[n]
	})

	// (sym->str sym) => str return the string representing the symbol.
	interp.Def("sym->str", 1, func(a []any) any {
		return a[0].(*Sym).String()
	})

	// TODO cross check with make-symbol and intern!
	// (str->sym s) => sym returns a symbol for given string.
	interp.Def("str->sym", 1, func(a []any) any {
		return NewSym(strings.Join(strings.Fields(a[0].(string)), "-"))
	})

	// (strmap s proc) => string map the string.
	interp.Def("strmap", 2, func(a []any) any {
		r := []rune(a[0].(string))
		for i := range r {
			var char any = goarith.AsNumber(r[i])
			result, exact := goarith.AsNumber(interp.Eval(&Cell{a[1], &Cell{char, Nil}}, Nil)).Int()
			if !exact {
				panic("strmap: procedure returned an inexact number instead of a valid unicode rune int")
			}
			r[i] = rune(result)
		}
		return string(r)
	})

	// (strcase s sym) => string
	interp.Def("strcase", 2, func(a []any) any {
		s := a[0].(string)
		sym := a[1].(*Sym)
		switch sym.Name {
		case "lower":
			return strings.ToLower(s)
		case "upper":
			return strings.ToUpper(s)
		case "title":
			return strings.ToTitle(s)
		case "utf-8":
			return strings.ToValidUTF8(s, "?")
		default:
			panic(fmt.Sprintf(`strcase: unknown selector '%v, expected symbol in '(lower upper title utf-8)`, Str(sym)))
		}
	})

	// (nonce) => str return a unique string (not crypto secure)
	interp.Def("nonce", 0, func(a []any) any {
		c := 200
		var s string
		var err error
		for c > 0 {
			s, err = shortid.Generate()
			if err != nil {
				c--
			} else {
				break
			}
			time.Sleep(1 * time.Millisecond)
		}
		return s

	})

	// (semver.build str) => str
	interp.Def("semver.build", 1, func(a []any) any {
		return semver.Build(a[0].(string))
	})

	// (semver.canonical str) => str
	interp.Def("semver.canonical", 1, func(a []any) any {
		return semver.Canonical(a[0].(string))
	})

	// (semver.compare s1 s2) => int
	interp.Def("semver.compare", 2, func(a []any) any {
		return goarith.AsNumber(semver.Compare(a[0].(string), a[1].(string)))
	})

	// (semver.is-valid? str) => bool
	interp.Def("semver.is-valid?", 1, func(a []any) any {
		return AsLispBool(semver.IsValid(a[0].(string)))
	})

	// (semver.major str) => str
	interp.Def("semver.major", 1, func(a []any) any {
		return semver.Major(a[0].(string))
	})

	// (semver.major-minor str) => str
	interp.Def("semver.major-minor", 1, func(a []any) any {
		return semver.MajorMinor(a[0].(string))
	})

	// (semver.max s1 s2) => str
	interp.Def("semver.max", 2, func(a []any) any {
		cp := semver.Compare(a[0].(string), a[1].(string))
		if cp >= 0 {
			return a[0]
		}
		return a[1]
	})

	// (semver.prerelease str) => str
	interp.Def("semver.prerelease", 1, func(a []any) any {
		return semver.Prerelease(a[0].(string))
	})

	// (array-sort arr proc) => arr destructively sorts the array by proc.
	interp.Def("array-sort", 2, func(a []any) any {
		arr := a[0].([]any)
		sort.Slice(arr, func(i, j int) bool {
			args := &Cell{arr[i], &Cell{arr[j], Nil}}
			qq := args.MapCar(QqQuote)
			return ToBool(interp.Eval(&Cell{a[1], qq}, Nil))
		})
		return arr
	})

	// (sort li proc) => li sort the list by given less-than procedure that takes two elements.
	interp.Def("sort", 2, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		sort.Slice(arr, func(i, j int) bool {
			args := &Cell{arr[i], &Cell{arr[j], Nil}}
			qq := args.MapCar(QqQuote)
			return ToBool(interp.Eval(&Cell{a[1], qq}, Nil))
		})
		return ArrayToList(arr)
	})

	// (strless s1 s2) => bool returns true if s1<s2, false otherwise.
	interp.Def("strless", 2, func(a []any) any {
		return AsLispBool(a[0].(string) < a[1].(string))
	})

	// (list-ref li n) => any returns the n-th list element (0-indexed).
	interp.Def("list-ref", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("list-ref: expected valid int index")
		}
		if a[0] == Nil {
			panic("list-ref: list index out of range")
		}
		li := a[0].(*Cell)
		for i := 0; i < n; i++ {
			if li.Cdr == Nil {
				panic("list-ref: list index out of range")
			}
			li = li.Cdr.(*Cell)
		}
		return li.Car
	})

	// (list-slice li low high) => li returns a slice of the list from low (including) to high (excluding).
	interp.Def("list-slice", 3, func(a []any) any {
		n := interp.ExpectInts("string-slice", a, 1, 2)
		if a[0] == Nil {
			panic("list-slice: list index out of range")
		}
		arr := ListToArray(a[0].(*Cell))
		return ArrayToList(arr[n[0]:n[1]])
	})

	// (str->array s) => li turns a string into an array of int.
	interp.Def("str->array", 1, func(a []any) any {
		runes := []rune(a[0].(string))
		arr := make([]any, len(runes))
		for i := range arr {
			arr[i] = goarith.AsNumber(int64(runes[i]))
		}
		return arr
	})

	// (array->str arr) => s turns an array of int chars into a string
	interp.Def("array->str", 1, func(a []any) any {
		s := ""
		for _, char := range a[0].([]any) {
			n := ToInt64("array->str", char)
			s += string(rune(n))
		}
		return s
	})

	// (str->char s) => num return the char as number
	interp.Def("str->char", 1, func(a []any) any {
		runes := []rune(a[0].(string))
		return goarith.AsNumber(int64(runes[0]))
	})

	// (string-slice s low high) => string returns a slice of characters from low (including) to high (excluding).
	interp.Def("str-slice", 3, func(a []any) any {
		n := interp.ExpectInts("string-slice", a, 1, 2)
		return string([]rune(a[0].(string))[n[0]:n[1]])
	})

	// (expr->str expr) => str convert an expression to a string.
	interp.Def("expr->str", 1, func(a []any) any {
		return fmt.Sprintf("%v", Str2(a[0], true))
	})

	// (str->expr s) => any convert a string into a Lisp expressions. Only the first expression in s is read.
	interp.Def("str->expr", -1, func(a []any) any {
		li := a[0].(*Cell)
		s := li.Car.(string)
		reader := NewReader(strings.NewReader(s), NewInternalSource("str->expr", s))
		x, err := reader.Read()
		if err != nil {
			if li.CdrCell() != Nil {
				return li.CdrCell().Car
			} else {
				panic(err)
			}
		}
		return x
	})

	// (str->expr* s) => li convert a string into a list, where each expression in s is a list member (like read-all).
	interp.Def("str->expr*", -1, func(a []any) any {
		li := a[0].(*Cell)
		s := li.Car.(string)
		reader := NewReader(strings.NewReader(s), NewInternalSource("str->expr*", s))
		arr := make([]any, 0)
		for {
			x, err := reader.Read()
			if err != nil {
				if li.CdrCell() != Nil {
					arr = append(arr, li.CdrCell().Car)
				} else {
					panic(err)
				}
			} else {
				if x == EofToken {
					return ArrayToList(arr)
				}
				arr = append(arr, x)
			}
		}
	})

	// (str->chars s) => array of int convert a UTF-8 string into an array of runes
	interp.Def("str->chars", 1, func(a []any) any {
		runes := []rune(norm.NFC.String(a[0].(string)))
		arr := make([]any, len(runes))
		for i := range runes {
			arr[i] = goarith.AsNumber(runes[i])
		}
		return arr
	})

	// (chars->str a) => s convert an array of utf-8 runes into a UTF-8 string
	interp.Def("chars->str", 1, func(a []any) any {
		arr := a[0].([]any)
		runes := make([]rune, len(arr))
		for i := range arr {
			n, exact := goarith.AsNumber(arr[i]).Int()
			if !exact {
				panic(fmt.Sprintf("runes->str: expected array of UTF-8 int of size 32 bit, given %v", Str(arr[i])))
			}
			runes[i] = rune(int32(n))
		}
		return string(runes)
	})

	// BLOBs - boxed binary data for I/O

	// (blob? datum) => bool return true if datum is a blob, nil otherwise
	interp.Def("blob?", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		if obj.Sort != BoxedBlob {
			return Nil
		}
		return true
	})

	// (blob->str blob [start] [end]) => str
	interp.Def("blob->str", -1, func(a []any) any {
		b, n, m := interp.MustParseBlobArgs("blob->str", a)
		return string(b[n:m])
	})

	// (str->blob s) => blob
	interp.Def("str->blob", 1, func(a []any) any {
		return &Boxed{Sort: BoxedBlob, Datum: []byte(a[0].(string)), Valid: true}
	})

	// (blob->hex blob [start] [end]) => str convert blob to hex string
	interp.Def("blob->hex", -1, func(a []any) any {
		b, n, m := interp.MustParseBlobArgs("blob->hex", a)
		return hex.EncodeToString(b[n:m])
	})

	// (hex->blob s) => blob convert hex string to blob
	interp.Def("hex->blob", 1, func(a []any) any {
		s := a[0].(string)
		b, err := hex.DecodeString(s)
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedBlob, Datum: b, Valid: true}
	})

	// (blob->base64 blob [start] [end]) => str convert blob to base64 string
	interp.Def("blob->base64", -1, func(a []any) any {
		b, n, m := interp.MustParseBlobArgs("blob->base64", a)
		return base64.StdEncoding.EncodeToString(b[n:m])
	})

	// (base64->blob s) => blob convert base64 string to blob
	interp.Def("base64->blob", 1, func(a []any) any {
		s := a[0].(string)
		b, err := base64.StdEncoding.DecodeString(s)
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedBlob, Datum: b, Valid: true}
	})

	// (blob->ascii85 blob) => str convert blob to ascii85 string
	interp.Def("blob->ascii85", -1, func(a []any) any {
		b, n, m := interp.MustParseBlobArgs("blob->base64", a)
		out := bytes.NewBuffer(nil)
		enc := ascii85.NewEncoder(out)
		enc.Write(b[n:m])
		enc.Close()
		return out.String()
	})

	// (ascii85->blob s) => blob convert ascii85 string to blob
	interp.Def("ascii85->blob", 1, func(a []any) any {
		s := a[0].(string)
		dec := ascii85.NewDecoder(strings.NewReader(s))
		b, err := io.ReadAll(dec)
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedBlob, Datum: b, Valid: true}
	})

	// (blob-free blob) free the blob, invalidating it.
	interp.Def("blob-free", 1, func(a []any) any {
		blob := MustGetBoxed("blob-free", a[0], BoxedBlob)
		blob.Valid = false
		blob.Datum = nil
		return Void
	})

	// (blob-equal? b1 b2) => bool return true if b1 and b2 are valid and hold the same content, nil otherwise.
	interp.Def("blob-equal?", 2, func(a []any) any {
		blob1, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		blob2, ok := a[1].(*Boxed)
		if !ok {
			return Nil
		}
		if blob1.Sort != BoxedBlob {
			return Nil
		}
		if blob1.Sort != BoxedBlob {
			return Nil
		}
		if blob1.Valid != blob2.Valid {
			return Nil
		}
		if blob1.Datum == nil || blob2.Datum == nil {
			if blob1.Datum == nil && blob2.Datum == nil {
				return true
			}
			return Nil
		}
		if bytes.Equal(blob1.Datum.([]byte), blob2.Datum.([]byte)) {
			return true
		}
		return Nil
	})

	// (blob-chksum b [start] [end]) => blob return the checksum of b as another blob
	interp.Def("blob-chksum", -11, func(a []any) any {
		b, n, m := interp.MustParseBlobArgs("blob->base64", a)
		hasher, err := blake2b.New256(nil)
		if err != nil {
			panic(err)
		}
		if _, err := io.Copy(hasher, bytes.NewReader(b[n:m])); err != nil {
			panic(err)
		}
		hash := hasher.Sum(nil)
		return &Boxed{Sort: BoxedBlob, Datum: hash[:], Valid: true}
	})

	// (make-blob n) => blob create a blob of size n, with zeroed out content
	interp.Def("make-blob", 1, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(fmt.Sprintf("make-blob: expected integer for size of blob, given %v", n))
		}
		b := make([]byte, n)
		return &Boxed{Sort: BoxedBlob, Datum: b, Valid: true}
	})

	// (poke b pos end sel val) => put val into blob b at position pos with endianness end and symbolic type sel (bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64)
	interp.Def("poke", 5, func(a []any) any {
		blob := MustGetBoxed("poke", a[0], BoxedBlob)
		if !blob.Valid {
			panic("poke: the blob is invalid!")
		}
		b, ok := blob.Datum.([]byte)
		if !ok || b == nil {
			panic("poke: the blob is empty!")
		}
		pos, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Sprintf("poke: expected an integer as position, given %v", Str(a[1])))
		}
		var endianness binary.ByteOrder
		switch Str(a[2]) {
		case "little":
			endianness = binary.LittleEndian
		case "big":
			endianness = binary.BigEndian
		default:
			panic(fmt.Sprintf("poke: unknown byte order '%v, must be one of '(little big)", Str(a[2])))
		}
		x, _ := goarith.AsNumber(a[4]).Int()
		switch Str(a[3]) {
		case "bool":
			if ToBool(a[4]) {
				b[pos] = 1
			} else {
				b[pos] = 0
			}
		case "int8":
			b[pos] = byte(x)
		case "uint8":
			b[pos] = uint8(x)
		case "int16":
			endianness.PutUint16(b[pos:], uint16(x))
		case "uint16":
			endianness.PutUint16(b[pos:], uint16(x))
		case "int32":
			endianness.PutUint32(b[pos:], uint32(x))
		case "uint32":
			endianness.PutUint32(b[pos:], uint32(x))
		case "int64":
			endianness.PutUint64(b[pos:], uint64(x))
		case "uint64":
			endianness.PutUint64(b[pos:], uint64(x))
		case "float32":
			fl, ok := a[4].(float32)
			if !ok {
				panic(fmt.Sprintf("poke: unable to convert value to float32, given %v", Str(a[4])))
			}
			endianness.PutUint32(b[pos:], math.Float32bits(fl))
		case "float64":
			fl, ok := a[4].(float64)
			if !ok {
				panic(fmt.Sprintf("poke: unable to convert value to float64, given %v", Str(a[4])))
			}
			endianness.PutUint64(b[pos:], math.Float64bits(fl))
		default:
			panic(fmt.Sprintf("poke: unsupported selector, given '%v, should be one in '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64", Str(a[3])))
		}
		return Void
	})

	// (peek b pos end sel) => value obtain value from blob b at pos of type sel and endianness end
	interp.Def("peek", 4, func(a []any) any {
		blob := MustGetBoxed("poke", a[0], BoxedBlob)
		if !blob.Valid {
			panic("peek: the blob is invalid!")
		}
		b, ok := blob.Datum.([]byte)
		if !ok || b == nil {
			panic("peek: the blob is empty!")
		}
		pos, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Sprintf("peek: expected an integer as position, given %v", pos))
		}
		var endianness binary.ByteOrder
		switch Str(a[2]) {
		case "little":
			endianness = binary.LittleEndian
		case "big":
			endianness = binary.BigEndian
		default:
			panic(fmt.Sprintf("peek: unknown byte order '%v, must be one of '(little big)", Str(a[2])))
		}
		var n any
		switch Str(a[3]) {
		case "bool":
			if b[pos] == 1 {
				n = true
			} else {
				n = false
			}
		case "int8":
			n, _ = goarith.AsNumber(int8(b[pos])).Int()

		case "uint8":
			n, _ = goarith.AsNumber(uint8(b[pos])).Int()
		case "int16":
			n, _ = goarith.AsNumber(int16(endianness.Uint16(b[pos:]))).Int()
		case "uint16":
			n, _ = goarith.AsNumber(endianness.Uint16(b[pos:])).Int()
		case "int32":
			n, _ = goarith.AsNumber(int32(endianness.Uint32(b[pos:]))).Int()
		case "uint32":
			n, _ = goarith.AsNumber(endianness.Uint32(b[pos:])).Int()
		case "int64":
			n, _ = goarith.AsNumber(int64(endianness.Uint64(b[pos:]))).Int()
		case "uint64":
			n, _ = goarith.AsNumber(endianness.Uint64(b[pos:])).Int()
		case "float32":
			k := endianness.Uint32(b[pos:])
			n = goarith.AsNumber(float64(math.Float32frombits(k)))
		case "float64":
			k := endianness.Uint64(b[pos:])
			n = goarith.AsNumber(math.Float64frombits(k))
		default:
			panic(fmt.Sprintf("poke: unsupported selector, given '%v, should be one in '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64", Str(a[3])))
		}
		return n
	})

	// MISC

	// (sleep n) sleep n milliseconds
	interp.Def("sleep", 1, func(a []any) any {
		ints := interp.ExpectInts("sleep", a, 0, 1)
		time.Sleep(time.Millisecond * time.Duration(ints[0]))
		return Void
	})

	// (sleep-ns) sleep n nanoseconds
	interp.Def("sleep-ns", 1, func(a []any) any {
		var n int32
		SleepHiRes(time.Nanosecond*time.Duration(toUInt64("sleep-ns", a[0])), &n)
		return Void
	})

	// (add-hook-internal hook proc) => int add a hook with given id procedure, where hook is a unique integer id.
	// Returns the ID of the hook. This function does not check whether the hook exists.
	interp.Def("add-hook-internal", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(`add-hook-internal: expected integer hook ID as first argument`)
		}
		cl, ok := a[1].(*Closure)
		if !ok {
			panic(`add-hook-internal: expected closure as second argument`)
		}
		id := hooks.Add(n, func(arr []any) {
			_, err := interp.SafeEval(&Cell{cl, (&Cell{ArrayToList(arr), Nil}).MapCar(QqQuote)}, Nil)
			if err != nil {
				fmt.Println(err.(error).Error())
			}
		})
		return goarith.AsNumber(id)
	})

	// (run-hook internal hook) run all procs for hook
	interp.Def("run-hook-internal", -2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic("run-hook-internal: expected integer as hook ID")
		}
		arr := ListToArray(a[1].(*Cell))
		hooks.Exec(n, arr...)
		return Void
	})

	// (remove-hook-internal hook id) removes the hook if it exists
	interp.Def("remove-hook-internal", 2, func(a []any) any {
		n := interp.ExpectInts("remove-hook-internal", a, 0, 2)
		hooks.Remove(n[0], n[1])
		return Void
	})

	// (remove-hooks-internal hook) removes all callbacks for the hook
	interp.Def("remove-hooks-internal", 1, func(a []any) any {
		n := interp.ExpectInts("remove-hooks-internal", a, 0, 1)
		hooks.RemoveAll(n[0])
		return Void
	})

	// (beep selector) play a pre-defined system sound.
	interp.Def("beep", -1, func(a []any) any {
		if a[0].(*Cell) == Nil {
			interp.pc.SoundInterface().SystemSound(SND_READY)
			return Void
		}
		switch Str(a[0].(*Cell).Car) {
		case "error":
			interp.pc.SoundInterface().SystemSound(SND_ERROR)
		case "start":
			interp.pc.SoundInterface().SystemSound(SND_START)
		case "ready":
			interp.pc.SoundInterface().SystemSound(SND_READY)
		case "click":
			interp.pc.SoundInterface().SystemSound(SND_CLICK)
		case "okay":
			interp.pc.SoundInterface().SystemSound(SND_OKAY)
		case "confirm":
			interp.pc.SoundInterface().SystemSound(SND_CONFIRM)
		case "info":
			interp.pc.SoundInterface().SystemSound(SND_INFO)
		default:
			panic(fmt.Sprintf(`beep: unknown system sound '%s`, Str(a[0])))
		}
		return Void
	})

	// (set-volume fl) set the system volume (only has an effect if sound support is enabled)
	interp.Def("set-volume", 1, func(a []any) any {
		fl := ToFloat64(a[0])
		interp.pc.SoundInterface().SetVolume(fl)
		return Void
	})

	// EOF
	interp.Def("eof?", 1, func(a []any) any {
		return AsLispBool(a[0] == EofToken)
	})

	// (end-of-file) => EOF
	interp.Def("end-of-file", 0, func(a []any) any {
		return EofToken
	})

	// (log str) print a log entry (usually on the console)
	interp.Def("log", 1, func(a []any) any {
		log.Println(a[0].(string))
		return Void
	})

	// (sys selector [default]) => any returns configuration data, default if the configuration
	// is not supported. This is significantly scaled down from Z3S5 Machine system data. Notice that
	// rows and columns are currently returning hardcodes dummy values.
	interp.Def("sys", -1, func(in []any) any {
		interp.RLock()
		defer interp.RUnlock()
		a := ListToArray(in[0].(*Cell))
		if len(a) < 1 {
			panic("sys: missing selector argument!")
		}
		switch Str(a[0]) {
		case "rows":
			return goarith.AsNumber(40)
		case "cols", "columns":
			return goarith.AsNumber(72)
		case "build-info":
			bi, ok := debug.ReadBuildInfo()
			var buildInfoStr string
			arr := make([]any, 0)
			if ok {
				buildInfoStr = bi.Main.Version
				arr = append(arr, &Cell{bi.Main.Path, buildInfoStr})
				for _, v := range bi.Deps {
					arr = append(arr, &Cell{v.Path, v.Version})
				}
			}
			return arr
		case "version":
			var v any
			var ok bool
			v, ok = interp.GetGlobalVar(NewSym("*z3s5-version*"))
			if !ok {
				v = "<unknown-version>"
			}
			hostname, err := os.Hostname()
			if err != nil {
				hostname = "<unknown-host>"
			}
			return &Cell{
				v,
				&Cell{
					goarith.AsNumber(runtime.NumCPU()),
					&Cell{
						fmt.Sprintf("%s/%s",
							runtime.GOOS, runtime.GOARCH),
						&Cell{
							NewSym("Z3S5-Lisp"),
							&Cell{
								hostname,
								Nil},
						}}}}
		case "linecount":
			return goarith.AsNumber(40)
		case "editmode":
			return NewSym("console")
		case "inserting":
			return Nil
		case "editmodes":
			return &Cell{NewSym("console"), Nil}
		case "taskid":
			return goarith.AsNumber(int64(GetGID()))
		case "concurrency":
			return goarith.AsNumber(runtime.NumGoroutine())
		case "sys":
			return ArrayToList([]any{
				NewSym("sys"), NewSym("rows"), NewSym("cols"),
				NewSym("columns"), NewSym("version"), NewSym("linecount"), NewSym("editmode"),
				NewSym("inserting"), NewSym("editmodes"), NewSym("taskid"),
				NewSym("concurrency"),
			})
		default:
			if len(a) > 1 {
				return a[1]
			}
			return Nil
		}
	})

	// (setsys selector value) sets the configuration data to value.
	// See config for more information.
	interp.Def("setsys", 2, func(a []any) any {
		var result any = true
		switch Str(a[0]) {
		default:
			result = Nil
		}
		return result
	})

	// (break) attempts to break the current evaluation and return to the read-eval-print loop.
	interp.Def("break", 0, func(a []any) any {
		interp.Break()
		return Void // never reached
	})

	// (eval sexpr) => any evaluate the given sexpr.
	interp.Def("eval", 1, func(a []any) any {
		return interp.Eval(a[0], Nil)
	})

	// (permissions) => li list of permissions
	interp.Def("permissions", 0, func(a []any) any {
		p := interp.pc.Perm()
		perms := (&p).Strings()
		sym := make([]any, len(perms))
		for i, s := range perms {
			sym[i] = NewSym(s)
		}
		return ArrayToList(sym)
	})

	// (set-permissions li) set the permissions in li, return true if all permissions could be set
	interp.Def("set-permissions", 1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		s := make([]string, len(arr))
		for i := range s {
			s[i] = arr[i].(*Sym).String()
		}
		var err error
		p, err := NewPermissions(interp.pc.Perm(), s)
		if err != nil {
			panic(fmt.Errorf("set-permissions: %w", err))
		}
		err = interp.pc.SetPerm(p)
		if err != nil {
			panic(fmt.Errorf("set-permissions: %w", err))
		}
		return Void
	})

	// (permission? sym) => bool return true if the permission is set, nil otherwise
	interp.Def("permission?", -1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		if len(arr) == 0 {
			panic("permission?: expected a symbol as first argument")
		}
		s := arr[0].(*Sym)
		p := interp.pc.Perm()
		result, err := (&p).Get(s.String())
		if err == ErrUnknownPermission {
			if len(arr) > 1 {
				return arr[1]
			}
			return Nil
		}
		return AsLispBool(result.(bool))
	})

	// (out datum) prints a datum to the runtime output (which may not be the OS console).
	interp.Def("out", 1, func(a []any) any {
		interp.Print(Str2(a[0], false))
		return Void
	})

	// (out* datum) prints datum to the runtime output like out but quoting strings.
	interp.Def("out*", 1, func(a []any) any {
		interp.Print(Str2(a[0], true))
		return Void
	})

	// (expand-macros expr) => expr
	interp.Def("expand-macros", 1, func(a []any) any {
		return interp.ExpandMacros(a[0], 200)
	})

}

/*
  Copyright (c) 2015, 2016 OKI Software Co., Ltd.
  Copyright (c) 2019 SUZUKI Hisao
  Copyright (c) 2019-2022 Erich Rast

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/
