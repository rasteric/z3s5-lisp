package z3s5

import (
	"fmt"
	"math"

	"github.com/nukata/goarith"
)

// Define_Float defines floating point related functions with prefix fl.
func (interp *Interp) Define_Float() {
	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("float"), reflect})

	// MATH FLOATING POINT FUNCTIONS START
	// direct mapping to Go math library for float64 only
	// prefix fl.
	interp.Def("fl.abs", 1, func(a []any) any {
		return goarith.AsNumber(math.Abs(ToFloat64(a[0])))
	})

	interp.Def("fl.acos", 1, func(a []any) any {
		return goarith.AsNumber(math.Acos(ToFloat64(a[0])))
	})

	interp.Def("fl.asin", 1, func(a []any) any {
		return goarith.AsNumber(math.Asin(ToFloat64(a[0])))
	})

	interp.Def("fl.asinh", 1, func(a []any) any {
		return goarith.AsNumber(math.Asinh(ToFloat64(a[0])))
	})

	interp.Def("fl.atan", 1, func(a []any) any {
		return goarith.AsNumber(math.Atan(ToFloat64(a[0])))
	})

	interp.Def("fl.atan2", 2, func(a []any) any {
		return goarith.AsNumber(math.Atan2(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.atanh", 1, func(a []any) any {
		return goarith.AsNumber(math.Atanh(ToFloat64(a[0])))
	})

	interp.Def("fl.cbrt", 1, func(a []any) any {
		return goarith.AsNumber(math.Cbrt(ToFloat64(a[0])))
	})

	interp.Def("fl.ceil", 1, func(a []any) any {
		return goarith.AsNumber(math.Ceil(ToFloat64(a[0])))
	})

	interp.Def("fl.cos", 1, func(a []any) any {
		return goarith.AsNumber(math.Cos(ToFloat64(a[0])))
	})

	interp.Def("fl.cosh", 1, func(a []any) any {
		return goarith.AsNumber(math.Cosh(ToFloat64(a[0])))
	})

	interp.Def("fl.dim", 2, func(a []any) any {
		return goarith.AsNumber(math.Dim(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.erf", 1, func(a []any) any {
		return goarith.AsNumber(math.Erf(ToFloat64(a[0])))
	})

	interp.Def("fl.erfcinv", 1, func(a []any) any {
		return goarith.AsNumber(math.Erfcinv(ToFloat64(a[0])))
	})

	interp.Def("fl.erfc", 1, func(a []any) any {
		return goarith.AsNumber(math.Erfc(ToFloat64(a[0])))
	})

	interp.Def("fl.erfinv", 1, func(a []any) any {
		return goarith.AsNumber(math.Erfinv(ToFloat64(a[0])))
	})

	interp.Def("fl.exp", 1, func(a []any) any {
		return goarith.AsNumber(math.Exp(ToFloat64(a[0])))
	})

	interp.Def("fl.exp2", 1, func(a []any) any {
		return goarith.AsNumber(math.Exp2(ToFloat64(a[0])))
	})

	interp.Def("fl.expm1", 1, func(a []any) any {
		return goarith.AsNumber(math.Expm1(ToFloat64(a[0])))
	})

	interp.Def("fl.fma", 3, func(a []any) any {
		return goarith.AsNumber(math.FMA(ToFloat64(a[0]),
			ToFloat64(a[1]), ToFloat64(a[2])))
	})

	interp.Def("fl.floor", 1, func(a []any) any {
		return goarith.AsNumber(math.Floor(ToFloat64(a[0])))
	})

	interp.Def("fl.frexp", 1, func(a []any) any {
		fl, n := math.Frexp(ToFloat64(a[0]))
		return &Cell{goarith.AsNumber(fl), &Cell{goarith.AsNumber(n), Nil}}
	})

	interp.Def("fl.gamma", 1, func(a []any) any {
		return goarith.AsNumber(math.Gamma(ToFloat64(a[0])))
	})

	interp.Def("fl.hypot", 2, func(a []any) any {
		return goarith.AsNumber(math.Hypot(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.ilogb", 1, func(a []any) any {
		return goarith.AsNumber(math.Ilogb(ToFloat64(a[0])))
	})

	interp.Def("fl.inf", 1, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(fmt.Sprintf(`inf: expected exact integer, given an inexact number %v`, a[0]))
		}
		return goarith.AsNumber(math.Inf(n))
	})

	interp.Def("fl.is-nan?", 1, func(a []any) any {
		return AsLispBool(math.IsNaN(ToFloat64(a[0])))
	})

	interp.Def("fl.j0", 1, func(a []any) any {
		return goarith.AsNumber(math.J0(ToFloat64(a[0])))
	})

	interp.Def("fl.j1", 1, func(a []any) any {
		return goarith.AsNumber(math.J1(ToFloat64(a[0])))
	})

	interp.Def("fl.jn", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(fmt.Sprintf("fl.jn: first argument must be an integer, given %v", Str(a[0])))
		}
		return goarith.AsNumber(math.Jn(n, ToFloat64(a[1])))
	})

	interp.Def("fl.ldexp", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Sprintf(`ldexp: expected exact integer as second argument, given an inexact number %v`, a[1]))
		}
		return goarith.AsNumber(math.Ldexp(ToFloat64(a[0]), n))
	})

	interp.Def("fl.lgamma", 1, func(a []any) any {
		g, n := math.Lgamma(ToFloat64(a[0]))
		return &Cell{goarith.AsNumber(g), &Cell{goarith.AsNumber(n), Nil}}
	})

	interp.Def("fl.log", 1, func(a []any) any {
		return goarith.AsNumber(math.Log(ToFloat64(a[0])))
	})

	interp.Def("fl.log10", 1, func(a []any) any {
		return goarith.AsNumber(math.Log10(ToFloat64(a[0])))
	})

	interp.Def("fl.log1p", 1, func(a []any) any {
		return goarith.AsNumber(math.Log1p(ToFloat64(a[0])))
	})

	interp.Def("fl.log2", 1, func(a []any) any {
		return goarith.AsNumber(math.Log2(ToFloat64(a[0])))
	})

	interp.Def("fl.logb", 1, func(a []any) any {
		return goarith.AsNumber(math.Logb(ToFloat64(a[0])))
	})

	interp.Def("fl.max", 2, func(a []any) any {
		return goarith.AsNumber(math.Max(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.min", 2, func(a []any) any {
		return goarith.AsNumber(math.Min(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.mod", 2, func(a []any) any {
		return goarith.AsNumber(math.Mod(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.modf", 1, func(a []any) any {
		n, f := math.Modf(ToFloat64(a[0]))
		return &Cell{goarith.AsNumber(n), &Cell{goarith.AsNumber(f), Nil}}
	})

	interp.Def("fl.nan", 0, func(a []any) any {
		return goarith.AsNumber(math.NaN())
	})

	interp.Def("fl.next-after", 2, func(a []any) any {
		return goarith.AsNumber(math.Nextafter(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.pow", 2, func(a []any) any {
		return goarith.AsNumber(math.Pow(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.pow10", 1, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(fmt.Sprintf(`pow10: expected exact integer as argument, given an inexact number %v`, a[0]))
		}
		return goarith.AsNumber(math.Pow10(n))
	})

	interp.Def("fl.remainder", 2, func(a []any) any {
		return goarith.AsNumber(math.Remainder(ToFloat64(a[0]), ToFloat64(a[1])))
	})

	interp.Def("fl.round", 1, func(a []any) any {
		return goarith.AsNumber(math.Round(ToFloat64(a[0])))
	})

	interp.Def("fl.round-to-even", 1, func(a []any) any {
		return goarith.AsNumber(math.RoundToEven(ToFloat64(a[0])))
	})

	interp.Def("fl.signbit", 1, func(a []any) any {
		return AsLispBool(math.Signbit(ToFloat64(a[0])))
	})

	interp.Def("fl.sin", 1, func(a []any) any {
		return goarith.AsNumber(math.Sin(ToFloat64(a[0])))
	})

	interp.Def("fl.sinh", 1, func(a []any) any {
		return goarith.AsNumber(math.Sinh(ToFloat64(a[0])))
	})

	interp.Def("fl.sqrt", 1, func(a []any) any {
		return goarith.AsNumber(math.Sqrt(ToFloat64(a[0])))
	})

	interp.Def("fl.tan", 1, func(a []any) any {
		return goarith.AsNumber(math.Tan(ToFloat64(a[0])))
	})

	interp.Def("fl.tanh", 1, func(a []any) any {
		return goarith.AsNumber(math.Tanh(ToFloat64(a[0])))
	})

	interp.Def("fl.trunc", 1, func(a []any) any {
		return goarith.AsNumber(math.Trunc(ToFloat64(a[0])))
	})

	interp.Def("fl.y0", 1, func(a []any) any {
		return goarith.AsNumber(math.Y0(ToFloat64(a[0])))
	})

	interp.Def("fl.y1", 1, func(a []any) any {
		return goarith.AsNumber(math.Y1(ToFloat64(a[0])))
	})

	interp.Def("fl.yn", 2, func(a []any) any {
		n, exact := goarith.AsNumber(a[0]).Int()
		if !exact {
			panic(fmt.Sprintf(`yn: expected exact integer as first argument, given an inexact number %v`, a[0]))
		}
		return goarith.AsNumber(math.Yn(n, ToFloat64(a[1])))
	})

	// MATH FUNCTIONS END
}

/*
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
