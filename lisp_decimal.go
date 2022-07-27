package z3s5

import (
	"fmt"

	"github.com/nukata/goarith"
	"github.com/shopspring/decimal"
)

var BoxedDecimal = NewSym("dec.num")

// Define_Graphics defines the graphics package with prefix gfx.
func (interp *Interp) Define_Decimal() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("decimal"), reflect})

	interp.DefBoxed(BoxedDecimal)

	interp.Def("dec.new", 1, func(a []any) any {
		dec, err := decimal.NewFromString(a[0].(string))
		if err != nil {
			panic(fmt.Errorf("dec.new: %w", err))
		}
		return &Boxed{Datum: dec, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.avg", -1, func(a []any) any {
		arr1 := ListToArray(a[0].(*Cell))
		if len(arr1) < 1 {
			panic("dec.avg: missing decimal numbers as arguments")
		}
		if len(arr1) == 1 {
			return arr1[0]
		}
		arr2 := make([]decimal.Decimal, len(arr1))
		for i := range arr1 {
			arr2[i] = arr1[i].(*Boxed).Datum.(decimal.Decimal)
		}
		result := decimal.Avg(arr2[0], arr2[1:]...)
		return &Boxed{Datum: result, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.max", -1, func(a []any) any {
		arr1 := ListToArray(a[0].(*Cell))
		if len(arr1) < 1 {
			panic("dec.max: missing decimal numbers as arguments")
		}
		if len(arr1) == 1 {
			return arr1[0]
		}
		arr2 := make([]decimal.Decimal, len(arr1))
		for i := range arr1 {
			arr2[i] = arr1[i].(*Boxed).Datum.(decimal.Decimal)
		}
		result := decimal.Max(arr2[0], arr2[1:]...)
		return &Boxed{Datum: result, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.min", -1, func(a []any) any {
		arr1 := ListToArray(a[0].(*Cell))
		if len(arr1) < 1 {
			panic("dec.min: missing decimal numbers as arguments")
		}
		if len(arr1) == 1 {
			return arr1[0]
		}
		arr2 := make([]decimal.Decimal, len(arr1))
		for i := range arr1 {
			arr2[i] = arr1[i].(*Boxed).Datum.(decimal.Decimal)
		}
		result := decimal.Min(arr2[0], arr2[1:]...)
		return &Boxed{Datum: result, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.sum", -1, func(a []any) any {
		arr1 := ListToArray(a[0].(*Cell))
		if len(arr1) < 1 {
			panic("dec.sum: missing decimal numbers as arguments")
		}
		if len(arr1) == 1 {
			return arr1[0]
		}
		arr2 := make([]decimal.Decimal, len(arr1))
		for i := range arr1 {
			arr2[i] = arr1[i].(*Boxed).Datum.(decimal.Decimal)
		}
		result := decimal.Sum(arr2[0], arr2[1:]...)
		return &Boxed{Datum: result, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.abs", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Abs(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.atan", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Atan(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.ceil", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Ceil(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.cmp", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return goarith.AsNumber(n.Cmp(m))
	})

	interp.Def("dec.cos", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Cos(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.div", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return &Boxed{Datum: n.Div(m), Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.div-round", 3, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[2]).Int()
		if !exact {
			panic(fmt.Errorf("dec.div-round: precision argument must be an integer, given %v", Str(precision)))
		}
		return &Boxed{Datum: n.DivRound(m, int32(precision)), Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.equal", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return AsLispBool(n.Equal(m))
	})

	interp.Def("dec.exponent", 1, func(a []any) any {
		return goarith.AsNumber(a[0].(*Boxed).Datum.(decimal.Decimal).Exponent())
	})

	interp.Def("dec.float", 1, func(a []any) any {
		f, exact := a[0].(*Boxed).Datum.(decimal.Decimal).Float64()
		return &Cell{goarith.AsNumber(f), &Cell{AsLispBool(exact), Nil}}
	})

	interp.Def("dec.floor", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Floor(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.greater-than?", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return AsLispBool(n.GreaterThan(m))
	})

	interp.Def("dec.greater-than-or-equal?", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return AsLispBool(n.GreaterThanOrEqual(m))
	})

	interp.Def("dec.int-part", 1, func(a []any) any {
		return goarith.AsNumber(a[0].(*Boxed).Datum.(decimal.Decimal).IntPart())
	})

	interp.Def("dec.is-negative?", 1, func(a []any) any {
		return AsLispBool(a[0].(*Boxed).Datum.(decimal.Decimal).IsNegative())
	})

	interp.Def("dec.is-positive?", 1, func(a []any) any {
		return AsLispBool(a[0].(*Boxed).Datum.(decimal.Decimal).IsPositive())
	})

	interp.Def("dec.is-zero?", 1, func(a []any) any {
		return AsLispBool(a[0].(*Boxed).Datum.(decimal.Decimal).IsZero())
	})

	interp.Def("dec.less-than?", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return AsLispBool(n.LessThan(m))
	})

	interp.Def("dec.less-than-or-equal?", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return AsLispBool(n.LessThan(m))
	})

	interp.Def("dec.to-blob", 1, func(a []any) any {
		data, err := a[0].(*Boxed).Datum.(decimal.Decimal).MarshalBinary()
		if err != nil {
			panic(fmt.Errorf("dec.to-blob: %w", err))
		}
		return &Boxed{Datum: data, Sort: BoxedBlob, Valid: true}
	})

	interp.Def("dec.to-json", 1, func(a []any) any {
		data, err := a[0].(*Boxed).Datum.(decimal.Decimal).MarshalJSON()
		if err != nil {
			panic(fmt.Errorf("dec.to-json: %w", err))
		}
		return string(data)
	})

	interp.Def("dec.to-text", 1, func(a []any) any {
		data, err := a[0].(*Boxed).Datum.(decimal.Decimal).MarshalText()
		if err != nil {
			panic(fmt.Errorf("dec.to-xml: %w", err))
		}
		return string(data)
	})

	interp.Def("dec.mod", 2, func(a []any) any {
		return &Boxed{
			Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Mod(a[1].(*Boxed).Datum.(decimal.Decimal)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.mul", 2, func(a []any) any {
		return &Boxed{
			Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Mul(a[1].(*Boxed).Datum.(decimal.Decimal)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.neg", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Neg(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.pow", 2, func(a []any) any {
		return &Boxed{
			Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Pow(a[1].(*Boxed).Datum.(decimal.Decimal)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.quorem", 3, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[2]).Int()
		if !exact {
			panic(fmt.Errorf("dec.quorem: precision argument must be an integer, given %v", Str(precision)))
		}
		quo, rem := n.QuoRem(m, int32(precision))
		return &Cell{
			&Boxed{Datum: quo, Sort: BoxedDecimal, Valid: true},
			&Cell{&Boxed{Datum: rem, Sort: BoxedDecimal, Valid: true},
				Nil}}
	})

	interp.Def("dec.round", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		places, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.round: places argument must be an integer, given %v", Str(places)))
		}
		return &Boxed{
			Datum: n.Round(int32(places)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.round-bank", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		places, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.round-bank: places argument must be an integer, given %v", Str(places)))
		}
		return &Boxed{
			Datum: n.RoundBank(int32(places)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.round-cash", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		places, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.round-cash: places argument must be an integer, given %v", Str(places)))
		}
		return &Boxed{
			Datum: n.RoundCash(uint8(places)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.shift", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		shift, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.shift: shift argument must be an integer, given %v", Str(shift)))
		}
		return &Boxed{
			Datum: n.Shift(int32(shift)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.sign", 1, func(a []any) any {
		return goarith.AsNumber(a[0].(*Boxed).Datum.(decimal.Decimal).Sign())
	})

	interp.Def("dec.sin", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Sin(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.str", 1, func(a []any) any {
		return a[0].(*Boxed).Datum.(decimal.Decimal).String()
	})

	interp.Def("dec.str-fixed", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.str-fixed: precision argument must be an integer, given %v", Str(precision)))
		}
		return n.StringFixed(int32(precision))
	})

	interp.Def("dec.str-fixed-bank", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.str-fixed-bank: precision argument must be an integer, given %v", Str(precision)))
		}
		return n.StringFixedBank(int32(precision))
	})

	interp.Def("dec.str-fixed-cash", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.str-fixed-cash: precision argument must be an integer, given %v", Str(precision)))
		}
		return n.StringFixedCash(uint8(precision))
	})

	interp.Def("dec.sub", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		m := a[1].(*Boxed).Datum.(decimal.Decimal)
		return &Boxed{Datum: n.Sub(m), Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.tan", 1, func(a []any) any {
		return &Boxed{Datum: a[0].(*Boxed).Datum.(decimal.Decimal).Tan(),
			Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.truncate", 2, func(a []any) any {
		n := a[0].(*Boxed).Datum.(decimal.Decimal)
		precision, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic(fmt.Errorf("dec.truncate: precision argument must be an integer, given %v", Str(precision)))
		}
		return &Boxed{
			Datum: n.Truncate(int32(precision)),
			Sort:  BoxedDecimal,
			Valid: true,
		}
	})

	interp.Def("dec.from-blob", 1, func(a []any) any {
		blob, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Errorf("dec.from-blob: expected a binary blob as argument, given %v", Str(a[0])))
		}
		if blob.Sort != BoxedBlob {
			panic(fmt.Errorf("dec.from-blob: expected a binary blob as argument, given %v", Str(a[0])))
		}
		d := decimal.Decimal{}
		err := d.UnmarshalBinary(blob.Datum.([]byte))
		if err != nil {
			panic(fmt.Errorf("dec.from-blob: %w", err))
		}
		return &Boxed{Datum: d, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.from-json", 1, func(a []any) any {
		s := a[0].(string)
		d := decimal.Decimal{}
		err := d.UnmarshalJSON([]byte(s))
		if err != nil {
			panic(fmt.Errorf("dec.from-json: %w", err))
		}
		return &Boxed{Datum: d, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.from-text", 1, func(a []any) any {
		s := a[0].(string)
		d := decimal.Decimal{}
		err := d.UnmarshalText([]byte(s))
		if err != nil {
			panic(fmt.Errorf("dec.from-text: %w", err))
		}
		return &Boxed{Datum: d, Sort: BoxedDecimal, Valid: true}
	})

	interp.Def("dec.from-num", 1, func(a []any) any {
		d, err := decimal.NewFromString(Str(a[0]))
		if err != nil {
			panic(fmt.Errorf("dec.from-num: %w", err))
		}
		return &Boxed{Datum: d, Sort: BoxedDecimal, Valid: true}
	})
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
