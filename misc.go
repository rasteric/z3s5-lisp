package z3s5

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math/big"
	"runtime"
	"strconv"
	"sync"
	"sync/atomic"
	"time"

	"github.com/gookit/color"
	"github.com/loov/hrtime"
	"github.com/nukata/goarith"
)

// Itob returns an 8-byte big endian representation of an uint64.
func Itob(v uint64) []byte {
	b := make([]byte, 8)
	binary.BigEndian.PutUint64(b, v)
	return b
}

// Btoi returns the uint64 from an 8-byte big endian representation.
func Btoi(b []byte) uint64 {
	return binary.BigEndian.Uint64(b)
}

// MinInt returns the smaller of two ints, the first argument if they are equal.
func MinInt(a, b int) int {
	if a <= b {
		return a
	}
	return b
}

// MaxInt returns the larger of two ints, the first argument if they are equal.
func MaxInt(a, b int) int {
	if a >= b {
		return a
	}
	return b
}

// GetGID returns the current goroutine's ID. This should not be abused and may break. But who cares!
// CHECK DANGER UNSAFE
func GetGID() uint64 {
	b := make([]byte, 64)
	b = b[:runtime.Stack(b, false)]
	b = bytes.TrimPrefix(b, []byte("goroutine "))
	b = b[:bytes.IndexByte(b, ' ')]
	n, _ := strconv.ParseUint(string(b), 10, 64)
	return n
}

// AsLispBool returns a given Go bool as a Lisp bool, i.e., using Nil for false.
func AsLispBool(b bool) any {
	if b {
		return true
	}
	return Nil
}

// ToBool takes a Lisp bool and returns a Go bool. See asLispBool for more information.
func ToBool(a any) bool {
	if a == Nil {
		return false
	}
	return true
}

// ToUInt8 convert a value to uint8
func ToUInt8(a any) uint8 {
	n, _ := goarith.AsNumber(a).Int()
	return uint8(n)
}

// ToUInt32 converts a value to uint32
func ToUInt32(a any) uint32 {
	n, _ := goarith.AsNumber(a).Int()
	return uint32(n)
}

// ToUInt64 attempts to convert a value to uint64
func toUInt64(caller string, a any) uint64 {
	switch a.(type) {
	case goarith.Int32:
		return uint64(a.(goarith.Int32))
	case goarith.Int64:
		return uint64(a.(goarith.Int64))
	default:
		n, exact := goarith.AsNumber(a).Int()
		if !exact {
			panic(fmt.Sprintf("%v: expected exact integer, given %v", caller, a))
		}
		return uint64(n)
	}
}

// ToMaybeUInt64 attempts to convert a value to uint64
func ToMaybeUInt64(a any) (uint64, bool) {
	switch a.(type) {
	case goarith.Int32:
		return uint64(a.(goarith.Int32)), true
	case goarith.Int64:
		return uint64(a.(goarith.Int64)), true
	default:
		n, exact := goarith.AsNumber(a).Int()
		if !exact {
			return uint64(n), false
		}
		return uint64(n), true
	}
}

// toInt64 attempts to convert a value to int64
func ToInt64(caller string, a any) int64 {
	switch a.(type) {
	case goarith.Int32:
		return int64(a.(goarith.Int32))
	case goarith.Int64:
		return int64(a.(goarith.Int64))
	default:
		n, exact := goarith.AsNumber(a).Int()
		if !exact {
			panic(fmt.Sprintf("%v: expected exact integer, given %v", caller, a))
		}
		return int64(n)
	}
}

// ConvertNumbersToInts converts any goarith.Int32 and goarith.Int64 values in a slice
// of interface{} values to Int64, modifying the array. The array can no longer be used
// for representing Lisp numbers afterwards.
func ConvertNumbersToInts(a []any) {
	for i := range a {
		switch a[i].(type) {
		case goarith.Int32:
			a[i] = int64(a[i].(goarith.Int32))
		case goarith.Int64:
			a[i] = int64(a[i].(goarith.Int64))
		}
	}
}

// ConvertIntsToNumbers converts any Int64 integer values in a slice to goarith.Int64 values,
// modifying the array. The int64 values in the array can afterwards be used in Lisp.
func ConvertIntsToNumbers(a []any) {
	for i := range a {
		switch a[i].(type) {
		case int32:
			a[i] = goarith.AsNumber(a[i])
		case int64:
			a[i] = goarith.AsNumber(a[i])
		}
	}
}

func ToFloat64(x any) float64 {
	switch x.(type) {
	case goarith.Int32:
		return float64(x.(goarith.Int32))
	case goarith.Int64:
		return float64(int64(x.(goarith.Int64)))
	case goarith.Float64:
		return float64(x.(goarith.Float64))
	case *goarith.BigInt:
		z := new(big.Rat).SetInt((*big.Int)(x.(*goarith.BigInt)))
		f, _ := z.Float64() // f may be infinity.
		return float64(f)
	case float32:
		return float64(x.(float32))
	default:
		panic(fmt.Sprintf("expected floating point value, given %v", x))
	}
}

func toFloat32(x any) float32 {
	switch x.(type) {
	case goarith.Int32:
		return float32(x.(goarith.Int32))
	case goarith.Int64:
		return float32(int64(x.(goarith.Int64)))
	case goarith.Float64:
		return float32(x.(goarith.Float64))
	case *goarith.BigInt:
		z := new(big.Rat).SetInt((*big.Int)(x.(*goarith.BigInt)))
		f, _ := z.Float64() // f may be infinity.
		return float32(f)
	case float32:
		return x.(float32)
	default:
		panic(fmt.Sprintf("expected floating point value, given %v", x))
	}
}

// ConvertTimeToDateList converts a time.Time value into a Lisp datelist.
func ConvertTimeToDateList(t time.Time) *Cell {
	arr := make([]any, 5)
	arr[0] = t.Year()
	arr[1] = int(t.Month())
	arr[2] = t.Day()
	arr[3] = int(t.Weekday())
	_, arr[4] = t.ISOWeek()
	arr2 := make([]any, 5)
	arr2[0] = t.Hour()
	arr2[1] = t.Minute()
	arr2[2] = t.Second()
	arr2[3] = t.Nanosecond()
	arr2[4] = t.UnixNano()
	return &Cell{ArrayToList(arr), &Cell{ArrayToList(arr2), Nil}}
}

// ListToArray converts a list into an array (Go slice).
func ListToArray(li *Cell) []any {
	arr := make([]any, 0)
	if li == Nil {
		return arr
	}
	for {
		arr = append(arr, li.Car)
		li = li.Cdr.(*Cell)
		if li == Nil {
			return arr
		}
	}
}

// ArrayToList converts an array (Go slice) to a list.
func ArrayToList(arr []any) *Cell {
	if len(arr) == 0 {
		return Nil
	}
	start := &Cell{}
	li := start
	for i, _ := range arr {
		li.Car = arr[i]
		if i != len(arr)-1 {
			li.Cdr = &Cell{}
		} else {
			li.Cdr = Nil
		}
		li = li.Cdr.(*Cell)
	}
	return start
}

// reverse array returns an array reversed as copy
func ReverseArray(arr []any) []any {
	arr2 := make([]any, len(arr))
	n := len(arr2)
	for i := range arr2 {
		arr2[i] = arr[n-i-1]
	}
	return arr2
}

// reverse a string
func ReverseStr(s string) string {
	r := []rune(s)
	n := len(r)
	r2 := make([]rune, n)
	for i := range r2 {
		r2[i] = r[n-i-1]
	}
	return string(r2)
}

// dictToArray converts a dict (Go map) to an array.
func DictToArray(dict any) []any {
	m := dict.(*Dict)
	arr := make([]any, 0, 1024)
	m.Data.Range(func(k, v any) bool {
		arr = append(arr, k)
		arr = append(arr, v)
		return true
	})
	return arr
}

// copyMap copies a interface{} map deeply.
func CopyMap(m map[any]any) map[any]any {
	cp := make(map[any]any)
	for k, v := range m {
		vm, ok := v.(map[any]any)
		if ok {
			cp[k] = CopyMap(vm)
		} else {
			cp[k] = v
		}
	}
	return cp
}

// copyDict copies a Dict deeply.
func CopyDict(m *Dict) *Dict {
	var cp sync.Map
	m.Data.Range(func(k, v any) bool {
		vm, ok := v.(*Dict)
		if ok {
			cp.Store(k, CopyDict(vm))
		} else {
			cp.Store(k, v)
		}
		return true
	})
	return &Dict{Data: &cp, Protected: m.Protected}
}

// SleepNS blocks for duration and then returns. This function busy loops over a
// high precision timer.
func SleepHiRes(duration time.Duration, cancel *int32) {
	t0 := hrtime.Now()
	for hrtime.Since(t0) < duration && atomic.LoadInt32(cancel) < 2 {
		runtime.Gosched()
	}
}

// colorToList converts a Raylib color to a Lisp list.
func colorToList(c color.RGBColor) *Cell {
	return &Cell{
		goarith.AsNumber(int(c[0])),
		&Cell{
			goarith.AsNumber(int(c[1])),
			&Cell{
				goarith.AsNumber(int(c[2])),
				&Cell{
					goarith.AsNumber(255),
					Nil}}}}
}

// listToColor converts a color list to a Raylib color.
func listToColor(caller string, li *Cell) color.RGBColor {
	arr := ListToArray(li)
	r := byte(ToInt64(caller, arr[0]))
	g := byte(ToInt64(caller, arr[1]))
	b := byte(ToInt64(caller, arr[2]))
	return color.RGBColor{r, g, b}
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
