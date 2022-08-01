//go:build db
// +build db

package z3s5

import (
	"database/sql"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	_ "github.com/mattn/go-sqlite3"
	"github.com/nukata/goarith"
)

var BoxedDB = NewSym("db")
var BoxedDBResult = NewSym("db-result")

// DBResult holds a DB result and scans it once.
// This is for compatibility with the API, since the API steps through individual columns
// and this was supported with an earlier used driver. Now we haver to buffer the result.
type DBResult struct {
	rows *sql.Rows
	data []any
}

// MaybeScan scans the columns of the row into an internal buffer unless this has already
// been done.
func (result *DBResult) MaybeScan() error {
	if result.data != nil {
		return nil
	}
	colTypes, err := result.rows.ColumnTypes()
	if err != nil {
		panic(err)
	}
	n := len(colTypes)
	result.data = make([]any, n)
	for i := range result.data {
		var ii any
		result.data[i] = &ii
	}
	return result.rows.Scan(result.data...)
}

// Define_Lisp_DB defines database routines with prefix db.
func (interp *Interp) Define_DB() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("db"), reflect})

	interp.DefBoxed(BoxedDB)

	interp.DefBoxed(BoxedDBResult)
	interp.DefBoxed(BoxedBlob)

	// DATABASE
	// (db.open file) => db create or open a db
	interp.Def("db.open", 1, func(a []any) any {
		var err error
		p := a[0].(string)
		var conn *sql.DB
		if strings.Contains(p, "?") {
			conn, err = sql.Open("sqlite3", filepath.FromSlash(p))
		} else {
			conn, err = sql.Open("sqlite3", filepath.FromSlash(p)+"?_busy_timeout=5000&_journal-mode=WAL&_mutex=full")
		}
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedDB, Datum: conn, Valid: true}
	})

	// (db.open* sel) => db open a temporary or in-memory db
	interp.Def("db.open*", 1, func(a []any) any {
		sym := a[0].(*Sym)
		var conn *sql.DB
		var err error
		switch sym.String() {
		case "mem":
			conn, err = sql.Open("sqlite3", "file::memory:?cache=shared")
		case "temp":
			conn, err = sql.Open("sqlite3", "")
		default:
			panic(fmt.Sprintf("db.open*: unknown selector '%v", sym.String()))
		}
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedDB, Datum: conn}
	})

	// (db.close db) close the db
	interp.Def("db.close", 1, func(a []any) any {
		db := MustGetBoxed("db.close", a[0], BoxedDB)
		handle := db.Datum.(*sql.DB)
		err := handle.Close()
		if err != nil {
			panic(err)
		}
		return Void
	})

	// (db.exec db stmt [args] ...)
	interp.Def("db.exec", -1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		if len(arr) < 1 {
			panic("db.exec: missing DB as first argument")
		}
		if len(arr) < 2 {
			panic("db.exec: missing statement string as second argument")
		}
		db := MustGetBoxed("db.exec", arr[0], BoxedDB)
		if len(arr) > 2 {
			ConvertNumbersToInts(arr[2:])
		}
		handle := db.Datum.(*sql.DB)
		_, err := handle.Exec(arr[1].(string), arr[2:]...)
		if err != nil {
			panic(err)
		}
		return Void
	})

	// (db.query db stmt [args] ...) => db-result
	interp.Def("db.query", -1, func(a []any) any {
		arr := ListToArray(a[0].(*Cell))
		if len(arr) < 1 {
			panic("db.query: missing DB as first argument")
		}
		if len(arr) < 2 {
			panic("db.query: missing statement string as second argument")
		}
		db := MustGetBoxed("db.query", arr[0], BoxedDB)
		handle := db.Datum.(*sql.DB)
		if len(arr) > 2 {
			ConvertNumbersToInts(arr[2:])
		}
		rows, err := handle.Query(arr[1].(string), arr[2:]...)
		if err != nil {
			panic(err)
		}
		return &Boxed{Sort: BoxedDBResult, Datum: &DBResult{rows: rows, data: nil}, Valid: true}
	})

	// (db.step db-result) => bool get next result row and return true, nil if there is no next row
	interp.Def("db.step", 1, func(a []any) any {
		bst := MustGetBoxed("db.step", a[0], BoxedDBResult)
		ok := bst.Datum.(*DBResult).rows.Next()
		return AsLispBool(ok)
	})

	// (db.close-result db-result)
	interp.Def("db.close-result", 1, func(a []any) any {
		bst := MustGetBoxed("db.close-result", a[0], BoxedDBResult)
		err := bst.Datum.(*DBResult).rows.Close()
		if err != nil {
			panic(err)
		}
		bst.Datum.(*DBResult).data = nil
		bst.Valid = false
		bst.Datum = nil
		return Void
	})

	// (db.int db-result column) => int or nil
	interp.Def("db.int", 2, func(a []any) any {
		bst := MustGetBoxed("db.int", a[0], BoxedDBResult)
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("db.int: column argument must be an exact integer!")
		}
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			return err
		}
		var val int64
		s, ok := (*result.data[n].(*any)).(string)
		if ok {
			val, err = strconv.ParseInt(s, 10, 64)
			if err != nil {
				panic(fmt.Errorf("db.int: could not parse string as number, given \"%s\"", s))
			}
			return goarith.AsNumber(val)
		}
		val, ok = (*result.data[n].(*any)).(int64)
		if !ok {
			panic(fmt.Errorf("db.int: expected a valid floating point number, given %s", Str((*result.data[n].(*any)))))
		}
		return goarith.AsNumber(val)
	})

	// (db.str db-result column) => str or nil
	interp.Def("db.str", 2, func(a []any) any {
		bst := MustGetBoxed("db.str", a[0], BoxedDBResult)
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("db.str: column argument must be an exact integer!")
		}
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			return err
		}
		return *result.data[n].(*any)
	})

	// (db.float db-result column) => float or nil
	interp.Def("db.float", 2, func(a []any) any {
		bst := MustGetBoxed("db.float", a[0], BoxedDBResult)
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("db.float: column argument must be an exact integer!")
		}
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			return err
		}
		var val float64
		s, ok := (*result.data[n].(*any)).(string)
		if ok {
			val, err = strconv.ParseFloat(s, 64)
			if err != nil {
				panic(fmt.Errorf("db.float: expected a valid floating point number as string, given %s: %w",
					Str(*result.data[n].(*any)), err))
			}
			return val
		}
		val, ok = (*result.data[n].(*any)).(float64)
		if !ok {
			panic(fmt.Errorf("db.float: expected a valid floating point number, given %s", Str((*result.data[n].(*any)))))
		}
		return goarith.AsNumber(val)
	})

	// (db.blob db-result column) => blob or nil
	interp.Def("db.blob", 2, func(a []any) any {
		bst := MustGetBoxed("db.blob", a[0], BoxedDBResult)
		n, exact := goarith.AsNumber(a[1]).Int()
		if !exact {
			panic("db.blob: column argument must be an exact integer!")
		}
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			return err
		}
		val, ok := (*result.data[n].(*any)).([]byte)
		if !ok {
			panic(fmt.Errorf("db.blob: unable to convert column %v to blob!", Str(result.data[n])))
		}
		return &Boxed{Sort: BoxedBlob, Datum: val}
	})

	// (db.column-count db-result) => int
	interp.Def("db.result-column-count", 1, func(a []any) any {
		bst := MustGetBoxed("db.result-column-count", a[0], BoxedDBResult)
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			return err
		}
		return goarith.AsNumber(len(result.data))
	})

	// (db.result-columns) => li return all columns as a list
	interp.Def("db.result-columns", 1, func(a []any) any {
		bst := MustGetBoxed("db.result-columns", a[0], BoxedDBResult)
		result := bst.Datum.(*DBResult)
		names, err := result.rows.Columns()
		if err != nil {
			panic(err)
		}
		sorts, err := result.rows.ColumnTypes()
		if err != nil {
			panic(err)
		}
		li := make([]any, len(names))
		for i := range names {
			li[i] = &Cell{names[i], &Cell{sorts[i].Name(), Nil}}
		}
		return ArrayToList(li)
	})

	// (db.row db-result) => li
	interp.Def("db.row", 1, func(a []any) any {
		bst := MustGetBoxed("db.row", a[0], BoxedDBResult)
		result := bst.Datum.(*DBResult)
		err := result.MaybeScan()
		if err != nil {
			panic(err)
		}
		colTypes, err := result.rows.ColumnTypes()
		if err != nil {
			panic(err)
		}
		n := len(result.data)
		li := make([]any, n)
		for i := 0; i < n; i++ {
			sort := colTypes[i].DatabaseTypeName()
			switch sort {
			case "TEXT", "VARCHAR":
				val := *result.data[n].(*any)
				li[i] = val
			case "INTEGER", "INT":
				var val int64
				s, ok := (*result.data[n].(*any)).(string)
				if ok {
					val, err = strconv.ParseInt(s, 10, 64)
					if err != nil {
						li[i] = Nil
					} else {
						li[i] = goarith.AsNumber(val)
					}
				} else {
					val = (*result.data[n].(*any)).(int64)
					li[i] = goarith.AsNumber(val)
				}
			case "FLOAT":
				var val float64
				s, ok := (*result.data[n].(*any)).(string)
				if ok {
					val, err = strconv.ParseFloat(s, 64)
					if err != nil {
						li[i] = Nil
					} else {
						li[i] = goarith.AsNumber(val)
					}
				} else {
					val = (*result.data[n].(*any)).(float64)
					li[i] = goarith.AsNumber(val)
				}
			default:
				val, ok := result.data[i].([]byte)
				if !ok {
					panic(fmt.Errorf("db.row: failed to convert column %v to a blob!", Str(goarith.AsNumber(i))))
				}
				li[i] = &Boxed{Sort: BoxedBlob, Datum: val, Valid: true}
			}
		}
		return li
	})

	// (db.fuzzify s proc) => str apply proc to each alphabetic term in a search term
	// or FTS expression without changing operators
	interp.Def("db.fuzzify", 2, func(a []any) any {
		s := a[0].(string)
		rr := strings.FieldsFunc(s, func(c rune) bool {
			return !unicode.IsLetter(c)
		})
		// we join the words because running the Lisp function once is faster!
		result := strings.Join(rr, " ")
		args := &Cell{result, Nil}
		qq := args.MapCar(QqQuote)
		intermediate := interp.Eval(&Cell{a[1], qq}, Nil)
		rr2 := strings.Split(intermediate.(string), " ")
		// now replace the originals with the fuzzified strings
		// but do not change stop words from FTS5
		var w string
		for i := range rr2 {
			if i < len(rr) {
				w = strings.ToUpper(rr[i])
				if !(w == "NEAR" || w == "OR" || w == "AND" || w == "NOT") {
					s = strings.Replace(s, rr[i], rr2[i], 1)
				}
			}
		}
		return s
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
