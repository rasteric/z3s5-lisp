package z3s5

import (
	"fmt"
	"sync"
)

// Boxed is the type of an opaque, non-writable Lisp object.
// It holds the actual object as interface{} and manages the Lisp type and
// printing.
type Boxed struct {
	Datum    any
	Sort     *Sym
	Template string
	Equal    func(other *Boxed) bool
	Valid    bool
}

// Print the object into a string.
func (o *Boxed) Print() string {
	if o.Template == "" {
		return fmt.Sprintf("#<%v %p>", Str(o.Sort), o.Datum)
	}
	return fmt.Sprintf(o.Template, Str(o.Sort), o.Datum)
}

// ExpectBoxed returns a boxed value of type sort if a is one and true, nil and false otherwise.
func ExpectBoxed(a any, sort *Sym) (*Boxed, bool) {
	if b, ok := a.(*Boxed); ok {
		if b.Sort == sort {
			return b, true
		}
	}
	return nil, false
}

// MustGetBoxed returns a boxed value of type sort or panics.
func MustGetBoxed(caller string, a any, sort *Sym) *Boxed {
	if b, ok := ExpectBoxed(a, sort); ok {
		if b.Valid {
			return b
		}
		panic(fmt.Sprintf("%v: the %v is invalid!", caller, sort))
	}
	panic(fmt.Sprintf("%v: expected a value of type '%v, given %v", caller, Str(sort), Str(a)))
}

// BoxedArray is a synchronized array of boxed items.
type BoxedArray struct {
	data  []*Boxed
	mutex *sync.Mutex
}

// NewBoxedArray returns a new empty array for boxed items.
func NewBoxedArray() *BoxedArray {
	b := BoxedArray{}
	b.data = make([]*Boxed, 0)
	b.mutex = &sync.Mutex{}
	return &b
}

// Add a new boxed item to the array.
func (a *BoxedArray) Add(obj *Boxed) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	a.data = append(a.data, obj)
}

// Remove a boxed item from the array.
func (a *BoxedArray) Remove(obj *Boxed) {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	for i := range a.data {
		if a.data[i] == obj {
			copy(a.data[i:], a.data[i+1:])
			a.data = a.data[:len(a.data)-1]
			break
		}
	}
}

// Range is used to traverse a boxed array with a function that takes an index and an object and returns bool.
// If the function returns false, then the traversal stops. Otherwise it continues until the whole array has been
// traversed.
func (a *BoxedArray) Range(f func(i int, obj *Boxed) bool) {
	for i, obj := range a.data {
		if !f(i, obj) {
			break
		}
	}
}

// Clear the array.
func (a *BoxedArray) Clear() {
	a.mutex.Lock()
	defer a.mutex.Unlock()
	for i := range a.data {
		a.data[i] = nil
	}
	a.data = make([]*Boxed, 0)
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
