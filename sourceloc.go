/*
  Z3S5 Lisp by Erich Rast.
  A Lisp-1 for Go based on Nukata Lisp 2.0 by SUZUKI Hisao.
  MIT License, see the accompanying LICENSE file.
*/
package z3s5

const (
	SourceSortFile = iota
	SourceSortInternal
)

type Source struct {
	Sort   int    // the type of the source
	Path   string // path to the source file, URL, etc.
	LineNo int    // line number (if applicable)
	Line   string // the current line (used sometimes by the reader)
}

func NewInternalSource(path, line string) *Source {
	return &Source{Sort: SourceSortInternal, Path: path}
}

func NewFileSource(path string) *Source {
	return &Source{Sort: SourceSortInternal, Path: path}
}

// Clone returns a new Source with the given line number, all other data remaining the same.
func (s Source) Clone(newLine int, line string) *Source {
	return &Source{Sort: s.Sort, Path: s.Path, LineNo: newLine, Line: line}
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
