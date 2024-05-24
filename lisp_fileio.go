//go:build fileio
// +build fileio

package z3s5

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"

	"github.com/adrg/xdg"
	"github.com/nukata/goarith"
)

var ErrPortNotWritable = errors.New("port not writable")
var ErrPortNotReadable = errors.New("port not readable")
var ErrPortNotSeekable = errors.New("port not seekable")
var ErrNoPort = errors.New("invalid port or operation not supported")

var BoxedFilePort = NewSym("file-port")
var BoxedStringPort = NewSym("str-port")
var BoxedURIPort = NewSym("uri-port")

var PortTypes []*Sym
var fileIOMutex sync.Mutex

const DefaultOpenFlags = os.O_RDWR | os.O_APPEND | os.O_CREATE
const DefaultFilePermissions = 0o640

func init() {
	PortTypes = make([]*Sym, 0)
	AddPortType(BoxedFilePort)
	AddPortType(BoxedStringPort)
	AddPortType(BoxedURIPort)
}

// AddPortType adds a foreign port type to the system so port? knows about it.
// This symbol needs to be the one in boxed.Sort of the new BoxedPort created and added with DefBoxed, like
// in the examples of BoxedFilePort and BoxedStringPort.
func AddPortType(sym *Sym) {
	fileIOMutex.Lock()
	defer fileIOMutex.Unlock()
	PortTypes = append(PortTypes, sym)
}

// Define_FileIO defines file-i/o-related functions that allow for direct filesystem access
// without safeguards. For security reasons this module is "opt-in" by using the build-tag "io".
// Even if it is included, the functions will only work if permissions are given for
// AllowFileRead and AllowFileWrite respectively.
func (interp *Interp) Define_FileIO() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("fileio"), reflect})

	// register boxed values
	interp.DefBoxed(BoxedFilePort)
	interp.DefBoxed(BoxedStringPort)
	interp.DefBoxed(BoxedURIPort)

	// (open file [flags] [permissions]) => port open a file for reading or writing.
	// If the file does not exist, it is created. The given file path must be a slash path
	// and is converted to a local path.
	interp.Def("open", -1, func(a []any) any {
		args := ListToArray(a[0].(*Cell))
		le := len(args)
		if le < 1 {
			panic(`open: missing argument, expected file path as first argument`)
		}
		file := args[0].(string)
		var flags, perm int
		flags = DefaultOpenFlags
		perm = DefaultFilePermissions
		if le > 1 {
			flags = FileFlagsToInt("open", args[1].(*Cell))
			if le > 2 {
				perm = FilePermissionToInt("open", args[2])
			}
		}

		var err error
		var fi *os.File
		path := filepath.FromSlash(file)
		fi, err = os.OpenFile(path, flags, os.FileMode(perm))
		if err != nil {
			panic(fmt.Errorf("open: %w", err))
		}
		port := &Boxed{Datum: fi, Sort: BoxedFilePort, Valid: true}

		ioReader, _ := port.Datum.(io.Reader)
		iowriter, _ := port.Datum.(io.Writer)
		ioseeker, _ := port.Datum.(io.Seeker)
		iowriterat, _ := port.Datum.(io.WriterAt)
		stream := NewStream(NewFileSource(path), ioReader, iowriter, ioseeker, iowriterat)
		interp.Streams().Store(port, stream)
		return port
	})

	// (close port) close the given port
	interp.Def("close", 1, func(a []any) any {
		port, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("close: expected valid port, given %v", Str(a[0])))
		}
		if !port.Valid {
			return Void // do nothing since it is already closed or never was valid
		}

		fileIOMutex.Lock()
		defer fileIOMutex.Unlock()

		fi := port.Datum
		switch fi.(type) {
		case *bytes.Buffer:
			port.Valid = false
			fi.(*bytes.Buffer).Reset()
		case io.ReadWriteCloser:
			port.Valid = false
			fi.(io.ReadWriteCloser).Close()
		default:
			panic(ErrNoPort)
		}

		interp.Streams().Delete(port)
		return Void
	})

	// (port? x) => bool return t if x is a port, nil otherwise
	interp.Def("port?", 1, func(a []any) any {
		port, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		fileIOMutex.Lock()
		defer fileIOMutex.Unlock()
		found := false
		for i := range PortTypes {
			if PortTypes[i] == port.Sort {
				found = true
				break
			}
		}
		return AsLispBool(found)
	})

	// (file-exists? fi) => bool return true if the file exists at given path, nil otherwise
	interp.Def("file-exists?", 1, func(a []any) any {
		path := a[0].(string)
		if _, err := os.Stat(path); err == nil {
			return true
		}
		return Nil
	})

	// (dir? fi) => bool returns true if the given file exists and is a directory, nil otherwise
	interp.Def("dir?", 1, func(a []any) any {
		path := a[0].(string)
		if fileInfo, err := os.Stat(path); err == nil {
			return AsLispBool(fileInfo.IsDir())
		}
		return Nil
	})

	// (readable? port) => bool return true if the stream can be read from (but it may be empty), nil otherwise
	interp.Def("readable?", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			return Nil
		}
		stream, ok := s.(*Stream)
		if !ok {
			return Nil
		}
		if stream.LispReader.IOReader() == nil {
			return Nil
		}
		return true
	})

	// (writable? id) => bool return true if the stream can be written to, nil otherwise
	interp.Def("writable?", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			return Nil
		}
		stream, ok := s.(*Stream)
		if !ok {
			return Nil
		}
		if stream.LispWriter.IOWriter() == nil {
			return Nil
		}
		return true
	})

	// (seekable? id) => bool return true if the stream supports seek, nil otherwise
	interp.Def("seekable?", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			return Nil
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			return Nil
		}
		stream, ok := s.(*Stream)
		if !ok {
			return Nil
		}
		if stream.LispSeeker.IOSeeker() == nil {
			return Nil
		}
		return true
	})

	// (read port) => any read one expression from port, EOF if no more to read
	interp.Def("read", 1, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("read: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("read: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("read: stream not valid (internal error)")
		}
		result, errObj := stream.LispReader.Read()
		if err, ok := errObj.(error); ok && err != nil {
			if err == EofToken {
				return EofToken
			}
			panic(fmt.Errorf("read: %w", err.Error()))
		}
		return result
	})

	// (write port datum) => int write datum to port, return number of bytes written
	interp.Def("write", 2, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("write: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("write: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("write: stream not valid (internal error)")
		}
		k, err := stream.LispWriter.Write(a[1])
		if err != nil {
			panic(fmt.Sprintf("write: %v", err))
		}
		return goarith.AsNumber(k)
	})

	// (write-string port s) => int writes s to port
	interp.Def("write-string", 2, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("write-string: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("write-string: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("write-string: stream not valid (internal error)")
		}
		k, err := io.WriteString(stream.LispWriter.IOWriter(), a[1].(string))
		if err != nil {
			panic(fmt.Sprintf("write-string: %v", err))
		}
		return goarith.AsNumber(k)
	})

	// (read-string p del)
	interp.Def("read-string", 2, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("read-string: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("read-string: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("read-string: stream not valid (internal error)")
		}
		del := []byte(a[1].(string))
		if len(del) == 0 {
			panic("read-string: empty delimiter string, the string must contain one single-byte delimiter character")
		}
		if len(del) > 1 {
			panic("read-string: delimiter string too long, read-string currenly only supports single-byte characters")
		}
		b, err := stream.BuffReader.BuffIOReader().ReadString(del[0])
		if err != nil && err != io.EOF {
			panic(fmt.Errorf("read-string: %w", err))
		}
		return string(b)
	})

	// (seek id pos sel) => offset seek to pos in stream id, depending on selector sel. Return the new offset.
	interp.Def("seek", 3, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf(`seek: not a valid port port, given %v`, Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic(`seek: stream not valid`)
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("seek: stream not valid (internal error)")
		}
		if stream.LispSeeker.IOSeeker() == nil {
			panic(fmt.Sprintf(`seek: the stream port %v is not seekable (no random access supported)`, Str(a[0])))
		}
		pos := ToInt64("seek", a[1])
		var whence int
		switch Str(a[2]) {
		case "start":
			whence = io.SeekStart
		case "current":
			whence = io.SeekCurrent
		case "end":
			whence = io.SeekEnd
		default:
			panic(fmt.Sprintf(`seek: selector must be one of '(start current end), given %v`, Str(a[2])))
		}
		offset, err := stream.LispSeeker.IOSeeker().Seek(pos, whence)
		if err != nil {
			panic(fmt.Errorf(`seek: %w`, err.Error()))
		}
		return goarith.AsNumber(offset)
	})

	// (read-binary id buff n) => int read n bytes of data from stream id into buff, return the number of bytes read
	interp.Def("read-binary", 3, func(a []any) any {
		obj, ok := a[0].(*Boxed)

		if !ok {
			panic(fmt.Sprintf("read-binary: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("read-binary: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("read-binary: stream not valid (internal error)")
		}
		blob := MustGetBoxed("read-binary", a[1], BoxedBlob)
		buff := blob.Datum.([]byte)
		n, exact := goarith.AsNumber(a[2]).Int()
		if !exact {
			panic(fmt.Sprintf("read-binary: expected integer, given %v", n))
		}
		k, err := io.ReadAtLeast(stream.LispReader.IOReader(), buff, n)
		if err == EofToken {
			return goarith.AsNumber(0)
		}
		if err == io.ErrUnexpectedEOF {
			return goarith.AsNumber(k)
		}
		if err != nil {
			panic(fmt.Errorf(`read-binary: %w`, err))
		}
		return goarith.AsNumber(k)
	})

	// (write-binary id buff n offset) => int write n bytes from buff at offset into stream id,
	// return the number of bytes written
	interp.Def("write-binary", 4, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("write-binary: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("write-binary: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("write-binary: stream not valid (internal error)")
		}
		blob := MustGetBoxed("write-binary", a[1], BoxedBlob)
		buff := blob.Datum.([]byte)
		n, exact := goarith.AsNumber(a[2]).Int()
		if !exact {
			panic(fmt.Sprintf("write-binary: expected integer for number of bytes to write, given %v", n))
		}
		k, exact := goarith.AsNumber(a[3]).Int()
		if !exact {
			panic(fmt.Sprintf("write-binary: expected integer for offset, given %v", k))
		}
		if len(buff) < n+k {
			panic(`write-binary: buffer overflow`)
		}
		i, err := stream.LispWriter.IOWriter().Write(buff[k : n+k])
		if err != nil {
			panic(fmt.Errorf(`write-binary: %w`, err))
		}
		return goarith.AsNumber(i)
	})

	// (write-binary-at id buff n offset fpos) write datum to seekable stream id from 0-indexed offset into position fpos
	interp.Def("write-binary-at", 5, func(a []any) any {
		obj, ok := a[0].(*Boxed)
		if !ok {
			panic(fmt.Sprintf("write-binary-at: expected valid port, given %v", Str(a[0])))
		}
		s, ok := interp.Streams().Load(obj)
		if !ok {
			panic("write-binary-at: stream not valid")
		}
		stream, ok := s.(*Stream)
		if !ok {
			panic("write-binary-at: stream not valid (internal error)")
		}
		blob := MustGetBoxed("write-binary-at", a[1], BoxedBlob)
		buff := blob.Datum.([]byte)
		n, exact := goarith.AsNumber(a[2]).Int()
		if !exact {
			panic(fmt.Sprintf("write-binary-at: expected integer for number of bytes to write, given %v", n))
		}
		k, exact := goarith.AsNumber(a[3]).Int()
		if !exact {
			panic(fmt.Sprintf("write-binary-at: expected integer for buffer offset, given %v", k))
		}
		if len(buff) < n+k {
			panic(`write-binary-at: buffer overflow`)
		}
		fpos := ToInt64("write-binary-at", a[4])
		i, err := stream.LispWriterAt.IOWriterAt().WriteAt(buff[k:n+k], fpos)
		if err != nil {
			panic(fmt.Errorf(`write-binary-at: %w`, err))
		}
		return goarith.AsNumber(i)
	})

	// (stropen s) => p open a string as a stream
	interp.Def("stropen", 1, func(a []any) any {
		s := a[0].(string)
		buff := bytes.NewBufferString(s)
		obj := &Boxed{Datum: buff, Sort: BoxedStringPort, Valid: true}

		in, ok := obj.Datum.(io.Reader)
		if !ok {
			panic(fmt.Errorf(`stropen:%w`, ErrPortNotReadable))
		}
		out, ok := obj.Datum.(io.Writer)
		if !ok {
			panic(fmt.Errorf(`stropen:%w`, ErrPortNotWritable))
		}

		stream := NewStream(NewInternalSource("stropen", s), in, out, nil, nil)
		interp.Streams().Store(obj, stream)
		return obj
	})

	// (dir [path]) => li obtain a list of files, if no path is specified the current directory is listed
	interp.Def("dir", -1, func(a []any) any {
		var current string
		li := a[0].(*Cell)
		if li == Nil {
			current = "."
		} else {
			current = filepath.FromSlash(li.Car.(string))
		}
		file, err := os.Open(current)
		if err != nil {
			panic(fmt.Errorf(`dir: %w`, err))
		}

		defer file.Close()
		names, err := file.Readdirnames(0)
		if err != nil {
			panic(fmt.Errorf(`dir: %w`, err))
		}
		arr := make([]any, len(names))
		for i := range names {
			arr[i] = names[i]
		}
		return ArrayToList(arr)
	})

	// (sysdir sel) => str obtain a special system path based on the given selector
	interp.Def("sysdir", 1, func(a []any) any {
		sym := a[0].(*Sym)
		s := sym.String()
		switch s {
		case "data":
			return xdg.DataHome
		case "data-dirs":
			return StrArrayToList(xdg.DataDirs)
		case "config":
			return xdg.ConfigHome
		case "config-dirs":
			return StrArrayToList(xdg.ConfigDirs)
		case "state":
			return xdg.StateHome
		case "cache":
			return xdg.CacheHome
		case "runtime":
			return xdg.RuntimeDir
		case "home":
			return xdg.Home
		case "application-dirs":
			return StrArrayToList(xdg.ApplicationDirs)
		case "font-dirs":
			return StrArrayToList(xdg.FontDirs)
		case "desktop":
			return xdg.UserDirs.Desktop
		case "downloads":
			return xdg.UserDirs.Download
		case "documents":
			return xdg.UserDirs.Documents
		case "music":
			return xdg.UserDirs.Music
		case "pictures":
			return xdg.UserDirs.Pictures
		case "videos":
			return xdg.UserDirs.Videos
		case "templates":
			return xdg.UserDirs.Templates
		case "public", "shared":
			return xdg.UserDirs.PublicShare
		case "z3s5-data":
			d := xdg.DataHome + "/z3s5"
			os.MkdirAll(filepath.FromSlash(d), 0777)
			return d
		default:
			panic(fmt.Sprintf("sysdir: unknown directory '%v, the selector must be one of '(data data-dirs config config-dirs state cache runtime home application-dirs font-dirs desktop downloads documents music pictures videos templates public shared z3s5-data)",
				Str(sym)))
		}
	})

	// (mkdir path [permissions]) creates all directories in path that don't exist yet
	interp.Def("mkdir", -1, func(a []any) any {
		c := a[0].(*Cell)
		if c == Nil {
			panic("mkdir: missing path argument")
		}
		perm := 0777
		if c.CdrCell() != Nil {
			perm = FilePermissionToInt("mkdir", c.CdrCell().Car)
		}
		os.MkdirAll(filepath.FromSlash(c.Car.(string)), os.FileMode(perm))
		return Void
	})

	// (cd path) change the working directory to path
	interp.Def("cd", 1, func(a []any) any {
		if err := os.Chdir(filepath.FromSlash(a[0].(string))); err != nil {
			panic(fmt.Errorf(`cd: %w`, err))
		}
		return Void
	})

	// (fdelete path) remove the file or directory at path
	interp.Def("fdelete", 1, func(a []any) any {
		if err := os.RemoveAll(a[0].(string)); err != nil {
			panic(fmt.Errorf(`fdelete: %w`, err))
		}
		return Void
	})
}

// FileFlagsToInt converts a Lisp list of symbolic flags to the corresponding host os flag int
// of the modes for opening and creating files.
func FileFlagsToInt(caller string, a *Cell) int {
	fail := func() {
		panic(fmt.Sprintf("%v: only one of read, write, read-write can be specified in file flags, given %v",
			caller, Str(a)))
	}
	arr := ListToArray(a)
	flag := 0
	excl := false
	for _, s := range arr {
		sym, ok := s.(*Sym)
		if !ok {
			panic(fmt.Sprintf("%v: expected file mode flag symbol, given %v", caller, Str(s)))
		}
		switch sym.Name {
		case "read", "r", "rdonly":
			if excl {
				fail()
			}
			flag = flag | os.O_RDONLY
			excl = true
		case "write", "w", "wronly":
			if excl {
				fail()
			}
			flag = flag | os.O_WRONLY
			excl = true
		case "read-write", "rw", "rdwr":
			if excl {
				fail()
			}
			flag = flag | os.O_RDWR
			excl = true
		case "append", "a":
			flag = flag | os.O_APPEND
		case "create", "c":
			flag = flag | os.O_CREATE
		case "excl", "exclusive", "e":
			flag = flag | os.O_EXCL
		case "sync", "s":
			flag = flag | os.O_SYNC
		case "trunc", "truncate", "t":
			flag = flag | os.O_TRUNC
		default:
			panic(fmt.Sprintf(`%v: unknown file mode flag '%v in '%v`, caller, s, Str(a)))
		}
	}
	return flag
}

// FilePermissionsToInt returns the file permissions as an integer value.
func FilePermissionToInt(caller string, a any) int {
	n, exact := goarith.AsNumber(a).Int()
	if !exact {
		panic(fmt.Sprintf("%v: expected integer for file permissions, given %v", caller, Str(a)))
	}
	return n
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
