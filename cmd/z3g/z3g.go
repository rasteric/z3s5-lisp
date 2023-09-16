package main

import (
	"flag"
	"fmt"
	"os"
	"strings"
	"sync"

	"github.com/rasteric/hooks"
	z3 "github.com/rasteric/z3s5-lisp"
	z3ui "github.com/rasteric/z3s5-lisp/gui"
)

func run() int {
	load := flag.String("l", "", "load the specified file and execute it in a non-interactive session")
	exec := flag.String("e", "", "execute the expression(s) given as argument at startup")
	interactive := flag.Bool("i", false, "run the interpreter interactively even if a file is loaded with -l")
	silent := flag.Bool("s", false, "set *interactive-session* false even if -i is specified to prevent printing a start banner")
	flag.Parse()
	interp, err := z3.NewInterp(z3.NewBasicRuntime(z3.FullPermissions))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to start: %v\n", err)
		return 1
	}
	// the following needs to be set before boot to prevent printing start banner
	// it sets the global variable *interactive-session*
	interp.SetInteractive(((*load == "" && *exec == "") || *interactive) && !(*silent))
	err = interp.Boot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to boot the standard prelude: %v\n", err)
		return 2
	}
	// Load GUI functions and their help definitions.
	z3ui.DefGUI(interp, z3ui.DefaultConfig)
	if err := z3ui.DefGUIHelp(interp); err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return 3
	}
	interp.SafeEval(&z3.Cell{Car: z3.NewSym("protect-toplevel-symbols"), Cdr: z3.Nil}, z3.Nil)
	//	hooks.Exec(z3.StartupHook, nil)
	defer hooks.Exec(z3.ShutdownHook, nil)
	if *load != "" {
		file, err := os.Open(*load)
		if err != nil {
			panic(err)
		}
		defer file.Close()
		if !interp.Run(file, z3.NewFileSource(*load)) {
			fmt.Fprintf(os.Stderr, "Z3S5 Lisp error in input file \"%v\" given with -l flag.\n", *load)
			return 3
		}
		if !(*interactive) && *exec == "" {
			return 0
		}
	}
	if *exec != "" {
		ss := strings.NewReader(*exec)
		if !interp.Run(ss, z3.NewInternalSource("cmdline-exec-string", *exec)) {
			fmt.Fprintf(os.Stderr, "Z3S5 Lisp error in input expression given with -e flag.\n")
			return 4
		}
		if !(*interactive) {
			return 0
		}
	}
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		interp.Run(nil, z3.NewInternalSource("repl", ""))
	}()
	z3ui.RunGUI()
	defer z3ui.ShutDownGUI()
	wg.Wait()
	return 0
}

func main() {
	os.Exit(run())
}

/*
  Copyright (c) 2015, 2016 OKI Software Co., Ltd.
  Copyright (c) 2019 SUZUKI Hisao
  Copyright (c) 2022-2023 Erich Rast

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
