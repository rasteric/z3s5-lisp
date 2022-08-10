package z3s5

const SND_ERROR = 0
const SND_START = 1
const SND_READY = 2
const SND_CLICK = 3
const SND_OKAY = 4
const SND_CONFIRM = 5
const SND_INFO = 6

// Constants for hooks. These are used by various modules and introduce a flat global dependency
// on github.com/rasteric/hooks. They are defined individually in order to prevent insertion errors.
// Except for LastHook, the numbers are fixed for all future and can never change or be reused.
const SetCursorHook = 1     // Z3S5 Machine set cursor position in vram - unused in Z3S5 Lisp
const DrawTextHook = 2      // Z3S5 Machine draw text in vram - unused in Z3S5 Lisp
const PrintHook = 3         // Z3S5 Machine print text in editor - unused in Z3S5 Lisp
const LoadHook = 4          // Z3S5 Machine editor load hook - unused in Z3S5 Lisp
const LoadTextHook = 5      // Z3S5 Machine editor load text hook - unused in Z3S5 Lisp
const SaveHook = 6          // Z3S5 Machine editor save hook - unused in Z3S5 Lisp
const SaveTextHook = 7      // Z3S5 Machine editor save text hook - unused in Z3S5 Lisp
const SlowIRQHook = 8       // general slow IRQ (ca. 1 sec. intervals)
const ShutdownHook = 9      // just before the system shuts down
const SuperslowIRQHook = 10 // very slow (only every few minutes)
const ScrollUpPre = 11      // Z3S5 Machine before scrolling up - unused in Z3S5 Lisp
const ScrollUpPost = 12     // Z3S5 Machine after scrolling up - unused in Z3S5 Lisp
const ScrollDownPre = 13    // Z3S5 Machine before scrolling down - unused in Z3S5 Lisp
const ScrollDownPost = 14   // Z3S5 Machine after scrolling down - unused in Z3S5 Lisp
const StartupHook = 15      // called after the system has been started (all init files loaded)
const LastHook = 16         // *never used*

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
