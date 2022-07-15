package z3s5

import "sync"

type Dict struct {
	Data      *sync.Map
	Protected bool
}
