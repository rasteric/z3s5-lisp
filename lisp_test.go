package z3s5

import (
	"fmt"
	"os/exec"
	"testing"
)

func TestDistribution(t *testing.T) {
	cmd := exec.Command("make", "test")
	_, err := cmd.Output()
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			panic(fmt.Errorf("Lisp internal tests failed with error code %v", exitError.ExitCode()))
		}
	}
}
