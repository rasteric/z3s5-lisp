package main

import (
	"fmt"
	"os/exec"
	"testing"
)

func TestDistribution(t *testing.T) {
	cmd := exec.Command("z3", "-l test-distribution.lisp")
	if err := cmd.Run(); err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			panic(fmt.Errorf("Lisp internal tests failed with error code %v", exitError.ExitCode()))
		}
	}
}
