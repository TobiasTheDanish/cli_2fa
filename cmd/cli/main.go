package main

import (
	"fmt"
	"os"
)

func main() {
	err := StartTui()
	if err != nil {
		fmt.Printf("Error during execution: %v", err)
		os.Exit(1)
	}
}
