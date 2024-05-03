package main

import (
	"crypto/sha256"
	"fmt"
	"os"
)

func main()  {

    fileName := os.Args[1]

    dat, _ := os.ReadFile(fileName)

    h := sha256.New()

    h.Write(dat)

    fmt.Printf("%v\n", h.Sum(nil))
    
}

