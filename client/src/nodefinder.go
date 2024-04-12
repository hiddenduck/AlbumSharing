package main

import (
	"crypto/sha256"
	"fmt"
)

func main()  {

    albumName := "albumName"
    fileName := "fileName"

    h := sha256.New()

    h.Write([]byte(albumName))
    h.Write([]byte(fileName))

    fmt.Printf("%v\n", h.Sum(nil))
    
}

