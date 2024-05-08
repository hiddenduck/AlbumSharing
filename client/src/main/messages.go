package main

import "fmt"

func myprint(msg string) {
	fmt.Printf("%v\n", msg)
}

func CreateMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"print": myprint,
	}
}
