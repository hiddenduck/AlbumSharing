package main

import "fmt"

func myprint(msg []string) {
	for i, x := range msg {
		fmt.Printf("frame %v: %v\n", i, x)
	}
}

func CreateMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"print": myprint,
	}
}
