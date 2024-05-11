package dataservers

import (
	"bufio"
	"crypto/sha256"
	"io"
	"log"
	"os"
)

func HashFile(fileName string) Hash {

	h := sha256.New()

	fd, err := os.Open(fileName)

	if err != nil {
		log.Fatalf("Error to read [file=%v]: %v", fileName, err.Error())
	}

	r := bufio.NewReader(fd)

	buf := make([]byte, CHUNK_SIZE)

	for {

		bytes_read, err := r.Read(buf)

		if bytes_read != CHUNK_SIZE {
			buf = buf[:bytes_read]
		}

		if bytes_read == 0 {
			if err == nil {
				continue
			}
			if err == io.EOF {
				break
			}
			log.Fatal(err)
		}

		_, err = h.Write(buf)

		// process buf
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}
	}

	return [32]byte(h.Sum(nil))
}
