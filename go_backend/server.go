package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"net/http"
	"strconv"
	"fmt"
	color "github.com/fatih/color"
)

/* Behold/beware: the world's worst Go code, probably */

type TwoInts struct {
	Number1 int
	Number2 int
}

func calculateProduct(prod TwoInts) int {
	return prod.Number1 * prod.Number2
}

func toKey(prod TwoInts) string {
	n1 := strconv.Itoa(prod.Number1)
	n2 := strconv.Itoa(prod.Number2)
	return n1 + "," + n2
}

const cachingServiceUrl = "http://127.0.0.1:8082/cache/"

func logMessage(msg string, args... any) {
	c := color.New(color.FgMagenta)
	c.Print("[GO] ")
	fmt.Printf(msg + "\n", args...)
}

func checkCache(prod TwoInts) (int, error) {
	key := toKey(prod)
	logMessage("Looking up key '%v' in cache", key)

	url := cachingServiceUrl + key
	resp, err := http.Get(url)
	if err != nil {
		logMessage("GET request failed: %v", err)
	}

	if resp.StatusCode >= 200 && resp.StatusCode < 300 {
		var n int
		json.NewDecoder(resp.Body).Decode(&n)
		return n, nil
	}

	return -999, errors.New("Cache lookup failed")
}

func addToCache(key string, product int) {
	logMessage("Attempting to add key-value pair ('%v', %v) to cache", key, product)

	// The payload/body is just the integer product
	json, err := json.Marshal(product)
	if err != nil {
		logMessage("Error marshalling payload: %v", err)
	}

	url := cachingServiceUrl + key
	request, err := http.NewRequest(http.MethodPut, url, bytes.NewBuffer(json))
	if err != nil {
		logMessage("Error building PUT request: %v", err)
	}
	request.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	_, err = client.Do(request)
	if err != nil {
		logMessage("PUT request failed: %v", err)
	}
}

func expensiveProduct(w http.ResponseWriter, r *http.Request) {
	var numbers TwoInts
	err := json.NewDecoder(r.Body).Decode(&numbers)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	w.Header().Set("Content-Type", "application/json")

	res, err := checkCache(numbers)
	if err != nil {
		logMessage("Cache lookup didn't succeed")
		product := calculateProduct(numbers)
		addToCache(toKey(numbers), product)
		json.NewEncoder(w).Encode(product)
		return
	}

	logMessage("Cache lookup succeeded")
	json.NewEncoder(w).Encode(res)
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/expensiveProduct", expensiveProduct)
	logMessage("Listening on port 8081...")
	http.ListenAndServe(":8081", mux)
}
