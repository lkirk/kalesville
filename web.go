package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	_ "github.com/lib/pq"
)

func ErrorResponse(w http.ResponseWriter, code int, message string) {
	JSONResponse(w, code, map[string]interface{}{
		"response": nil,
		"error":    message,
	})
}

func JSONResponse(w http.ResponseWriter, code int, payload interface{}) {
	response, err := json.Marshal(payload)
	if err != nil {
		errorMsg := fmt.Sprintf("Error marshalling json: %v", err.Error())
		ErrorResponse(w, http.StatusInternalServerError, errorMsg)
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}

func IndexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "Welcome!\n")
}
