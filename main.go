package main

import (
	"database/sql"
	"flag"
	"fmt"
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

// type Route struct {
// 	route   string
// 	handler func(Env) (w http.ResponseWriter, r *http.Request)
// 	name    string
// 	method  string
// }

// type Routes []Route

func initializeRouter(db *sql.DB) *mux.Router {
	router := mux.NewRouter().PathPrefix("/api").Subrouter()

	router.HandleFunc("/", IndexHandler).Name("Index").Methods("GET")
	router.HandleFunc("/recipe/{id}", func(w http.ResponseWriter, r *http.Request) {
		getRecipe(db, w, r)
	}).Name("GetRecipe").Methods("GET")
	router.HandleFunc("/recipes", func(w http.ResponseWriter, r *http.Request) {
		getRecipes(db, w, r)
	}).Name("GetRecipes").Methods("GET")
	router.HandleFunc("/recipe", func(w http.ResponseWriter, r *http.Request) {
		postRecipe(db, w, r)
	}).Name("PostRecipe").Methods("POST")
	router.HandleFunc("/comment/{id}", func(w http.ResponseWriter, r *http.Request) {
		getComment(db, w, r)
	}).Name("GetComment").Methods("GET")
	router.HandleFunc("/comments", func(w http.ResponseWriter, r *http.Request) {
		getComments(db, w, r)
	}).Name("GetComments").Methods("GET")
	router.HandleFunc("/comment", func(w http.ResponseWriter, r *http.Request) {
		postComment(db, w, r)
	}).Name("PostComment").Methods("POST")

	return router
}

func initializeDB(user, password, host, dbname string, port int) *sql.DB {
	dbString := fmt.Sprintf("postgres://%s:%s@%s:%d/%s?sslmode=disable",
		user, password, host, port, dbname)
	log.Printf("connecting to %s:%d/%s", host, port, dbname)
	db, err := sql.Open("postgres", dbString)
	if err != nil {
		log.Fatal(err)
	}
	return db
}

func runServer(addr string, db *sql.DB) {
	log.Printf("Starting Kalesville at %v", addr)
	router := initializeRouter(db)
	log.Fatal(http.ListenAndServe(addr, router))
}

func parseCLIArgs(addr, dbUser, dbPass, dbName, dbHost *string, dbPort *int) {
	flag.StringVar(addr, "addr", ":8000", "web address to accept requests from")

	flag.StringVar(dbUser, "db-user", "postgres", "database username")
	flag.StringVar(dbPass, "db-pass", "postgres", "database password")
	flag.StringVar(dbName, "db-name", "kalesville-web", "database name")
	flag.StringVar(dbHost, "db-host", "kalesville", "database hostname")
	flag.IntVar(dbPort, "db-port", 5432, "database port")

	flag.Parse()
}

func main() {
	var (
		addr   string
		dbUser string
		dbPass string
		dbName string
		dbHost string
		dbPort int
	)

	parseCLIArgs(&addr, &dbUser, &dbPass, &dbName, &dbHost, &dbPort)
	db := initializeDB(dbUser, dbPass, dbHost, dbName, dbPort)

	runServer(addr, db)
}
