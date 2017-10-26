package main

import (
	"database/sql"
	"errors"
	"flag"
	"fmt"
	"log"
	"net/http"
	"reflect"

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
	log.Printf("db: connecting to '%s:%d/%s'", host, port, dbname)
	db, err := sql.Open("postgres", dbString)
	if err != nil {
		log.Fatal(err)
	}
	return db
}

func runServer(addr string, db *sql.DB) {
	log.Printf("starting Kalesville at %v", addr)
	router := initializeRouter(db)
	log.Fatal(http.ListenAndServe(addr, router))
}

func validateCLIArgs(a cliArgs) {
	args := reflect.TypeOf(a)
	argValues := reflect.ValueOf(&a).Elem()
	for i := 0; i < args.NumField(); i++ {
		arg := args.Field(i)
		tag := arg.Tag.Get("required")

		required := false
		if tag == "true" {
			required = true
		}

		if required {
			value := argValues.Field(i).Interface()
			if value == "" {
				msg := fmt.Sprintf(
					"cli: %v is a required field", arg.Name)
				log.Fatal(errors.New(msg))
			}

		}

	}
}

func parseAndValidateCLIArgs() (args cliArgs) {
	flag.StringVar(&args.Addr, "addr", ":8000",
		"web address to accept requests from")
	flag.StringVar(&args.DbUser, "db-user", "", "database username")
	flag.StringVar(&args.DbPass, "db-pass", "", "database password")
	flag.StringVar(&args.DbName, "db-name", "", "database name")
	flag.StringVar(&args.DbHost, "db-host", "", "database hostname")
	flag.IntVar(&args.DbPort, "db-port", 5432, "database port")

	flag.Parse()

	validateCLIArgs(args)

	return args
}

type cliArgs struct {
	Addr   string
	DbUser string `required:"true"`
	DbPass string `required:"true"`
	DbName string `required:"true"`
	DbHost string `required:"true"`
	DbPort int    `required:"true"`
}

func main() {
	args := parseAndValidateCLIArgs()

	db := initializeDB(
		args.DbUser,
		args.DbPass,
		args.DbHost,
		args.DbName,
		args.DbPort,
	)

	runServer(args.Addr, db)
}
