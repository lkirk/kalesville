package main

import (
	"database/sql"
	"errors"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"reflect"
	"strings"

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

func getArgNameFromEnv(envPrefix string, argName string) string {
	key := strings.Join([]string{envPrefix, strings.ToUpper(argName)}, "_")
	return os.Getenv(key)
}

func validateCLIArgsCheckEnv(a cliArgs, envPrefix string) cliArgs {
	// Get environment variables from the struct field names
	// The format for this is "envPrefix_fieldName"
	// Cli arguments take precedence over env variables

	args := reflect.TypeOf(a)
	argValues := reflect.ValueOf(&a).Elem()

	for i := 0; i < args.NumField(); i++ {
		arg := args.Field(i)
		required := arg.Tag.Get("required")
		if required == "true" {
			value := argValues.Field(i).Interface()
			envValue := getArgNameFromEnv(envPrefix, arg.Name)
			if value == "" {
				if envValue != "" {
					argValues.Field(i).SetString(envValue)
					value = envValue
				}
			}
			if value == "" {
				msg := fmt.Sprintf(
					"%s: %s is a required field",
					os.Args[0],
					arg.Name)
				log.Fatal(errors.New(msg))
			}

		}

	}
	return a
}

func parseAndValidateCLIArgsOrEnvVar() (args cliArgs) {
	flag.StringVar(&args.Addr, "addr", ":8000",
		"web address to accept requests from.  env var: (KALESVILLE_ADDR)")
	flag.StringVar(&args.DbUser, "db-user", "", "database username. env var: (KALESVILLE_DBUSER)")
	flag.StringVar(&args.DbPass, "db-pass", "", "database password. env var: (KALESVILLE_DBPASS)")
	flag.StringVar(&args.DbName, "db-name", "", "database name. env var: (KALESVILLE_DBNAME)")
	flag.StringVar(&args.DbHost, "db-host", "", "database hostname. env var: (KALESVILLE_DBHOST)")
	flag.IntVar(&args.DbPort, "db-port", 5432, "database port. env var: (KALESVILLE_DBPORT)")

	flag.Parse()

	args = validateCLIArgsCheckEnv(args, "KALESVILLE")

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
	args := parseAndValidateCLIArgsOrEnvVar()

	db := initializeDB(
		args.DbUser,
		args.DbPass,
		args.DbHost,
		args.DbName,
		args.DbPort,
	)

	runServer(args.Addr, db)
}
