package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
)

func ErrorResponse(w http.ResponseWriter, code int, message string) {
	JSONResponse(w, code, map[string]interface{} {
		"response": nil,
		"error": message,
	})
}

func JSONResponse(w http.ResponseWriter, code int, payload interface{}) {
	response, _ := json.Marshal(payload)
	// if err != nil {
	// 	panic(err)
	// }

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}

func HTTPLogger(inner http.Handler, name string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()

		inner.ServeHTTP(w, r)

		log.Printf("%s\t%s\t%s\t%s", r.Method, r.RequestURI, name, time.Since(start))
	})
}

func ServerStartMsg() {
	startMessage := fmt.Sprintf("Starting Kalesville at Port %v", 8000)
	log.Printf(startMessage)
}

// Recipe ///////////////////////////////////////////////////////////////////////

type Recipe struct {
	ID          int    `json:"id"`
	Title       string `json:"title"`
	Ingredients string `json:"ingredients"`
	Procedures  string `json:"procedures"`
}

func (r *Recipe) getRecipe(db *sql.DB) error {
	return db.QueryRow(
		`SELECT "id", "title", "ingredients", "procedures" 
                 FROM "recipes" WHERE id=$1;`,
		r.ID).Scan(&r.ID, &r.Title, &r.Ingredients, &r.Procedures)
}

func (r *Recipe) createRecipe(db *sql.DB) error {
	return db.QueryRow(
		`INSERT INTO "recipes" ("id", "title", "ingredients", "procedures")
                 VALUES(DEFAULT, $1, $2, $3)
                 RETURNING "id", "title", "ingredients", "procedures";`,
		r.Title, r.Ingredients, r.Procedures).Scan(&r.ID, &r.Title, &r.Ingredients, &r.Procedures)
}

// App //////////////////////////////////////////////////////////////////////////

func (a *App) getRecipe(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	// TODO: input validation
	recipeId, _ := strconv.Atoi(vars["id"])
	recipe := Recipe{ID: recipeId}
	err := recipe.getRecipe(a.DB)
	if err != nil {
		ErrorResponse(w, 500, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, recipe)
	}
}

func (a *App) postRecipe(w http.ResponseWriter, r *http.Request) {
	var recipe Recipe
	decoder := json.NewDecoder(r.Body)

	err := decoder.Decode(&recipe)
	if err != nil {
		ErrorResponse(w, 500, err.Error())
	}

	err = recipe.createRecipe(a.DB)
	if err != nil {
		ErrorResponse(w, 500, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, recipe)
	}
}

func IndexHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "Welcome!\n")
}

type App struct {
	Router *mux.Router
	DB     *sql.DB
}

func (a *App) Initialize() {
	var err error
	// a.DB, err = sql.Open("mysql", "mysql:mysql@tcp(172.18.0.2:3306)/kalesville-web")
	a.DB, err = sql.Open("postgres", "postgres://mysql:mysql@kalesville-pg:5432/kalesville-web?sslmode=disable")
	if err != nil {
		log.Fatal(err)
	}
	a.initializeRouter()
}

type Route struct {
	Name        string
	Method      string
	Pattern     string
	HandlerFunc http.HandlerFunc
}

type Routes []Route

func (a *App) initializeRouter() {
	var routes = Routes{
		Route{"Index", "GET", "/", IndexHandler},
		Route{"GetRecipe", "GET", "/recipe/{id}", a.getRecipe},
		Route{"PostRecipe", "POST", "/recipe", a.postRecipe},
	}
	a.Router = mux.NewRouter().PathPrefix("/api").Subrouter()
	for _, route := range routes {
		var handler http.Handler

		handler = route.HandlerFunc
		handler = HTTPLogger(handler, route.Name)

		a.Router.
			Methods(route.Method).
			Path(route.Pattern).
			Name(route.Name).
			Handler(handler)
	}
}

func (a *App) Run(addr string) {
	ServerStartMsg()
	log.Fatal(http.ListenAndServe(addr, a.Router))
}

func main() {
	a := App{}
	a.Initialize()
	a.Run(":8000")
}
