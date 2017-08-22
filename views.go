package main

import (
	"database/sql"
	"encoding/json"
	"net/http"
	"strconv"

	"github.com/gorilla/mux"
	_ "github.com/lib/pq"
)

func getRecipe(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	// TODO: input validation
	recipeId, _ := strconv.Atoi(vars["id"])
	recipe := Recipe{ID: recipeId}
	err := recipe.getRecipe(db)

	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, recipe)
	}
}

func getRecipes(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	recipes, err := getRecipesGivenDB(db)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, recipes)
	}
}

func postRecipe(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	var recipe Recipe
	decoder := json.NewDecoder(r.Body)

	err := decoder.Decode(&recipe)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	}

	err = recipe.createRecipe(db)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, recipe)
	}
}

func getComment(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	// TODO: input validation
	commentId, _ := strconv.Atoi(vars["id"])
	comment := Comment{ID: commentId}
	err := comment.getComment(db)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, comment)
	}
}

func getComments(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	comments, err := getCommentsGivenDB(db)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, comments)
	}
}

func postComment(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	var comment Comment
	decoder := json.NewDecoder(r.Body)

	err := decoder.Decode(&comment)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	}

	err = comment.createComment(db)
	if err != nil {
		ErrorResponse(w, http.StatusInternalServerError, err.Error())
	} else {
		JSONResponse(w, http.StatusOK, comment)
	}
}
