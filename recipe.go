package main

import (
	"database/sql"
)

type Recipe struct {
	ID          int    `json:"id"`
	Title       string `json:"title"`
	Ingredients string `json:"ingredients"`
	Procedures  string `json:"procedures"`
}

func (r *Recipe) createRecipe(db *sql.DB) error {
	return db.QueryRow(
		`INSERT INTO "recipes" ("id", "title", "ingredients", "procedures")
                 VALUES(DEFAULT, $1, $2, $3)
                 RETURNING "id", "title", "ingredients", "procedures";`,
		r.Title, r.Ingredients, r.Procedures).Scan(&r.ID, &r.Title, &r.Ingredients, &r.Procedures)
}

func (r *Recipe) getRecipe(db *sql.DB) error {
	return db.QueryRow(
		`SELECT "id", "title", "ingredients", "procedures" 
                 FROM "recipes" WHERE id=$1;`,
		r.ID).Scan(&r.ID, &r.Title, &r.Ingredients, &r.Procedures)
}

func getRecipesGivenDB(db *sql.DB) (recipes []Recipe, err error) {
	recipes = make([]Recipe, 0)
	rows, err := db.Query(
		`SELECT "id", "title", "ingredients", "procedures" FROM "recipes";`)
	if err != nil {
		return
	}
	defer rows.Close()
	for rows.Next() {
		recipe := Recipe{}
		err := rows.Scan(
			&recipe.ID,
			&recipe.Title,
			&recipe.Ingredients,
			&recipe.Procedures)
		recipes = append(recipes, recipe)
		if err != nil {
			return recipes, err
		}
	}
	return
}
