package main

import (
	"database/sql"
)

type Comment struct {
	ID     int    `json:"id"`
	Author string `json:"author"`
	Text   string `json:"text"`
}

func (c *Comment) createComment(db *sql.DB) error {
	return db.QueryRow(
		`INSERT INTO "user_comments" ("id", "author", "text")
                 VALUES(DEFAULT, $1, $2)
                 RETURNING "id", "author", "text";`,
		c.Author, c.Text).Scan(&c.ID, &c.Author, &c.Text)
}

func (c *Comment) getComment(db *sql.DB) error {
	return db.QueryRow(
		`SELECT "id", "author", "text"
                 FROM "user_comments" WHERE id=$1;`,
		c.ID).Scan(&c.ID, &c.Author, &c.Text)
}

func getCommentsGivenDB(db *sql.DB) (comments []Comment, err error) {
	comments = make([]Comment, 0)
	rows, err := db.Query(
		`SELECT "id", "author", "text" FROM "user_comments";`)
	if err != nil {
		return
	}
	defer rows.Close()
	for rows.Next() {
		comment := Comment{}
		err := rows.Scan(
			&comment.ID,
			&comment.Author,
			&comment.Text)
		comments = append(comments, comment)
		if err != nil {
			return comments, err
		}
	}
	return
}
