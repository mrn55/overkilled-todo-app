package main

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/gin-gonic/gin"
	_ "github.com/go-sql-driver/mysql"
)

func main() {
	r := gin.Default()

	dsn := os.ExpandEnv("$DATABASE_USER:$DATABASE_PASSWORD@tcp($DATABASE_HOST:$DATABASE_PORT)/$DATABASE_NAME")
	db, err := sql.Open("mysql", dsn)
	if err != nil {
		log.Fatal("Unable to connect to database:", err)
	}
	defer db.Close()

	// GET /todos - Fetch all todos
	r.GET("/todo", func(c *gin.Context) {
		rows, err := db.Query("SELECT id, title FROM todos")
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to fetch todos"})
			return
		}
		defer rows.Close()

		var todos []map[string]interface{}
		for rows.Next() {
			var id int
			var title string
			if err := rows.Scan(&id, &title); err != nil {
				c.JSON(http.StatusInternalServerError, gin.H{"error": "Error reading row"})
				return
			}
			todos = append(todos, gin.H{"id": id, "title": title})
		}

		c.JSON(http.StatusOK, todos)
	})

	// Start server
	fmt.Println("Read service running on port 5002...")
	r.Run(":5002")
}
