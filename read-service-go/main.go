package main

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/gin-gonic/gin"
	_ "github.com/go-sql-driver/mysql"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	httpRequestsTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "todo_read_service_http_requests_total",
			Help: "Total HTTP requests handled by the read service.",
		},
		[]string{"method", "route", "status"},
	)
	httpRequestDuration = prometheus.NewHistogramVec(
		prometheus.HistogramOpts{
			Name:    "todo_read_service_http_request_duration_seconds",
			Help:    "HTTP request duration in seconds for the read service.",
			Buckets: prometheus.DefBuckets,
		},
		[]string{"method", "route", "status"},
	)
	dbFailuresTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "todo_read_service_db_failures_total",
			Help: "Database operation failures observed by the read service.",
		},
		[]string{"operation"},
	)
)

func init() {
	prometheus.MustRegister(httpRequestsTotal, httpRequestDuration, dbFailuresTotal)
}

func metricsMiddleware() gin.HandlerFunc {
	return func(c *gin.Context) {
		start := time.Now()
		c.Next()

		route := c.FullPath()
		if route == "" {
			route = "unmatched"
		}
		status := strconv.Itoa(c.Writer.Status())
		httpRequestsTotal.WithLabelValues(c.Request.Method, route, status).Inc()
		httpRequestDuration.WithLabelValues(c.Request.Method, route, status).Observe(time.Since(start).Seconds())
	}
}

func main() {
	r := gin.Default()
	r.Use(metricsMiddleware())

	dsn := os.ExpandEnv("$DATABASE_USER:$DATABASE_PASSWORD@tcp($DATABASE_HOST:$DATABASE_PORT)/$DATABASE_NAME")
	db, err := sql.Open("mysql", dsn)
	if err != nil {
		log.Fatal("Unable to connect to database:", err)
	}
	defer db.Close()

	r.GET("/healthz", func(c *gin.Context) {
		c.String(http.StatusOK, "ok\n")
	})

	r.GET("/readyz", func(c *gin.Context) {
		if err := db.Ping(); err != nil {
			dbFailuresTotal.WithLabelValues("ping").Inc()
			c.JSON(http.StatusServiceUnavailable, gin.H{"status": "unready"})
			return
		}
		c.JSON(http.StatusOK, gin.H{"status": "ready"})
	})

	r.GET("/metrics", gin.WrapH(promhttp.Handler()))

	// GET /todos - Fetch all todos
	r.GET("/todo", func(c *gin.Context) {
		rows, err := db.Query("SELECT id, title FROM todos")
		if err != nil {
			dbFailuresTotal.WithLabelValues("select_todos").Inc()
			c.JSON(http.StatusInternalServerError, gin.H{"error": "Failed to fetch todos"})
			return
		}
		defer rows.Close()

		var todos []map[string]interface{}
		for rows.Next() {
			var id int
			var title string
			if err := rows.Scan(&id, &title); err != nil {
				dbFailuresTotal.WithLabelValues("scan_todos").Inc()
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
