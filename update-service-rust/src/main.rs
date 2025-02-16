use axum::{
    extract::{Path, State},
    routing::put,
    Router,
    Json,
};
use serde::{Deserialize, Serialize};
use sqlx::mysql::MySqlPool;
use std::{env, net::SocketAddr};

#[derive(Debug, Deserialize)]
struct UpdateTodo {
    title: String,
}

#[derive(Debug, Serialize)]
struct Todo {
    id: i32,
    title: String,
}

#[tokio::main]
async fn main() {
    let db_host = env::var("DATABASE_HOST").expect("DATABASE_HOST must be set");
    let db_user = env::var("DATABASE_USER").expect("DATABASE_USER must be set");
    let db_pass = env::var("DATABASE_PASSWORD").expect("DATABASE_PASSWORD must be set");
    let db_name = env::var("DATABASE_NAME").expect("DATABASE_NAME must be set");
    let db_port = env::var("DATABASE_PORT").unwrap_or_else(|_| "3306".to_string());

    let database_url = format!(
        "mysql://{}:{}@{}:{}/{}",
        db_user, db_pass, db_host, db_port, db_name
    );

    // let pool = MySqlPool::connect(database_url).await.expect("Failed to connect to database");
    let pool = MySqlPool::connect(&database_url)
    .await
    .expect("Failed to connect to database");

    let app = Router::new()
        .route("/todo/{id}", put(update_todo))
        .with_state(pool);

    let addr = SocketAddr::from(([0, 0, 0, 0], 5003));
    println!("Update service running on {}", addr);

    axum::serve(tokio::net::TcpListener::bind(addr).await.unwrap(), app.into_make_service())
        .await
        .unwrap();
}

#[axum::debug_handler]
async fn update_todo(
    State(pool): State<MySqlPool>,
    Path(id): Path<i32>,
    Json(payload): Json<UpdateTodo>,
) -> Result<Json<Todo>, Json<String>> {
    let result = sqlx::query("UPDATE todos SET title = ? WHERE id = ?")
        .bind(&payload.title)
        .bind(id)
        .execute(&pool)
        .await;

        match result {
            Ok(query_result) if query_result.rows_affected() > 0 => {
                // Construct the updated todo response directly
                let updated_todo = Todo {
                    id,
                    title: payload.title,
                };
                Ok(Json(updated_todo))
            }
            Ok(_) => Err(Json("Todo not found".to_string())), // No rows affected, meaning ID was invalid
            Err(err) => {
                eprintln!("Database error: {:?}", err);
                Err(Json("Failed to update todo".to_string()))
            }
        }
}
