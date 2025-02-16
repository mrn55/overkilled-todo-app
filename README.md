### **The Overkill(ed) Todo App**
#### _Because a simple CRUD app just wasn't complicated enough, I've made a very simple CRUD app!_

Welcome to the **Overkill(ed) Todo App**, where we've taken the simplest concept—managing a list of tasks—and turned it into a **polyglot microservices nightmare**. 🎉  

This is the summation of a three day project that I wanted to put together. ChatGPTy did prove to be helpful pulling a lot of this together, but it falls pretty short when the Haskell and Erlang came up. I nearly gave up a couple times, just on what little code I had to write. Watching each piece come together and start working felt real good. I had a couple times I wanted to give up when working with Haskell or Erlang. The others I've either worked in or built at least something in during my past.

### **📜 The Tech Stack (a.k.a. "Why?")**
Yes, I could've gone with a basic **Monolith**… but **NO**! I said, _"We shall use multiple languages, just because we can."_  

| Service        | Language / Framework | Purpose |
|---------------|----------------------|---------|
| `create-service` | **Haskell (Scotty)** | Adds todos to the database |
| `read-service`   | **Go (Gin Gonic)** | Retrieves todos (because REST exists) |
| `update-service` | **Rust (Axum)** | Updates todos (fast and memory safe!) |
| `delete-service` | **Erlang (Cowboy)** | Deletes todos (because BEAM is cool) |
| `api-gateway`    | **NGINX** | Routes requests to the right service |
| `todo-frontend`  | **Vite + React + TypeScript** | The UI (because vanilla JS would be too easy) |
| `db`            | **MariaDB** | The poor, abused database |
| `phpmyadmin`    | **PHPMyAdmin** | Because we like graphical DB tools |

---

### **🚀 Getting Started**
To run this **masterpiece of engineering** (or madness):  
1. **Clone the repo**  
   ```sh
   git clone https://github.com/your-repo/overkill-todo.git && cd overkill-todo
   ```
2. **Make sure you have Docker & Docker Compose** installed.  
3. **Run the whole thing with one command**  
   ```sh
   mv .env-example .env
   docker-compose up --build -d
   ```
4. **Access the magic:**  
   - **Frontend:** [`http://localhost`](http://localhost)  
   - **API Gateway:** [`http://localhost/todo`](http://localhost/todo) (_/api/v1 with a version would be a better choice here I sus_)
   - **Database UI:** [`http://localhost:8080`](http://localhost:8080) (_if you must peek at the DB_)  

---

### **🛠️ Environment Variables (Handled by Docker)**
Instead of hardcoding things like some kind of barbarian (tbf I did, but at the behest of a friend), we're using **Docker Compose** and a `.env` file:  
```ini
DATABASE_HOST=db
DATABASE_USER=todo_user
DATABASE_PASSWORD=supersecurepassword!
DATABASE_NAME=todo_db
DATABASE_PORT=3306
```
Docker injects these into all services automatically, so you don’t have to.

---

### **📂 Project Structure**
```bash
.
├── create-service-haskell/   # Adds todos (functional & elegant) (gpt put elegant, i did not)
├── read-service-go/          # Fetches todos (efficient & boring) (gpt put boring, i did not)
├── update-service-rust/      # Updates todos (blazingly fast™)
├── delete-service-erlang/    # Deletes todos (because BEAM)
├── todo-frontend/            # Vite + React + TypeScript frontend
├── api-gateway.conf          # NGINX routes requests like a traffic cop
├── docker-compose.yml        # Ties the madness together
├── .env                      # Centralized environment variables
└── README.md                 # This wonderful guide
```

---

### **🛑 Troubleshooting**
#### **1. Something isn't working?**
Here you have a couple options:
Check logs:
```sh
docker-compose logs -f <service-name>
```

Try:  
```sh
docker-compose down && docker-compose up --build -d
```
And hope for the best.  

Delete this repo:
```sh
cd .. && rm overkilled-todo-app/
```
And same here, hope for the best.

#### **2. Frontend not showing?**
Make sure `todo-frontend` is either:  
- Served via `api-gateway` (via NGINX config), but you would need to update the api-gateway.conf to accommodate that.
- Running on a separate port (e.g., `http://localhost:3000`) Warning here: check api-gateway.conf for the `Access-Control-Allow-Origin` port.

#### **3. Database not connecting?**
- Check `phpmyadmin` at [`http://localhost:8080`](http://localhost:8080).  
- Your `.env` should match the MySQL/MariaDB service.  

---

### **🎯 Final Thoughts**
This project is a **tech demo,** a **joke,** and a **completely unnecessary use of microservices**—all in one. But if you **actually** need to scale your TODO list to enterprise levels, well… you've got a head start. 🚀  I think some future enhancements would be minikube deployment, some helm charts, testing, tracing, and authentication. But for now, I'll quit dodging the honey-do list and show my face to the family for a bit.

> **Built with way too much effort for a simple TODO app.**  

Enjoy! 🎩