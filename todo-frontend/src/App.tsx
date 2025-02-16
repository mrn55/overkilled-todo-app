import { useState, useEffect } from 'react';
import axios from 'axios';
import TodoList from './components/TodoList';
import TodoForm from './components/TodoForm';
import { Todo } from './types'; // Import the Todo type

const App = () => {
  const [todos, setTodos] = useState<Todo[]>([]);

  // Fetch todos from the API on load
  useEffect(() => {
    axios.get<Todo[]>('http://localhost/todo')
      .then(response => setTodos(response.data))
      .catch(error => console.error(error));
  }, []);

  // Function to handle adding a new todo
  const addTodo = (newTodo: Omit<Todo, 'id'>) => {
    axios.post('http://localhost/todo', newTodo)
      .then(response => setTodos([...todos, response.data]))
      .catch(error => console.error(error));
  };

  const updateTodoInList = (updatedTodo: Todo) => {
    setTodos((prevTodos) =>
      prevTodos.map((todo) =>
        todo.id === updatedTodo.id ? updatedTodo : todo
      )
    );
  };

  const removeTodoFromList = (id: number) => {
    setTodos((prevTodos) => prevTodos.filter((todo) => todo.id !== id));
  };

  return (
    <div>
      <h1>Todo App</h1>
      <TodoForm addTodo={addTodo} />
      <TodoList todos={todos} onUpdate={updateTodoInList} onDelete={removeTodoFromList} />
    </div>
  );
};

export default App;
