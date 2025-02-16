import React from "react";
import TodoItem from "./TodoItem";
import { Todo } from "../types";

interface TodoListProps {
  todos: Todo[];
  onUpdate: (updatedTodo: Todo) => void; // Function to handle updates
  onDelete: (id: number) => void; // Function to handle deletions
}

const TodoList: React.FC<TodoListProps> = ({ todos, onUpdate, onDelete }) => {
  return (
    <ul>
      {todos.map((todo) => (
        <TodoItem key={todo.id} todo={todo} onUpdate={onUpdate} onDelete={onDelete} />
      ))}
    </ul>
  );
};

export default TodoList;
