// src/components/TodoForm.tsx
import React, { useState } from 'react';
import { Todo } from '../types'; // Import the Todo type

interface TodoFormProps {
  addTodo: (newTodo: Omit<Todo, 'id'>) => void; // addTodo expects a Todo without 'id'
}

const TodoForm: React.FC<TodoFormProps> = ({ addTodo }) => {
  const [title, setTitle] = useState<string>('');

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (title.trim()) {
      addTodo({ title });
      setTitle('');
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        value={title}
        onChange={(e) => setTitle(e.target.value)}
        placeholder="Enter a new todo"
      />
      <button type="submit">Add Todo</button>
    </form>
  );
};

export default TodoForm;
