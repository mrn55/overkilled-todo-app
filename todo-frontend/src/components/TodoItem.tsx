import React, { useState } from "react";
import { Todo } from "../types"; // Import Todo type
import axios from "axios";

interface TodoItemProps {
  todo: Todo;
  onUpdate: (updatedTodo: Todo) => void; // Callback to update the UI
  onDelete: (id: number) => void; // Callback to remove the item from the list
}

const TodoItem: React.FC<TodoItemProps> = ({ todo, onUpdate, onDelete }) => {
  const [isEditing, setIsEditing] = useState(false);
  const [newTitle, setNewTitle] = useState(todo.title);

  // Handle Update (PUT request)
  const updateTodo = async () => {
    try {
      const response = await axios.put(`http://localhost/todo/${todo.id}`, {
        title: newTitle,
      });
      onUpdate(response.data); // Update state in parent
      setIsEditing(false); // Exit edit mode
    } catch (error) {
      console.error("Error updating todo:", error);
    }
  };

  // Handle Delete
  const deleteTodo = async () => {
    try {
      await axios.delete(`http://localhost/todo/${todo.id}`);
      onDelete(todo.id); // Remove from UI
    } catch (error) {
      console.error("Error deleting todo:", error);
    }
  };

  return (
    <li>
      {isEditing ? (
        <>
          <input
            type="text"
            value={newTitle}
            onChange={(e) => setNewTitle(e.target.value)}
            onKeyDown={(e) => e.key === "Enter" && updateTodo()} // Press Enter to save
          />
          <button onClick={updateTodo}>Save</button>
          <button onClick={() => setIsEditing(false)}>Cancel</button>
        </>
      ) : (
        <>
          <span onDoubleClick={() => setIsEditing(true)}>{todo.title}</span>
          <button onClick={() => setIsEditing(true)}>Edit</button>
          <button onClick={deleteTodo}>Delete</button>
        </>
      )}
    </li>
  );
};

export default TodoItem;
