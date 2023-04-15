import React from 'react';
import './App.css';

function App() {

  function changeBackground(e) {
    e.target.style.background = 'red';
  }

  return (
    <div className="App">
      <button onMouseOver={changeBackground}>Hover over me!</button>
    </div>
  );
}

export default App;