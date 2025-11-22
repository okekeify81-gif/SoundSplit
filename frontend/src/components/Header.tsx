import React from 'react';

export const Header: React.FC = () => {
  return (
    <header style={{
      padding: '1rem 0',
      borderBottom: '1px solid #333',
      marginBottom: '2rem'
    }}>
      <h1 style={{
        margin: 0,
        color: '#00d4aa',
        fontSize: '3rem',
        fontWeight: 'bold',
        textShadow: '0 0 20px rgba(0, 212, 170, 0.3)',
        background: 'linear-gradient(45deg, #00d4aa, #00b8a0)',
        WebkitBackgroundClip: 'text',
        WebkitTextFillColor: 'transparent',
        backgroundClip: 'text'
      }}>
        🎵 SoundSplit
      </h1>
      <p style={{
        margin: '0.5rem 0 0 0',
        color: '#ccc',
        fontSize: '1.1rem'
      }}>
        Algorithmic Music Rights Distribution on Stacks
      </p>
      <div className="music-wave" style={{
        width: '200px',
        margin: '1rem auto',
        borderRadius: '20px'
      }}></div>
    </header>
  );
};
