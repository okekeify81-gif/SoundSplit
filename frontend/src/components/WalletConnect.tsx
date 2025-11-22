import React from 'react';
import { useConnect } from '@stacks/connect-react';

export const WalletConnect: React.FC = () => {
  const { doOpenAuth } = useConnect();

  return (
    <div style={{
      textAlign: 'center',
      padding: '3rem',
      background: 'linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)',
      borderRadius: '16px',
      margin: '2rem auto',
      maxWidth: '600px',
      border: '1px solid #00d4aa',
      boxShadow: '0 8px 32px rgba(0, 212, 170, 0.2)'
    }}>
      <div style={{ fontSize: '4rem', marginBottom: '1rem' }}>🎼</div>
      <h2 style={{ color: '#00d4aa', marginBottom: '1rem' }}>
        Connect Your Stacks Wallet
      </h2>
      <p style={{ color: '#ccc', marginBottom: '2rem', lineHeight: '1.6' }}>
        Join the future of music royalties. Connect your wallet to register tracks,
        add contributors, distribute earnings, and participate in fair algorithmic
        royalty distribution powered by blockchain technology.
      </p>
      <button
        onClick={() => doOpenAuth()}
        style={{
          background: 'linear-gradient(45deg, #00d4aa, #00b8a0)',
          color: '#000',
          border: 'none',
          padding: '1rem 2rem',
          borderRadius: '12px',
          fontSize: '1.2rem',
          fontWeight: 'bold',
          cursor: 'pointer',
          transition: 'all 0.3s ease',
          boxShadow: '0 4px 15px rgba(0, 212, 170, 0.3)',
          textTransform: 'uppercase',
          letterSpacing: '1px'
        }}
        onMouseOver={(e) => {
          e.currentTarget.style.transform = 'translateY(-2px)';
          e.currentTarget.style.boxShadow = '0 6px 20px rgba(0, 212, 170, 0.4)';
        }}
        onMouseOut={(e) => {
          e.currentTarget.style.transform = 'translateY(0)';
          e.currentTarget.style.boxShadow = '0 4px 15px rgba(0, 212, 170, 0.3)';
        }}
      >
        🎵 Connect Wallet
      </button>
      <p style={{
        color: '#888',
        marginTop: '2rem',
        fontSize: '0.9rem'
      }}>
        Secure • Transparent • Fair • Decentralized
      </p>
    </div>
  );
};
