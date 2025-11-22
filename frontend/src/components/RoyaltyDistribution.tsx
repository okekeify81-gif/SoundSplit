import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';

interface Track {
  id: number;
  title: string;
  artist: string;
  uploader: string;
  audioHash: string;
  metadataUri: string;
  totalEarnings: number;
  isLocked: boolean;
  createdAt: number;
}

interface RoyaltyDistributionProps {
  track: Track;
  userSession: UserSession;
  onClose: () => void;
  onDistributionComplete: () => void;
}

export const RoyaltyDistribution: React.FC<RoyaltyDistributionProps> = ({
  track,
  userSession,
  onClose,
  onDistributionComplete
}) => {
  const [amount, setAmount] = useState('');
  const [distributing, setDistributing] = useState(false);
  const [preview, setPreview] = useState<any>(null);

  // Mock contributor data - in real implementation, this would come from contract
  const contributors = [
    { address: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM', role: 'Vocalist', percentage: 50 },
    { address: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5', role: 'Producer', percentage: 30 },
    { address: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG', role: 'Mixer', percentage: 20 }
  ];

  const calculateDistribution = (totalAmount: number) => {
    const platformFee = Math.floor(totalAmount * 0.025); // 2.5% platform fee
    const distributableAmount = totalAmount - platformFee;

    return {
      platformFee,
      distributableAmount,
      distributions: contributors.map(contributor => ({
        ...contributor,
        amount: Math.floor(distributableAmount * (contributor.percentage / 100))
      }))
    };
  };

  const handleAmountChange = (value: string) => {
    setAmount(value);
    if (value && !isNaN(Number(value))) {
      const numAmount = parseInt(value);
      if (numAmount > 0) {
        setPreview(calculateDistribution(numAmount));
      } else {
        setPreview(null);
      }
    } else {
      setPreview(null);
    }
  };

  const handleDistribute = async () => {
    const numAmount = parseInt(amount);
    if (!numAmount || numAmount <= 0) {
      alert('Please enter a valid amount');
      return;
    }

    try {
      setDistributing(true);

      // In real implementation, this would call the contract
      console.log('Distributing royalties:', numAmount, 'STX for track', track.id);

      // Simulate API call delay
      await new Promise(resolve => setTimeout(resolve, 2000));

      onDistributionComplete();
      alert('Royalties distributed successfully! 🎉');
    } catch (error) {
      console.error('Error distributing royalties:', error);
      alert('Error distributing royalties. Please try again.');
    } finally {
      setDistributing(false);
    }
  };

  return (
    <div className="royalty-distribution">
      <div style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        marginBottom: '1.5rem'
      }}>
        <h3 style={{ color: '#00d4aa', margin: 0 }}>
          💰 Distribute Royalties - {track.title}
        </h3>
        <button
          onClick={onClose}
          style={{
            background: 'transparent',
            border: '1px solid #666',
            color: '#ccc',
            padding: '0.5rem',
            borderRadius: '4px',
            cursor: 'pointer'
          }}
        >
          ✕
        </button>
      </div>

      <div style={{ marginBottom: '2rem' }}>
        <div className="form-group">
          <label htmlFor="amount">Royalty Amount (STX)</label>
          <input
            id="amount"
            type="number"
            value={amount}
            onChange={(e) => handleAmountChange(e.target.value)}
            placeholder="Enter amount to distribute"
            min="1"
            step="1"
          />
        </div>
      </div>

      {preview && (
        <div>
          <h4 style={{ color: '#00d4aa', marginBottom: '1rem' }}>Distribution Preview</h4>

          <div style={{
            background: '#2a2a2a',
            padding: '1rem',
            borderRadius: '8px',
            marginBottom: '1rem'
          }}>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              marginBottom: '0.5rem'
            }}>
              <span>Platform Fee (2.5%):</span>
              <span style={{ color: '#ff9800' }}>{preview.platformFee} STX</span>
            </div>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              fontWeight: 'bold'
            }}>
              <span>Distributable Amount:</span>
              <span style={{ color: '#00d4aa' }}>{preview.distributableAmount} STX</span>
            </div>
          </div>

          <div>
            <h5 style={{ color: '#00d4aa', marginBottom: '1rem' }}>Contributor Distributions</h5>
            {preview.distributions.map((dist: any, index: number) => (
              <div key={index} style={{
                display: 'flex',
                justifyContent: 'space-between',
                alignItems: 'center',
                padding: '0.75rem',
                background: '#2a2a2a',
                borderRadius: '6px',
                marginBottom: '0.5rem'
              }}>
                <div>
                  <div style={{ fontWeight: 'bold' }}>{dist.role}</div>
                  <div style={{ fontSize: '0.8rem', color: '#888' }}>
                    {dist.address.slice(0, 10)}... ({dist.percentage}%)
                  </div>
                </div>
                <div style={{
                  fontSize: '1.1rem',
                  fontWeight: 'bold',
                  color: '#00d4aa'
                }}>
                  {dist.amount} STX
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      <div style={{
        display: 'flex',
        gap: '1rem',
        marginTop: '2rem'
      }}>
        <button
          onClick={onClose}
          className="btn-secondary"
          style={{ flex: 1 }}
        >
          Cancel
        </button>
        <button
          onClick={handleDistribute}
          disabled={distributing || !preview}
          className="btn-primary"
          style={{
            flex: 2,
            opacity: (!preview || distributing) ? 0.6 : 1
          }}
        >
          {distributing ? '💰 Distributing...' : '💸 Distribute Royalties'}
        </button>
      </div>

      <div style={{
        marginTop: '1rem',
        padding: '1rem',
        background: '#1a1a1a',
        borderRadius: '6px',
        fontSize: '0.9rem',
        color: '#888'
      }}>
        <strong>Note:</strong> Once distributed, royalties are sent directly to contributors' wallets
        via smart contract automation. Platform fees support ongoing development and maintenance.
      </div>
    </div>
  );
};
