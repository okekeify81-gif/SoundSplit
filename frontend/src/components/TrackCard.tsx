import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';

interface Contributor {
  role: string;
  contributionPercentage: number;
  verified: boolean;
}

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

interface TrackCardProps {
  track: Track;
  userSession: UserSession;
  onDistributeRoyalties: () => void;
  onUpdate: () => void;
}

export const TrackCard: React.FC<TrackCardProps> = ({
  track,
  userSession,
  onDistributeRoyalties,
  onUpdate
}) => {
  const [showContributors, setShowContributors] = useState(false);
  const [contributors, setContributors] = useState<Contributor[]>([]);
  const [loading, setLoading] = useState(false);

  const loadContributors = async () => {
    // In a real implementation, this would fetch contributors from the contract
    setContributors([
      { role: 'Vocalist', contributionPercentage: 50, verified: true },
      { role: 'Producer', contributionPercentage: 30, verified: true },
      { role: 'Mixer', contributionPercentage: 20, verified: false }
    ]);
  };

  const handleToggleContributors = () => {
    if (!showContributors && contributors.length === 0) {
      loadContributors();
    }
    setShowContributors(!showContributors);
  };

  return (
    <div className="track-card">
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start' }}>
        <div style={{ flex: 1 }}>
          <h3>{track.title}</h3>
          <p style={{ color: '#ccc', margin: '0.5rem 0' }}>
            by {track.artist}
          </p>
          <div className="track-meta">
            <span>Uploaded by: {track.uploader.slice(0, 10)}...</span>
            <span className="earnings-display">
              {track.totalEarnings} STX earned
            </span>
          </div>
        </div>
        <span className={`status-badge ${track.isLocked ? 'status-locked' : 'status-unlocked'}`}>
          {track.isLocked ? '🔒 Locked' : '🔓 Unlocked'}
        </span>
      </div>

      <div style={{
        display: 'flex',
        gap: '1rem',
        marginTop: '1.5rem',
        flexWrap: 'wrap'
      }}>
        <button
          onClick={handleToggleContributors}
          className="btn-secondary"
          style={{ padding: '0.5rem 1rem' }}
        >
          {showContributors ? 'Hide' : 'Show'} Contributors ({contributors.length})
        </button>

        {track.isLocked && (
          <button
            onClick={onDistributeRoyalties}
            className="btn-primary"
            style={{ padding: '0.5rem 1rem' }}
          >
            💰 Distribute Royalties
          </button>
        )}

        {!track.isLocked && (
          <button
            className="btn-music"
            style={{ padding: '0.5rem 1rem' }}
            disabled={loading}
          >
            🔒 Lock Track
          </button>
        )}
      </div>

      {showContributors && (
        <div className="contributors-list">
          <h4 style={{ color: '#00d4aa', marginBottom: '1rem' }}>Contributors</h4>
          {contributors.map((contributor, index) => (
            <div key={index} className="contributor-item">
              <div>
                <strong>{contributor.role}</strong>
                <span style={{ color: '#ccc', marginLeft: '1rem' }}>
                  {contributor.contributionPercentage}% share
                </span>
              </div>
              <div>
                {contributor.verified ? (
                  <span style={{ color: '#4caf50' }}>✓ Verified</span>
                ) : (
                  <span style={{ color: '#ff9800' }}>⚠ Pending</span>
                )}
              </div>
            </div>
          ))}

          {!track.isLocked && (
            <button
              style={{
                width: '100%',
                marginTop: '1rem',
                padding: '0.75rem',
                background: '#333',
                color: '#00d4aa',
                border: '1px dashed #00d4aa',
                borderRadius: '8px',
                cursor: 'pointer'
              }}
            >
              + Add Contributor
            </button>
          )}
        </div>
      )}

      <div style={{
        marginTop: '1rem',
        padding: '1rem',
        background: '#1a1a1a',
        borderRadius: '8px',
        fontSize: '0.9rem',
        color: '#888'
      }}>
        <div>Created: {new Date(track.createdAt * 1000).toLocaleDateString()}</div>
        <div>Track ID: #{track.id}</div>
        <div>Audio Hash: {track.audioHash.slice(0, 10)}...</div>
      </div>
    </div>
  );
};
