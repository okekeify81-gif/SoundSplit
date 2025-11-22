import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { StacksMainnet } from '@stacks/network';
import { callReadOnlyFunction, cvToJSON } from '@stacks/transactions';
import { TrackCard } from './TrackCard';
import { CreateTrackForm } from './CreateTrackForm';
import { RoyaltyDistribution } from './RoyaltyDistribution';

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

interface TrackListProps {
  userSession: UserSession;
}

export const TrackList: React.FC<TrackListProps> = ({ userSession }) => {
  const [tracks, setTracks] = useState<Track[]>([]);
  const [loading, setLoading] = useState(true);
  const [showCreateForm, setShowCreateForm] = useState(false);
  const [selectedTrack, setSelectedTrack] = useState<Track | null>(null);

  const network = new StacksMainnet();
  const contractAddress = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'; // Replace with actual contract address
  const contractName = 'SoundSplitcontract';

  useEffect(() => {
    loadTracks();
  }, []);

  const loadTracks = async () => {
    try {
      setLoading(true);
      // In a real implementation, you'd fetch tracks from the contract
      // For now, showing empty state
      setTracks([]);
    } catch (error) {
      console.error('Error loading tracks:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleTrackCreated = () => {
    setShowCreateForm(false);
    loadTracks();
  };

  const handleDistributeRoyalties = (track: Track) => {
    setSelectedTrack(track);
  };

  if (loading) {
    return (
      <div style={{
        textAlign: 'center',
        padding: '3rem',
        color: '#00d4aa'
      }}>
        <div style={{ fontSize: '2rem', marginBottom: '1rem' }}>🎵</div>
        Loading your music catalog...
      </div>
    );
  }

  return (
    <div>
      <div style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        marginBottom: '2rem',
        flexWrap: 'wrap',
        gap: '1rem'
      }}>
        <div>
          <h2 style={{ color: '#00d4aa', margin: 0 }}>Your Music Catalog</h2>
          <p style={{ color: '#ccc', margin: '0.5rem 0' }}>
            Manage your tracks, contributors, and royalty distributions
          </p>
        </div>
        <button
          onClick={() => setShowCreateForm(!showCreateForm)}
          className="btn-music"
          style={{
            padding: '0.75rem 1.5rem',
            borderRadius: '8px',
            fontWeight: 'bold',
            cursor: 'pointer',
            textTransform: 'uppercase',
            letterSpacing: '1px'
          }}
        >
          {showCreateForm ? 'Cancel' : '🎼 Add New Track'}
        </button>
      </div>

      {showCreateForm && (
        <CreateTrackForm
          userSession={userSession}
          onTrackCreated={handleTrackCreated}
        />
      )}

      {selectedTrack && (
        <RoyaltyDistribution
          track={selectedTrack}
          userSession={userSession}
          onClose={() => setSelectedTrack(null)}
          onDistributionComplete={() => {
            setSelectedTrack(null);
            loadTracks();
          }}
        />
      )}

      <div>
        {tracks.length === 0 ? (
          <div style={{
            textAlign: 'center',
            padding: '4rem 2rem',
            background: 'linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)',
            borderRadius: '16px',
            border: '1px solid #333',
            color: '#ccc'
          }}>
            <div style={{ fontSize: '4rem', marginBottom: '1rem' }}>🎼</div>
            <h3 style={{ color: '#00d4aa', marginBottom: '1rem' }}>No tracks yet</h3>
            <p style={{ marginBottom: '2rem' }}>
              Start building your music catalog by registering your first track.
              Add contributors, set royalty percentages, and begin earning from algorithmic distributions.
            </p>
            <button
              onClick={() => setShowCreateForm(true)}
              className="btn-primary"
              style={{
                padding: '1rem 2rem',
                borderRadius: '8px',
                fontSize: '1.1rem'
              }}
            >
              Create Your First Track
            </button>
          </div>
        ) : (
          tracks.map((track) => (
            <TrackCard
              key={track.id}
              track={track}
              userSession={userSession}
              onDistributeRoyalties={() => handleDistributeRoyalties(track)}
              onUpdate={() => loadTracks()}
            />
          ))
        )}
      </div>

      {/* Statistics Dashboard */}
      <div style={{
        marginTop: '3rem',
        padding: '2rem',
        background: 'linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)',
        borderRadius: '12px',
        border: '1px solid #00d4aa'
      }}>
        <h3 style={{ color: '#00d4aa', marginTop: 0 }}>Platform Statistics</h3>
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
          gap: '1rem',
          marginTop: '1rem'
        }}>
          <div style={{
            background: '#2a2a2a',
            padding: '1rem',
            borderRadius: '8px',
            textAlign: 'center'
          }}>
            <div style={{ fontSize: '2rem', color: '#00d4aa' }}>📊</div>
            <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#00d4aa' }}>
              {tracks.length}
            </div>
            <div style={{ color: '#ccc' }}>Total Tracks</div>
          </div>
          <div style={{
            background: '#2a2a2a',
            padding: '1rem',
            borderRadius: '8px',
            textAlign: 'center'
          }}>
            <div style={{ fontSize: '2rem', color: '#00d4aa' }}>💰</div>
            <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#00d4aa' }}>
              {tracks.reduce((sum, track) => sum + track.totalEarnings, 0)} STX
            </div>
            <div style={{ color: '#ccc' }}>Total Earnings</div>
          </div>
          <div style={{
            background: '#2a2a2a',
            padding: '1rem',
            borderRadius: '8px',
            textAlign: 'center'
          }}>
            <div style={{ fontSize: '2rem', color: '#00d4aa' }}>🔒</div>
            <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#00d4aa' }}>
              {tracks.filter(track => track.isLocked).length}
            </div>
            <div style={{ color: '#ccc' }}>Locked Tracks</div>
          </div>
        </div>
      </div>
    </div>
  );
};
