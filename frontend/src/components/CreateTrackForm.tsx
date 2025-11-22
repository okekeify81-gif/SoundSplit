import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';

interface CreateTrackFormProps {
  userSession: UserSession;
  onTrackCreated: () => void;
}

export const CreateTrackForm: React.FC<CreateTrackFormProps> = ({
  userSession,
  onTrackCreated
}) => {
  const [title, setTitle] = useState('');
  const [artist, setArtist] = useState('');
  const [metadataUri, setMetadataUri] = useState('');
  const [audioFile, setAudioFile] = useState<File | null>(null);
  const [submitting, setSubmitting] = useState(false);

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0]) {
      setAudioFile(e.target.files[0]);
    }
  };

  const generateAudioHash = async (file: File): Promise<string> => {
    // In a real implementation, this would compute the actual hash
    // For demo purposes, return a mock hash
    return 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3';
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!title.trim() || !artist.trim() || !audioFile) {
      alert('Please fill in all required fields and select an audio file');
      return;
    }

    try {
      setSubmitting(true);

      // Generate audio hash (in real implementation, this would be done on-chain)
      const audioHash = await generateAudioHash(audioFile);

      // In a real implementation, this would call the contract
      // For now, just simulate success
      console.log('Creating track:', {
        title,
        artist,
        audioHash,
        metadataUri: metadataUri || `ipfs://Qm${Date.now()}`
      });

      // Reset form
      setTitle('');
      setArtist('');
      setMetadataUri('');
      setAudioFile(null);

      onTrackCreated();
      alert('Track registered successfully! 🎵');
    } catch (error) {
      console.error('Error creating track:', error);
      alert('Error registering track. Please try again.');
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="track-card" style={{ marginBottom: '2rem' }}>
      <h3 style={{ color: '#00d4aa' }}>🎼 Register New Track</h3>
      <p style={{ color: '#ccc', marginBottom: '1.5rem' }}>
        Add your music to the SoundSplit platform for algorithmic royalty distribution
      </p>

      <form onSubmit={handleSubmit}>
        <div className="form-group">
          <label htmlFor="title">Track Title *</label>
          <input
            id="title"
            type="text"
            value={title}
            onChange={(e) => setTitle(e.target.value)}
            placeholder="Enter track title"
            maxLength={256}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="artist">Artist Name *</label>
          <input
            id="artist"
            type="text"
            value={artist}
            onChange={(e) => setArtist(e.target.value)}
            placeholder="Enter artist name"
            maxLength={256}
            required
          />
        </div>

        <div className="form-group">
          <label htmlFor="metadataUri">Metadata URI (Optional)</label>
          <input
            id="metadataUri"
            type="url"
            value={metadataUri}
            onChange={(e) => setMetadataUri(e.target.value)}
            placeholder="ipfs://Qm... or https://..."
          />
          <small style={{ color: '#888' }}>
            Link to additional metadata (lyrics, artwork, etc.)
          </small>
        </div>

        <div className="form-group">
          <label htmlFor="audioFile">Audio File *</label>
          <input
            id="audioFile"
            type="file"
            accept="audio/*"
            onChange={handleFileChange}
            style={{
              padding: '0.5rem',
              border: '1px solid #444',
              borderRadius: '8px',
              background: '#1a1a1a',
              color: '#ccc'
            }}
            required
          />
          {audioFile && (
            <div style={{
              marginTop: '0.5rem',
              padding: '0.5rem',
              background: '#2a2a2a',
              borderRadius: '4px',
              fontSize: '0.9rem',
              color: '#00d4aa'
            }}>
              Selected: {audioFile.name} ({(audioFile.size / 1024 / 1024).toFixed(2)} MB)
            </div>
          )}
        </div>

        <div style={{
          background: '#1a1a1a',
          padding: '1rem',
          borderRadius: '8px',
          marginBottom: '1.5rem',
          border: '1px solid #333'
        }}>
          <h4 style={{ color: '#00d4aa', marginTop: 0 }}>⚠️ Important Notes</h4>
          <ul style={{ color: '#ccc', margin: 0, paddingLeft: '1.5rem' }}>
            <li>Your audio file will be hashed for uniqueness verification</li>
            <li>You can add contributors after registering the track</li>
            <li>Track must be "locked" before royalty distribution can begin</li>
            <li>All contributor percentages must sum to 100% before locking</li>
          </ul>
        </div>

        <button
          type="submit"
          disabled={submitting}
          style={{
            background: 'linear-gradient(45deg, #00d4aa, #00b8a0)',
            color: '#000',
            border: 'none',
            padding: '1rem 2rem',
            borderRadius: '8px',
            fontSize: '1.1rem',
            fontWeight: 'bold',
            cursor: submitting ? 'not-allowed' : 'pointer',
            width: '100%',
            textTransform: 'uppercase',
            letterSpacing: '1px'
          }}
        >
          {submitting ? '🎵 Registering Track...' : '🎼 Register Track'}
        </button>
      </form>
    </div>
  );
};
