# SoundSplit Frontend

A modern React frontend for the SoundSplit algorithmic music rights distribution platform built on Stacks blockchain.

## Features

- **🎵 Music Catalog Management**: Register tracks, add contributors, and manage metadata
- **💰 Royalty Distribution**: Automated fair distribution of music royalties
- **👥 Contributor Management**: Add, verify, and manage track contributors with percentage splits
- **🔒 Track Locking**: Secure tracks before enabling royalty distribution
- **📊 Real-time Analytics**: Track earnings, contributor status, and platform statistics
- **🎼 Audio Upload**: Secure audio file handling with hash verification
- **🔐 Wallet Integration**: Seamless Stacks wallet connection and transaction management

## Tech Stack

- **React 18** - Modern UI framework with hooks
- **TypeScript** - Type-safe development
- **Stacks.js** - Blockchain integration and wallet connectivity
- **Vite** - Fast build tool and development server
- **pnpm** - Efficient package management

## Getting Started

1. **Install dependencies:**
   ```bash
   cd frontend
   pnpm install
   ```

2. **Start development server:**
   ```bash
   pnpm dev
   ```

3. **Open your browser:**
   Navigate to `http://localhost:3000`

## Project Structure

```
frontend/
├── src/
│   ├── components/
│   │   ├── Header.tsx           # App header with branding
│   │   ├── WalletConnect.tsx    # Stacks wallet connection
│   │   ├── TrackList.tsx        # Main track management interface
│   │   ├── TrackCard.tsx        # Individual track display
│   │   ├── CreateTrackForm.tsx  # Track registration form
│   │   └── RoyaltyDistribution.tsx # Royalty distribution interface
│   ├── App.tsx                  # Main application component
│   ├── App.css                  # Global styles and animations
│   ├── main.tsx                 # Application entry point
│   └── index.css                # Base styles
├── index.html                   # HTML template
├── vite.config.ts              # Vite configuration
├── tsconfig.json               # TypeScript configuration
└── package.json                # Dependencies and scripts
```

## Key Components

### Track Management
- **CreateTrackForm**: Register new tracks with metadata and audio files
- **TrackCard**: Display track information, contributors, and earnings
- **TrackList**: Overview of user's music catalog with statistics

### Royalty System
- **RoyaltyDistribution**: Automated fair distribution to contributors
- **Contributor Management**: Add and verify track contributors
- **Batch Operations**: Efficient bulk contributor management

### User Interface
- **WalletConnect**: Secure Stacks wallet integration
- **Header**: Branded application header with navigation
- **Responsive Design**: Mobile-first approach with modern styling

## Smart Contract Integration

The frontend integrates with the SoundSplit Clarity smart contract for:

- Track registration and metadata storage
- Contributor management and verification
- Royalty distribution calculations
- Dispute resolution mechanisms
- Expert panel voting

## Security Features

- **Reentrancy Protection**: Guards against reentrancy attacks
- **Rate Limiting**: Prevents spam and abuse
- **Input Validation**: Comprehensive client and contract-side validation
- **Access Control**: Role-based permissions and authorization

## Building for Production

```bash
pnpm build
```

The built files will be in the `dist/` directory, ready for deployment to any static hosting service.

## Environment Setup

Make sure to update the contract address in the components:

```typescript
const contractAddress = 'YOUR_DEPLOYED_CONTRACT_ADDRESS';
```

## Contributing

1. Follow the existing code style and TypeScript patterns
2. Add comprehensive error handling
3. Include user feedback for all async operations
4. Test wallet interactions thoroughly
5. Ensure responsive design across devices

## License

This project is part of the SoundSplit platform for fair music royalty distribution.
