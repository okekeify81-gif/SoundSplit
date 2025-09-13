;; title: SoundSplit - Algorithmic Music Rights Distribution
;; version: 1.0.0
;; summary: Automatically distribute music royalties to contributors using AI audio analysis
;; description: Smart contract system for fair and transparent music royalty distribution
;;              based on algorithmic analysis of contributor participation

;; traits
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
  )
)

;; token definitions
(define-fungible-token soundsplit-token)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-percentage (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-track-locked (err u106))
(define-constant err-dispute-active (err u107))
(define-constant err-invalid-dispute (err u108))
(define-constant err-voting-period-ended (err u109))
(define-constant err-already-voted (err u110))

(define-constant max-contributors u10)
(define-constant dispute-voting-period u1008) ;; ~1 week in blocks
(define-constant min-expert-votes u3)
(define-constant percentage-precision u10000) ;; 100.00% = 10000

;; data vars
(define-data-var next-track-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% = 250/10000

;; data maps
(define-map tracks
  uint
  {
    title: (string-utf8 256),
    artist: (string-utf8 256),
    uploader: principal,
    audio-hash: (buff 32),
    metadata-uri: (string-utf8 256),
    total-earnings: uint,
    is-locked: bool,
    created-at: uint
  }
)

(define-map track-contributors
  {track-id: uint, contributor: principal}
  {
    role: (string-ascii 64),
    contribution-percentage: uint,
    audio-fingerprint: (buff 32),
    verified: bool
  }
)

(define-map contributor-earnings
  {track-id: uint, contributor: principal}
  uint
)

(define-map disputes
  uint
  {
    track-id: uint,
    disputer: principal,
    reason: (string-utf8 512),
    proposed-changes: (string-utf8 1024),
    status: (string-ascii 32), ;; "active", "resolved", "rejected"
    created-at: uint,
    voting-end: uint,
    votes-for: uint,
    votes-against: uint
  }
)

(define-map expert-panel principal bool)

(define-map dispute-votes
  {dispute-id: uint, expert: principal}
  bool ;; true = for, false = against
)

(define-map user-profiles
  principal
  {
    name: (string-utf8 128),
    reputation-score: uint,
    total-tracks: uint,
    total-earnings: uint
  }
)
