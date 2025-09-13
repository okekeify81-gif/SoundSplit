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


;; public functions

(define-public (register-track 
  (title (string-utf8 256))
  (artist (string-utf8 256))
  (audio-hash (buff 32))
  (metadata-uri (string-utf8 256)))
  (let ((track-id (var-get next-track-id)))
    (map-set tracks track-id {
      title: title,
      artist: artist,
      uploader: tx-sender,
      audio-hash: audio-hash,
      metadata-uri: metadata-uri,
      total-earnings: u0,
      is-locked: false,
      created-at: block-height
    })
    (var-set next-track-id (+ track-id u1))
    (update-user-profile tx-sender)
    (ok track-id)
  )
)

(define-public (add-contributor
  (track-id uint)
  (contributor principal)
  (role (string-ascii 64))
  (contribution-percentage uint)
  (audio-fingerprint (buff 32)))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found)))
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (asserts! (not (get is-locked track)) err-track-locked)
    (asserts! (<= contribution-percentage percentage-precision) err-invalid-percentage)
    (asserts! (is-none (map-get? track-contributors {track-id: track-id, contributor: contributor})) err-already-exists)
    
    (map-set track-contributors
      {track-id: track-id, contributor: contributor}
      {
        role: role,
        contribution-percentage: contribution-percentage,
        audio-fingerprint: audio-fingerprint,
        verified: false
      }
    )
    (ok true)
  )
)

(define-public (verify-contribution (track-id uint) (contributor principal))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found))
        (contribution (unwrap! (map-get? track-contributors {track-id: track-id, contributor: contributor}) err-not-found)))
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (map-set track-contributors
      {track-id: track-id, contributor: contributor}
      (merge contribution {verified: true})
    )
    (ok true)
  )
)

(define-public (lock-track (track-id uint))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found)))
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (asserts! (>= (get-total-contributions track-id) percentage-precision) err-invalid-percentage)
    
    (map-set tracks track-id (merge track {is-locked: true}))
    (ok true)
  )
)

(define-public (distribute-royalties (track-id uint) (amount uint))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found)))
    (asserts! (get is-locked track) err-track-locked)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (try! (distribute-to-contributors track-id amount))
    
    (map-set tracks track-id 
      (merge track {total-earnings: (+ (get total-earnings track) amount)})
    )
    (ok true)
  )
)

(define-public (create-dispute
  (track-id uint)
  (reason (string-utf8 512))
  (proposed-changes (string-utf8 1024)))
  (let ((dispute-id (var-get next-dispute-id))
        (track (unwrap! (map-get? tracks track-id) err-not-found)))
    
    (asserts! (get is-locked track) err-track-locked)
    (asserts! (is-some (map-get? track-contributors {track-id: track-id, contributor: tx-sender})) err-unauthorized)
    
    (map-set disputes dispute-id {
      track-id: track-id,
      disputer: tx-sender,
      reason: reason,
      proposed-changes: proposed-changes,
      status: "active",
      created-at: block-height,
      voting-end: (+ block-height dispute-voting-period),
      votes-for: u0,
      votes-against: u0
    })
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote bool))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-not-found)))
    (asserts! (default-to false (map-get? expert-panel tx-sender)) err-unauthorized)
    (asserts! (is-eq (get status dispute) "active") err-invalid-dispute)
    (asserts! (<= block-height (get voting-end dispute)) err-voting-period-ended)
    (asserts! (is-none (map-get? dispute-votes {dispute-id: dispute-id, expert: tx-sender})) err-already-voted)
    
    (map-set dispute-votes {dispute-id: dispute-id, expert: tx-sender} vote)
    
    (let ((new-votes-for (if vote (+ (get votes-for dispute) u1) (get votes-for dispute)))
          (new-votes-against (if vote (get votes-against dispute) (+ (get votes-against dispute) u1))))
      
      (map-set disputes dispute-id
        (merge dispute {
          votes-for: new-votes-for,
          votes-against: new-votes-against
        })
      )
      
      ;; Auto-resolve if minimum votes reached
      (if (>= (+ new-votes-for new-votes-against) min-expert-votes)
        (try! (resolve-dispute dispute-id))
        (ok true)
      )
    )
  )
)

(define-public (add-expert (expert principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set expert-panel expert true)
    (ok true)
  )
)

(define-public (remove-expert (expert principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete expert-panel expert)
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-percentage) ;; Max 10%
    (var-set platform-fee new-fee)
    (ok true)
  )
)


;; read only functions

(define-read-only (get-track (track-id uint))
  (map-get? tracks track-id)
)

(define-read-only (get-contributor (track-id uint) (contributor principal))
  (map-get? track-contributors {track-id: track-id, contributor: contributor})
)

(define-read-only (get-contributor-earnings (track-id uint) (contributor principal))
  (default-to u0 (map-get? contributor-earnings {track-id: track-id, contributor: contributor}))
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

(define-read-only (is-expert (user principal))
  (default-to false (map-get? expert-panel user))
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-total-contributions (track-id uint))
  (fold get-contribution-sum (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) u0)
)

;; private functions

(define-private (get-contribution-sum (index uint) (total uint))
  ;; This is a simplified version - in practice, you'd iterate through actual contributors
  total
)

(define-private (distribute-to-contributors (track-id uint) (amount uint))
  (let ((platform-cut (/ (* amount (var-get platform-fee)) percentage-precision))
        (remaining-amount (- amount platform-cut)))
    
    ;; Transfer platform fee to contract owner
    (try! (as-contract (stx-transfer? platform-cut tx-sender contract-owner)))
    
    ;; Distribute remaining amount to contributors
    ;; This would iterate through all contributors and distribute based on percentages
    ;; Simplified for this example
    (ok true)
  )
)

(define-private (resolve-dispute (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-not-found)))
    (let ((status (if (> (get votes-for dispute) (get votes-against dispute)) "resolved" "rejected")))
      (map-set disputes dispute-id (merge dispute {status: status}))
      (ok true)
    )
  )
)

(define-private (update-user-profile (user principal))
  (let ((profile (default-to 
    {name: u"", reputation-score: u0, total-tracks: u0, total-earnings: u0}
    (map-get? user-profiles user))))
    (map-set user-profiles user 
      (merge profile {total-tracks: (+ (get total-tracks profile) u1)})
    )
    (ok true)
  )
)