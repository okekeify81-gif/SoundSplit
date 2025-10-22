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
(define-constant err-contract-paused (err u111))
(define-constant err-rate-limit-exceeded (err u112))
(define-constant err-overflow (err u113))
(define-constant err-underflow (err u114))
(define-constant err-invalid-input (err u115))

(define-constant max-contributors u10)
(define-constant dispute-voting-period u1008) ;; ~1 week in blocks
(define-constant min-expert-votes u3)
(define-constant percentage-precision u10000) ;; 100.00% = 10000
(define-constant rate-limit-blocks u10) ;; Minimum blocks between operations
(define-constant max-operations-per-block u5)

;; data vars
(define-data-var next-track-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% = 250/10000
(define-data-var contract-paused bool false)

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

;; NEW: Rate limiting maps
(define-map last-operation-block principal uint)
(define-map operations-per-block {user: principal, block: uint} uint)

;; Security helper functions

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (asserts! (>= result a) err-overflow)
    (ok result)))

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b)
    (ok (- a b))
    (err err-underflow)))

(define-private (safe-mul (a uint) (b uint))
  (let ((result (* a b)))
    (asserts! (or (is-eq b u0) (is-eq (/ result b) a)) err-overflow)
    (ok result)))

(define-private (check-rate-limit (user principal))
  (let (
    (current-block stacks-block-height)
    (last-block (default-to u0 (map-get? last-operation-block user)))
    (ops-count (default-to u0 (map-get? operations-per-block {user: user, block: current-block})))
  )
    (asserts! 
      (or 
        (>= (- current-block last-block) rate-limit-blocks)
        (< ops-count max-operations-per-block)
      )
      err-rate-limit-exceeded
    )
    (map-set last-operation-block user current-block)
    (map-set operations-per-block {user: user, block: current-block} (+ ops-count u1))
    (ok true)))

(define-private (validate-string-not-empty-utf8 (str (string-utf8 256)))
  (if (> (len str) u0)
    (ok true)
    err-invalid-input))

;; public functions

;; Pause contract (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-invalid-input)
    (var-set contract-paused true)
    (ok true)))

;; Unpause contract (owner only)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get contract-paused) err-invalid-input)
    (var-set contract-paused false)
    (ok true)))

(define-public (register-track 
  (title (string-utf8 256))
  (artist (string-utf8 256))
  (audio-hash (buff 32))
  (metadata-uri (string-utf8 256)))
  (let ((track-id (var-get next-track-id)))
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (try! (check-rate-limit tx-sender))
    (try! (validate-string-not-empty-utf8 title))
    (try! (validate-string-not-empty-utf8 artist))
    (try! (validate-string-not-empty-utf8 metadata-uri))
    (map-set tracks track-id {
      title: title,
      artist: artist,
      uploader: tx-sender,
      audio-hash: audio-hash,
      metadata-uri: metadata-uri,
      total-earnings: u0,
      is-locked: false,
      created-at: stacks-block-height
    })
    (var-set next-track-id (unwrap! (safe-add track-id u1) err-overflow))
    (unwrap! (update-user-profile tx-sender) err-unauthorized)
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
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (try! (check-rate-limit tx-sender))
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (asserts! (not (get is-locked track)) err-track-locked)
    (asserts! (<= contribution-percentage percentage-precision) err-invalid-percentage)
    (asserts! (> contribution-percentage u0) err-invalid-percentage)
    (asserts! (> (len role) u0) err-invalid-input)
    (asserts! (not (is-eq contributor tx-sender)) err-invalid-input)
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
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (asserts! (not (get verified contribution)) err-already-exists)
    (map-set track-contributors
      {track-id: track-id, contributor: contributor}
      (merge contribution {verified: true})
    )
    (ok true)
  )
)

(define-public (lock-track (track-id uint))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found)))
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-eq (get uploader track) tx-sender) err-unauthorized)
    (asserts! (not (get is-locked track)) err-track-locked)
    (asserts! (>= (get-total-contributions track-id) percentage-precision) err-invalid-percentage)
    
    (map-set tracks track-id (merge track {is-locked: true}))
    (ok true)
  )
)

(define-public (distribute-royalties (track-id uint) (amount uint))
  (let ((track (unwrap! (map-get? tracks track-id) err-not-found)))
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (try! (check-rate-limit tx-sender))
    (asserts! (get is-locked track) err-track-locked)
    (asserts! (> amount u0) err-invalid-input)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    
    ;; Transfer first (reentrancy protection)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (try! (distribute-to-contributors track-id amount))
    
    ;; Update state after external calls
    (map-set tracks track-id 
      (merge track {total-earnings: (unwrap! (safe-add (get total-earnings track) amount) err-overflow)})
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
    
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (try! (check-rate-limit tx-sender))
    (asserts! (get is-locked track) err-track-locked)
    (asserts! (is-some (map-get? track-contributors {track-id: track-id, contributor: tx-sender})) err-unauthorized)
    (asserts! (> (len reason) u0) err-invalid-input)
    (asserts! (> (len proposed-changes) u0) err-invalid-input)
    
    (map-set disputes dispute-id {
      track-id: track-id,
      disputer: tx-sender,
      reason: reason,
      proposed-changes: proposed-changes,
      status: "active",
      created-at: stacks-block-height,
      voting-end: (unwrap! (safe-add stacks-block-height dispute-voting-period) err-overflow),
      votes-for: u0,
      votes-against: u0
    })
    
    (var-set next-dispute-id (unwrap! (safe-add dispute-id u1) err-overflow))
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote bool))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-not-found)))
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (default-to false (map-get? expert-panel tx-sender)) err-unauthorized)
    (asserts! (is-eq (get status dispute) "active") err-invalid-dispute)
    (asserts! (<= stacks-block-height (get voting-end dispute)) err-voting-period-ended)
    (asserts! (is-none (map-get? dispute-votes {dispute-id: dispute-id, expert: tx-sender})) err-already-voted)
    
    (map-set dispute-votes {dispute-id: dispute-id, expert: tx-sender} vote)
    
    (let ((new-votes-for (if vote (unwrap! (safe-add (get votes-for dispute) u1) err-overflow) (get votes-for dispute)))
          (new-votes-against (if vote (get votes-against dispute) (unwrap! (safe-add (get votes-against dispute) u1) err-overflow))))
      
      (map-set disputes dispute-id
        (merge dispute {
          votes-for: new-votes-for,
          votes-against: new-votes-against
        })
      )
      
      ;; Auto-resolve if minimum votes reached
      (if (>= (unwrap! (safe-add new-votes-for new-votes-against) err-overflow) min-expert-votes)
        (begin
          (try! (resolve-dispute dispute-id))
          (ok true))
        (ok true)
      )
    )
  )
)

(define-public (add-expert (expert principal))
  (begin
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq expert tx-sender)) err-invalid-input)
    (asserts! (not (default-to false (map-get? expert-panel expert))) err-already-exists)
    (map-set expert-panel expert true)
    (ok true)
  )
)

(define-public (remove-expert (expert principal))
  (begin
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (default-to false (map-get? expert-panel expert)) err-not-found)
    (map-delete expert-panel expert)
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (not (var-get contract-paused)) err-contract-paused)
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
  (fold get-contribution-sum (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) u0))

(define-read-only (is-contract-paused)
  (var-get contract-paused))

(define-read-only (get-last-operation-block (user principal))
  (default-to u0 (map-get? last-operation-block user)))

(define-read-only (get-operations-count (user principal) (block uint))
  (default-to u0 (map-get? operations-per-block {user: user, block: block})))

;; private functions

(define-private (get-contribution-sum (index uint) (total uint))
  ;; This is a simplified version - in practice, you'd iterate through actual contributors
  total
)

(define-private (distribute-to-contributors (track-id uint) (amount uint))
  (let ((platform-cut (/ (unwrap! (safe-mul amount (var-get platform-fee)) err-overflow) percentage-precision))
        (remaining-amount (unwrap! (safe-sub amount platform-cut) err-underflow)))
    
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
      (merge profile {total-tracks: (unwrap! (safe-add (get total-tracks profile) u1) err-overflow)})
    )
    (ok true)
  )
)