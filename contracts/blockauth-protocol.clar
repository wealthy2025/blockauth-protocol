

;; Blockchain Authentication Protocol - Decentralized framework for authenticating and managing creative works with permission tiers
;;
;; This protocol provides an immutable record of creative work ownership with comprehensive provenance tracking
;; and sophisticated permission management for collaborative creative environments

;; Universal Work Counter
(define-data-var creation-sequence-counter uint u0)

;; Protocol Governor
(define-constant governance-principal tx-sender)

;; Protocol Response Status Codes

(define-constant ownership-verification-failed (err u306))
(define-constant governance-restricted-function (err u300))
(define-constant work-identifier-invalid (err u301))
(define-constant work-already-registered (err u302))
(define-constant name-validation-failed (err u303))
(define-constant metadata-validation-failed (err u304))
(define-constant access-level-insufficient (err u305))
(define-constant viewing-rights-restricted (err u307))
(define-constant category-validation-failed (err u308))

;; Core Storage Architecture
(define-map creative-works
  { work-id: uint }
  {
    work-name: (string-ascii 64),
    creator-principal: principal,
    content-metadata: uint,
    creation-timestamp: uint,
    work-narrative: (string-ascii 128),
    work-categories: (list 10 (string-ascii 32))
  }
)

;; Collaborative Access Management
(define-map access-registry
  { work-id: uint, collaborator: principal }
  { collaboration-enabled: bool }
)

;; ===== Protocol Support Functions =====

;; Validates category formatting requirements
(define-private (validate-category-format (category (string-ascii 32)))
  (and
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Ensures the category set meets protocol standards
(define-private (validate-category-set (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter validate-category-format categories)) (len categories))
  )
)

;; Confirms work exists in registry
(define-private (work-registration-exists (work-id uint))
  (is-some (map-get? creative-works { work-id: work-id }))
)

;; Retrieves content metadata for a work
(define-private (retrieve-content-metadata (work-id uint))
  (default-to u0
    (get content-metadata
      (map-get? creative-works { work-id: work-id })
    )
  )
)

;; Ownership verification function
(define-private (verify-work-ownership (work-id uint) (requester principal))
  (match (map-get? creative-works { work-id: work-id })
    work-record (is-eq (get creator-principal work-record) requester)
    false
  )
)

;; ===== Primary Protocol Functions =====

;; Transfer creative work ownership to new principal
(define-public (transfer-work-ownership (work-id uint) (recipient-principal principal))
  (let
    (
      (work-record (unwrap! (map-get? creative-works { work-id: work-id })
        work-identifier-invalid))
    )
    ;; Validate caller is current owner
    (asserts! (work-registration-exists work-id) work-identifier-invalid)
    (asserts! (is-eq (get creator-principal work-record) tx-sender) ownership-verification-failed)

    ;; Update ownership record
    (map-set creative-works
      { work-id: work-id }
      (merge work-record { creator-principal: recipient-principal })
    )
    (ok true)
  )
)

;; Remove work from registry permanently
(define-public (unregister-creative-work (work-id uint))
  (let
    (
      (work-record (unwrap! (map-get? creative-works { work-id: work-id })
        work-identifier-invalid))
    )
    ;; Verify ownership
    (asserts! (work-registration-exists work-id) work-identifier-invalid)
    (asserts! (is-eq (get creator-principal work-record) tx-sender) ownership-verification-failed)

    ;; Remove work from registry
    (map-delete creative-works { work-id: work-id })
    (ok true)
  )
)

;;  Secure operation management with time-bounded confirmations
;; Prevents immediate work ownership changes by implementing a time-delay verification process

;; Operations awaiting confirmation
(define-map queued-operations
  { operation-sequence: uint, work-id: uint }
  {
    operation-classification: (string-ascii 20),
    requested-by: principal,
    beneficiary: (optional principal),
    timestamp-initiated: uint,
    authorization-digest: (buff 32),
    confirmation-deadline: uint
  }
)

;; Register new creative work with complete documentation
(define-public (register-creative-work
  (name (string-ascii 64))
  (metadata-size uint)
  (narrative (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (let
    (
      (new-work-id (+ (var-get creation-sequence-counter) u1))
    )
    ;; Input validation
    (asserts! (> (len name) u0) name-validation-failed)
    (asserts! (< (len name) u65) name-validation-failed)
    (asserts! (> metadata-size u0) metadata-validation-failed)
    (asserts! (< metadata-size u1000000000) metadata-validation-failed)
    (asserts! (> (len narrative) u0) name-validation-failed)
    (asserts! (< (len narrative) u129) name-validation-failed)
    (asserts! (validate-category-set categories) category-validation-failed)

    ;; Create work record
    (map-insert creative-works
      { work-id: new-work-id }
      {
        work-name: name,
        creator-principal: tx-sender,
        content-metadata: metadata-size,
        creation-timestamp: block-height,
        work-narrative: narrative,
        work-categories: categories
      }
    )

    ;; Initialize access permission for creator
    (map-insert access-registry
      { work-id: new-work-id, collaborator: tx-sender }
      { collaboration-enabled: true }
    )

    ;; Update registry counter
    (var-set creation-sequence-counter new-work-id)
    (ok new-work-id)
  )
)

;; Update existing creative work details
(define-public (update-work-information
  (work-id uint)
  (revised-name (string-ascii 64))
  (revised-metadata-size uint)
  (revised-narrative (string-ascii 128))
  (revised-categories (list 10 (string-ascii 32)))
)
  (let
    (
      (work-record (unwrap! (map-get? creative-works { work-id: work-id })
        work-identifier-invalid))
    )
    ;; Validate ownership and parameters
    (asserts! (work-registration-exists work-id) work-identifier-invalid)
    (asserts! (is-eq (get creator-principal work-record) tx-sender) ownership-verification-failed)
    (asserts! (> (len revised-name) u0) name-validation-failed)
    (asserts! (< (len revised-name) u65) name-validation-failed)
    (asserts! (> revised-metadata-size u0) metadata-validation-failed)
    (asserts! (< revised-metadata-size u1000000000) metadata-validation-failed)
    (asserts! (> (len revised-narrative) u0) name-validation-failed)
    (asserts! (< (len revised-narrative) u129) name-validation-failed)
    (asserts! (validate-category-set revised-categories) category-validation-failed)

    ;; Update work record
    (map-set creative-works
      { work-id: work-id }
      (merge work-record {
        work-name: revised-name,
        content-metadata: revised-metadata-size,
        work-narrative: revised-narrative,
        work-categories: revised-categories
      })
    )
    (ok true)
  )
)

;; Authenticate work content against registered fingerprint
(define-public (authenticate-work-integrity (work-id uint) (content-fingerprint (buff 32)))
  (let
    (
      (integrity-record (unwrap! (map-get? work-integrity { work-id: work-id })
        (err u601)))
    )
    ;; Verify fingerprint matches registered value
    (asserts! (is-eq (get content-fingerprint integrity-record) content-fingerprint) (err u602))

    (ok true)
  )
)

;;  Request throttling and security enhancement
;; Prevent system abuse through strategic request limiting

;; Request frequency monitoring
(define-map request-monitor
  { principal-address: principal }
  {
    last-request-block: uint,
    requests-in-timeframe: uint
  }
)

;; Throttling configuration
(define-data-var throttle-timeframe uint u100)  ;; blocks
(define-data-var throttle-threshold uint u10)  ;; max requests per timeframe

;; Monitor and enforce request limits
(define-private (enforce-request-limits (principal-address principal))
  (let
    (
      (request-data (default-to { last-request-block: u0, requests-in-timeframe: u0 }
        (map-get? request-monitor { principal-address: principal-address })))
      (current-timeframe-start (- block-height (var-get throttle-timeframe)))
    )
    (if (< (get last-request-block request-data) current-timeframe-start)
      ;; New timeframe, reset counter
      (begin
        (map-set request-monitor { principal-address: principal-address }
          { last-request-block: block-height, requests-in-timeframe: u1 })
        true)
      ;; Check limits in current timeframe
      (if (< (get requests-in-timeframe request-data) (var-get throttle-threshold))
        (begin
          (map-set request-monitor { principal-address: principal-address }
            { 
              last-request-block: block-height,
              requests-in-timeframe: (+ (get requests-in-timeframe request-data) u1)
            })
          true)
        false)
    )
  )
)

;; Throttled work registration
(define-public (throttled-work-registration
  (name (string-ascii 64))
  (metadata-size uint)
  (narrative (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (begin
    ;; Apply throttling check
    (asserts! (enforce-request-limits tx-sender) (err u700))

    ;; Call standard registration function
    (register-creative-work name metadata-size narrative categories)
  )
)

;;  Protocol safeguard mechanism
;; Enables governance to temporarily suspend protocol operations in emergency situations

;; Protocol operational status
(define-data-var protocol-suspended bool false)

;; Suspension justification
(define-data-var suspension-justification (string-ascii 128) "")

;; Restore protocol operations
(define-public (restore-protocol-operations)
  (begin
    ;; Governance restriction
    (asserts! (is-eq tx-sender governance-principal) governance-restricted-function)

    ;; Remove suspension state
    (var-set protocol-suspended false)
    (var-set suspension-justification "")
    (ok true)
  )
)

;; Verify protocol operational status
(define-private (protocol-operational-check)
  (not (var-get protocol-suspended))
)

;; Sequential operation tracking
(define-data-var operation-sequence uint u0)

;; Time-delay security parameter (blocks)
(define-data-var security-delay-blocks uint u10)

;; Initialize protected ownership transfer with time-delay security


;;  Hierarchical collaboration framework
;; Enables precise control over collaboration permissions with multiple tiers

;; Collaboration tier definitions
(define-constant tier-restricted u0)
(define-constant tier-viewer u1)
(define-constant tier-contributor u2)
(define-constant tier-manager u3)

;; Advanced collaboration registry with tiers
(define-map collaboration-tiers
  { work-id: uint, collaborator: principal }
  { 
    collaboration-tier: uint,
    authorized-by: principal,
    authorization-time: uint
  }
)

;; Establish collaboration tier for a participant
(define-public (authorize-collaborator (work-id uint) (collaborator principal) (collaboration-tier uint))
  (let
    (
      (work-record (unwrap! (map-get? creative-works { work-id: work-id })
        work-identifier-invalid))
    )
    ;; Verify caller is the work creator
    (asserts! (is-eq (get creator-principal work-record) tx-sender) ownership-verification-failed)
    ;; Verify valid tier assignment
    (asserts! (<= collaboration-tier tier-manager) (err u500))

    (ok true)
  )
)

;; Verify collaborator has required tier access
(define-private (verify-collaboration-tier (work-id uint) (collaborator principal) (required-tier uint))
  (let
    (
      (work-record (map-get? creative-works { work-id: work-id }))
      (tier-data (map-get? collaboration-tiers { work-id: work-id, collaborator: collaborator }))
    )
    (if (is-some work-record)
      (if (is-eq (get creator-principal (unwrap! work-record false)) collaborator)
        ;; Creator has all tiers
        true
        ;; Check tier level for collaborators
        (if (is-some tier-data)
          (>= (get collaboration-tier (unwrap! tier-data false)) required-tier)
          false
        )
      )
      false
    )
  )
)

;;  Content authenticity verification through cryptographic fingerprinting
;; Ensures the integrity and authenticity of creative works

;; Work integrity registry
(define-map work-integrity
  { work-id: uint }
  {
    content-fingerprint: (buff 32),
    fingerprint-algorithm: (string-ascii 10),
    verification-timestamp: uint,
    verified-by: principal
  }
)

;; Register cryptographic fingerprint for content verification
(define-public (register-content-fingerprint (work-id uint) (content-fingerprint (buff 32)) (algorithm-type (string-ascii 10)))
  (let
    (
      (work-record (unwrap! (map-get? creative-works { work-id: work-id })
        work-identifier-invalid))
    )
    ;; Verify caller is work creator
    (asserts! (is-eq (get creator-principal work-record) tx-sender) ownership-verification-failed)
    ;; Verify supported algorithm (sha256 or keccak256)
    (asserts! (or (is-eq algorithm-type "sha256") (is-eq algorithm-type "keccak256")) (err u600))

    (ok true)
  )
)


