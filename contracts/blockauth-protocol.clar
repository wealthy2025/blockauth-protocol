

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
