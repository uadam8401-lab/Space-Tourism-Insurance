;; flight-cancellation-compensation
;; Automated refunds and compensation for delayed or cancelled space flights
;; This contract manages insurance payouts and passenger compensation

;; Error constants
(define-constant ERR-OWNER-ONLY (err u300))
(define-constant ERR-NOT-FOUND (err u301))
(define-constant ERR-INVALID-INPUT (err u302))
(define-constant ERR-INSUFFICIENT-FUNDS (err u303))
(define-constant ERR-UNAUTHORIZED (err u304))
(define-constant ERR-ALREADY-PROCESSED (err u305))
(define-constant ERR-POLICY-EXPIRED (err u306))
(define-constant ERR-CLAIM-DENIED (err u307))

;; Compensation constants
(define-constant BASE-COMPENSATION u1000000) ;; 1,000,000 microSTX base compensation
(define-constant DELAY-MULTIPLIER u100000) ;; 100,000 microSTX per hour delayed
(define-constant CANCELLATION-MULTIPLIER u5) ;; 5x base for full cancellation
(define-constant EMERGENCY-MULTIPLIER u10) ;; 10x base for emergency situations
(define-constant MAX-CLAIM-AMOUNT u50000000) ;; Maximum claim amount in microSTX
(define-constant POLICY-DURATION u31536000) ;; 1 year in seconds

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)
(define-data-var insurance-pool uint u0)
(define-data-var claims-processing bool true)
(define-data-var emergency-mode bool false)

;; Insurance policies map
(define-map insurance-policies
  { policy-id: uint }
  {
    policy-holder: principal,
    flight-id: (string-ascii 30),
    coverage-type: (string-ascii 30),
    premium-paid: uint,
    coverage-amount: uint,
    policy-start: uint,
    policy-end: uint,
    active: bool,
    beneficiary: (optional principal)
  }
)

;; Flight information map
(define-map flight-details
  { flight-id: (string-ascii 30) }
  {
    operator: (string-ascii 100),
    departure-time: uint,
    arrival-time: uint,
    launch-site: (string-ascii 50),
    destination: (string-ascii 50),
    passenger-capacity: uint,
    flight-status: (string-ascii 20),
    risk-level: uint,
    insurance-required: bool
  }
)

;; Claims processing map
(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-type: (string-ascii 30),
    incident-date: uint,
    claim-amount: uint,
    claim-status: (string-ascii 20),
    submitted-date: uint,
    processed-date: uint,
    evidence-hash: (string-ascii 64),
    processor: (optional principal),
    payout-amount: uint
  }
)

;; Compensation payouts map
(define-map compensation-payouts
  { payout-id: uint }
  {
    claim-id: uint,
    recipient: principal,
    amount: uint,
    payout-date: uint,
    transaction-id: (string-ascii 64),
    payout-reason: (string-ascii 100),
    processed-by: principal
  }
)

;; Risk assessment map
(define-map risk-assessments
  { assessment-id: uint }
  {
    flight-id: (string-ascii 30),
    weather-risk: uint,
    technical-risk: uint,
    operational-risk: uint,
    total-risk-score: uint,
    assessment-date: uint,
    assessor: principal,
    recommendations: (string-ascii 200)
  }
)

;; Authorized claim processors
(define-map authorized-processors
  { processor-address: principal }
  {
    processor-name: (string-ascii 100),
    authorization-level: (string-ascii 20),
    max-claim-amount: uint,
    active: bool,
    processed-claims: uint
  }
)

;; Premium calculation factors
(define-map premium-factors
  { factor-type: (string-ascii 30) }
  {
    base-rate: uint,
    risk-multiplier: uint,
    duration-factor: uint,
    coverage-factor: uint,
    last-updated: uint
  }
)

;; Public functions

;; Purchase insurance policy
(define-public (purchase-policy
  (flight-id (string-ascii 30))
  (coverage-type (string-ascii 30))
  (coverage-amount uint)
  (beneficiary (optional principal)))
  (let (
    (policy-id (+ (var-get total-policies) u1))
    (current-time stacks-block-height)
    (premium (calculate-premium coverage-type coverage-amount flight-id))
    (policy-end (+ current-time POLICY-DURATION))
  )
    (asserts! (> coverage-amount u0) ERR-INVALID-INPUT)
    (asserts! (<= coverage-amount MAX-CLAIM-AMOUNT) ERR-INVALID-INPUT)
    (asserts! (>= (stx-get-balance tx-sender) premium) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer premium to insurance pool
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    (var-set total-policies policy-id)
    
    (ok (map-set insurance-policies
      { policy-id: policy-id }
      {
        policy-holder: tx-sender,
        flight-id: flight-id,
        coverage-type: coverage-type,
        premium-paid: premium,
        coverage-amount: coverage-amount,
        policy-start: current-time,
        policy-end: policy-end,
        active: true,
        beneficiary: beneficiary
      }
    ))
  )
)

;; Submit insurance claim
(define-public (submit-claim
  (policy-id uint)
  (claim-type (string-ascii 30))
  (incident-date uint)
  (claim-amount uint)
  (evidence-hash (string-ascii 64)))
  (let (
    (claim-id (+ (var-get total-claims) u1))
    (current-time stacks-block-height)
    (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get policy-holder policy)) ERR-UNAUTHORIZED)
    (asserts! (get active policy) ERR-POLICY-EXPIRED)
    (asserts! (>= incident-date (get policy-start policy)) ERR-INVALID-INPUT)
    (asserts! (<= incident-date (get policy-end policy)) ERR-POLICY-EXPIRED)
    (asserts! (<= claim-amount (get coverage-amount policy)) ERR-INVALID-INPUT)
    (asserts! (var-get claims-processing) ERR-UNAUTHORIZED)
    
    (var-set total-claims claim-id)
    
    (ok (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-type: claim-type,
        incident-date: incident-date,
        claim-amount: claim-amount,
        claim-status: "PENDING",
        submitted-date: current-time,
        processed-date: u0,
        evidence-hash: evidence-hash,
        processor: none,
        payout-amount: u0
      }
    ))
  )
)

;; Process insurance claim (authorized processors only)
(define-public (process-claim
  (claim-id uint)
  (approved bool)
  (payout-amount uint)
  (processing-notes (string-ascii 200)))
  (let (
    (current-time stacks-block-height)
    (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR-NOT-FOUND))
    (claim-status (if approved "APPROVED" "DENIED"))
    (final-payout (if approved payout-amount u0))
  )
    (asserts! (is-authorized-processor tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get claim-status claim) "PENDING") ERR-ALREADY-PROCESSED)
    (asserts! (<= final-payout (get claim-amount claim)) ERR-INVALID-INPUT)
    
    ;; Update claim status
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim {
        claim-status: claim-status,
        processed-date: current-time,
        processor: (some tx-sender),
        payout-amount: final-payout
      })
    )
    
    ;; Process payout if approved
    (if approved
      (execute-payout claim-id (get claimant claim) final-payout)
      (ok true)
    )
  )
)

;; Register flight for insurance coverage
(define-public (register-flight
  (flight-id (string-ascii 30))
  (operator (string-ascii 100))
  (departure-time uint)
  (arrival-time uint)
  (launch-site (string-ascii 50))
  (destination (string-ascii 50))
  (passenger-capacity uint)
  (risk-level uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (is-none (map-get? flight-details { flight-id: flight-id })) ERR-INVALID-INPUT)
    
    (ok (map-set flight-details
      { flight-id: flight-id }
      {
        operator: operator,
        departure-time: departure-time,
        arrival-time: arrival-time,
        launch-site: launch-site,
        destination: destination,
        passenger-capacity: passenger-capacity,
        flight-status: "SCHEDULED",
        risk-level: risk-level,
        insurance-required: (> risk-level u50)
      }
    ))
  )
)

;; Update flight status (triggers automatic compensation)
(define-public (update-flight-status
  (flight-id (string-ascii 30))
  (new-status (string-ascii 20))
  (delay-hours uint))
  (let (
    (flight (unwrap! (map-get? flight-details { flight-id: flight-id }) ERR-NOT-FOUND))
    (updated-flight (merge flight { flight-status: new-status }))
  )
    (asserts! (is-authorized-processor tx-sender) ERR-UNAUTHORIZED)
    
    ;; Update flight status
    (map-set flight-details { flight-id: flight-id } updated-flight)
    
    ;; Trigger automatic compensation for delays/cancellations
    (if (or (is-eq new-status "DELAYED") (is-eq new-status "CANCELLED"))
      (trigger-automatic-compensation flight-id new-status delay-hours)
      (ok true)
    )
  )
)

;; Add authorized claim processor
(define-public (add-authorized-processor
  (processor-address principal)
  (processor-name (string-ascii 100))
  (authorization-level (string-ascii 20))
  (max-claim-amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (ok (map-set authorized-processors
      { processor-address: processor-address }
      {
        processor-name: processor-name,
        authorization-level: authorization-level,
        max-claim-amount: max-claim-amount,
        active: true,
        processed-claims: u0
      }
    ))
  )
)

;; Fund insurance pool
(define-public (fund-insurance-pool (amount uint))
  (begin
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok amount)
  )
)

;; Read-only functions

;; Get insurance policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

;; Get flight details
(define-read-only (get-flight-details (flight-id (string-ascii 30)))
  (map-get? flight-details { flight-id: flight-id })
)

;; Check if processor is authorized
(define-read-only (is-authorized-processor (processor-address principal))
  (match (map-get? authorized-processors { processor-address: processor-address })
    processor-info (get active processor-info)
    false
  )
)

;; Calculate premium for coverage
(define-read-only (calculate-premium-quote
  (coverage-type (string-ascii 30))
  (coverage-amount uint)
  (flight-id (string-ascii 30)))
  (calculate-premium coverage-type coverage-amount flight-id)
)

;; Get insurance pool balance
(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

;; Get total active policies
(define-read-only (get-total-policies)
  (var-get total-policies)
)

;; Get total claims processed
(define-read-only (get-total-claims)
  (var-get total-claims)
)

;; Private functions

;; Calculate insurance premium based on coverage and risk
(define-private (calculate-premium
  (coverage-type (string-ascii 30))
  (coverage-amount uint)
  (flight-id (string-ascii 30)))
  (let (
    (base-premium (/ coverage-amount u100)) ;; 1% of coverage amount
    (risk-multiplier (get-risk-multiplier flight-id))
    (type-multiplier (get-coverage-type-multiplier coverage-type))
  )
    (* (* base-premium risk-multiplier) type-multiplier)
  )
)

;; Get risk multiplier for flight
(define-private (get-risk-multiplier (flight-id (string-ascii 30)))
  (match (map-get? flight-details { flight-id: flight-id })
    flight-info 
      (let ((risk-level (get risk-level flight-info)))
        (if (< risk-level u30) u1
          (if (< risk-level u60) u2
            (if (< risk-level u80) u3 u5)
          )
        )
      )
    u3 ;; Default moderate risk
  )
)

;; Get coverage type multiplier
(define-private (get-coverage-type-multiplier (coverage-type (string-ascii 30)))
  (if (is-eq coverage-type "BASIC") u1
    (if (is-eq coverage-type "PREMIUM") u2
      (if (is-eq coverage-type "COMPREHENSIVE") u3 u1)
    )
  )
)

;; Execute payout to claimant
(define-private (execute-payout (claim-id uint) (recipient principal) (amount uint))
  (let (
    (payout-id (+ (get-total-payouts) u1))
    (current-time stacks-block-height)
  )
    (asserts! (>= (var-get insurance-pool) amount) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer funds from insurance pool to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set insurance-pool (- (var-get insurance-pool) amount))
    
    ;; Record payout
    (ok (map-set compensation-payouts
      { payout-id: payout-id }
      {
        claim-id: claim-id,
        recipient: recipient,
        amount: amount,
        payout-date: current-time,
        transaction-id: "",
        payout-reason: "Claim approved",
        processed-by: tx-sender
      }
    ))
  )
)

;; Trigger automatic compensation for flight disruptions
(define-private (trigger-automatic-compensation
  (flight-id (string-ascii 30))
  (disruption-type (string-ascii 20))
  (delay-hours uint))
  (let (
    (compensation-amount (calculate-automatic-compensation disruption-type delay-hours))
  )
    ;; Find and compensate all affected policy holders
    (ok (process-affected-policies flight-id compensation-amount))
  )
)

;; Calculate automatic compensation amount
(define-private (calculate-automatic-compensation
  (disruption-type (string-ascii 20))
  (delay-hours uint))
  (if (is-eq disruption-type "CANCELLED")
    (* BASE-COMPENSATION CANCELLATION-MULTIPLIER)
    (if (is-eq disruption-type "DELAYED")
      (+ BASE-COMPENSATION (* DELAY-MULTIPLIER delay-hours))
      BASE-COMPENSATION
    )
  )
)

;; Process affected policies for automatic compensation
(define-private (process-affected-policies (flight-id (string-ascii 30)) (compensation-amount uint))
  ;; This is a simplified version - in a real implementation,
  ;; this would iterate through all policies for the flight
  true
)

;; Helper function to get total payouts count
(define-private (get-total-payouts)
  (fold + (map get-payout-count (list u1 u2 u3 u4 u5)) u0)
)

(define-private (get-payout-count (id uint))
  (if (is-some (map-get? compensation-payouts { payout-id: id })) u1 u0)
)
