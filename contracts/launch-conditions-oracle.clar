;; launch-conditions-oracle
;; Real-time weather monitoring, space traffic analysis, and launch window optimization
;; This contract serves as an oracle for launch conditions and risk assessment

;; Error constants
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-INPUT (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-UNAUTHORIZED (err u104))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-WIND-SPEED u50) ;; Maximum allowed wind speed in km/h
(define-constant MIN-VISIBILITY u10) ;; Minimum visibility in km
(define-constant MAX-PRECIPITATION u5) ;; Maximum precipitation percentage
(define-constant CRITICAL-RISK-THRESHOLD u80) ;; Risk threshold percentage

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var oracle-active bool true)
(define-data-var total-assessments uint u0)
(define-data-var emergency-override bool false)

;; Weather conditions data map
(define-map weather-conditions
  { launch-site: (string-ascii 50), timestamp: uint }
  {
    wind-speed: uint,
    visibility: uint,
    precipitation: uint,
    temperature: int,
    pressure: uint,
    cloud-coverage: uint,
    lightning-risk: bool,
    updated-by: principal
  }
)

;; Space traffic data map
(define-map space-traffic
  { orbital-zone: (string-ascii 30), timestamp: uint }
  {
    active-objects: uint,
    collision-risk: uint,
    debris-density: uint,
    traffic-status: (string-ascii 20),
    next-clear-window: uint,
    updated-by: principal
  }
)

;; Launch window assessments
(define-map launch-assessments
  { assessment-id: uint }
  {
    launch-site: (string-ascii 50),
    flight-id: (string-ascii 30),
    risk-score: uint,
    weather-score: uint,
    traffic-score: uint,
    recommendation: (string-ascii 20),
    assessment-time: uint,
    valid-until: uint,
    assessor: principal
  }
)

;; Authorized data providers
(define-map authorized-oracles
  { oracle-address: principal }
  {
    oracle-name: (string-ascii 50),
    data-types: (list 10 (string-ascii 20)),
    active: bool,
    last-update: uint
  }
)

;; Launch site configurations
(define-map launch-sites
  { site-id: (string-ascii 50) }
  {
    site-name: (string-ascii 100),
    latitude: int,
    longitude: int,
    elevation: uint,
    operational: bool,
    risk-factors: (list 5 (string-ascii 30))
  }
)

;; Public functions

;; Update weather conditions for a specific launch site
(define-public (update-weather-conditions
  (launch-site (string-ascii 50))
  (wind-speed uint)
  (visibility uint)
  (precipitation uint)
  (temperature int)
  (pressure uint)
  (cloud-coverage uint)
  (lightning-risk bool))
  (let (
    (timestamp stacks-block-height)
    (caller tx-sender)
  )
    (asserts! (is-authorized-oracle caller) ERR-UNAUTHORIZED)
    (asserts! (var-get oracle-active) ERR-UNAUTHORIZED)
    (ok (map-set weather-conditions
      { launch-site: launch-site, timestamp: timestamp }
      {
        wind-speed: wind-speed,
        visibility: visibility,
        precipitation: precipitation,
        temperature: temperature,
        pressure: pressure,
        cloud-coverage: cloud-coverage,
        lightning-risk: lightning-risk,
        updated-by: caller
      }
    ))
  )
)

;; Update space traffic information
(define-public (update-space-traffic
  (orbital-zone (string-ascii 30))
  (active-objects uint)
  (collision-risk uint)
  (debris-density uint)
  (traffic-status (string-ascii 20))
  (next-clear-window uint))
  (let (
    (timestamp stacks-block-height)
    (caller tx-sender)
  )
    (asserts! (is-authorized-oracle caller) ERR-UNAUTHORIZED)
    (asserts! (var-get oracle-active) ERR-UNAUTHORIZED)
    (ok (map-set space-traffic
      { orbital-zone: orbital-zone, timestamp: timestamp }
      {
        active-objects: active-objects,
        collision-risk: collision-risk,
        debris-density: debris-density,
        traffic-status: traffic-status,
        next-clear-window: next-clear-window,
        updated-by: caller
      }
    ))
  )
)

;; Create comprehensive launch assessment
(define-public (create-launch-assessment
  (flight-id (string-ascii 30))
  (launch-site (string-ascii 50))
  (orbital-zone (string-ascii 30)))
  (let (
    (assessment-id (+ (var-get total-assessments) u1))
    (current-time stacks-block-height)
    (weather-score (calculate-weather-score launch-site current-time))
    (traffic-score (calculate-traffic-score orbital-zone current-time))
    (risk-score (+ weather-score traffic-score))
    (recommendation (get-launch-recommendation risk-score))
  )
    (asserts! (is-authorized-oracle tx-sender) ERR-UNAUTHORIZED)
    (var-set total-assessments assessment-id)
    (ok (map-set launch-assessments
      { assessment-id: assessment-id }
      {
        launch-site: launch-site,
        flight-id: flight-id,
        risk-score: risk-score,
        weather-score: weather-score,
        traffic-score: traffic-score,
        recommendation: recommendation,
        assessment-time: current-time,
        valid-until: (+ current-time u3600), ;; Valid for 1 hour
        assessor: tx-sender
      }
    ))
  )
)

;; Add authorized oracle
(define-public (add-authorized-oracle
  (oracle-address principal)
  (oracle-name (string-ascii 50))
  (data-types (list 10 (string-ascii 20))))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (ok (map-set authorized-oracles
      { oracle-address: oracle-address }
      {
        oracle-name: oracle-name,
        data-types: data-types,
        active: true,
        last-update: u0
      }
    ))
  )
)

;; Register launch site
(define-public (register-launch-site
  (site-id (string-ascii 50))
  (site-name (string-ascii 100))
  (latitude int)
  (longitude int)
  (elevation uint)
  (risk-factors (list 5 (string-ascii 30))))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (is-none (map-get? launch-sites { site-id: site-id })) ERR-ALREADY-EXISTS)
    (ok (map-set launch-sites
      { site-id: site-id }
      {
        site-name: site-name,
        latitude: latitude,
        longitude: longitude,
        elevation: elevation,
        operational: true,
        risk-factors: risk-factors
      }
    ))
  )
)

;; Emergency override function
(define-public (set-emergency-override (override bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (ok (var-set emergency-override override))
  )
)

;; Toggle oracle status
(define-public (toggle-oracle-status)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (ok (var-set oracle-active (not (var-get oracle-active))))
  )
)

;; Read-only functions

;; Get current weather conditions
(define-read-only (get-weather-conditions (launch-site (string-ascii 50)) (timestamp uint))
  (map-get? weather-conditions { launch-site: launch-site, timestamp: timestamp })
)

;; Get space traffic information
(define-read-only (get-space-traffic (orbital-zone (string-ascii 30)) (timestamp uint))
  (map-get? space-traffic { orbital-zone: orbital-zone, timestamp: timestamp })
)

;; Get launch assessment
(define-read-only (get-launch-assessment (assessment-id uint))
  (map-get? launch-assessments { assessment-id: assessment-id })
)

;; Check if address is authorized oracle
(define-read-only (is-authorized-oracle (oracle-address principal))
  (match (map-get? authorized-oracles { oracle-address: oracle-address })
    oracle-info (get active oracle-info)
    false
  )
)

;; Get launch site information
(define-read-only (get-launch-site (site-id (string-ascii 50)))
  (map-get? launch-sites { site-id: site-id })
)

;; Get current risk assessment for launch
(define-read-only (get-current-risk-level (launch-site (string-ascii 50)) (orbital-zone (string-ascii 30)))
  (let (
    (current-time stacks-block-height)
    (weather-score (calculate-weather-score launch-site current-time))
    (traffic-score (calculate-traffic-score orbital-zone current-time))
  )
    (+ weather-score traffic-score)
  )
)

;; Private functions

;; Calculate weather risk score
(define-private (calculate-weather-score (launch-site (string-ascii 50)) (timestamp uint))
  (match (map-get? weather-conditions { launch-site: launch-site, timestamp: timestamp })
    weather-data
      (let (
        (wind-risk (if (> (get wind-speed weather-data) MAX-WIND-SPEED) u30 u0))
        (visibility-risk (if (< (get visibility weather-data) MIN-VISIBILITY) u20 u0))
        (precipitation-risk (if (> (get precipitation weather-data) MAX-PRECIPITATION) u25 u0))
        (lightning-risk (if (get lightning-risk weather-data) u40 u0))
      )
        (+ wind-risk visibility-risk precipitation-risk lightning-risk)
      )
    u50 ;; Default high risk if no data available
  )
)

;; Calculate space traffic risk score
(define-private (calculate-traffic-score (orbital-zone (string-ascii 30)) (timestamp uint))
  (match (map-get? space-traffic { orbital-zone: orbital-zone, timestamp: timestamp })
    traffic-data
      (let (
        (collision-risk (get collision-risk traffic-data))
        (debris-risk (if (> (get debris-density traffic-data) u100) u30 u10))
        (congestion-risk (if (> (get active-objects traffic-data) u50) u20 u5))
      )
        (+ collision-risk debris-risk congestion-risk)
      )
    u40 ;; Default moderate risk if no data available
  )
)

;; Get launch recommendation based on risk score
(define-private (get-launch-recommendation (risk-score uint))
  (if (var-get emergency-override)
    "OVERRIDE"
    (if (< risk-score u30)
      "GO"
      (if (< risk-score u60)
        "CAUTION"
        "NO-GO"
      )
    )
  )
)
