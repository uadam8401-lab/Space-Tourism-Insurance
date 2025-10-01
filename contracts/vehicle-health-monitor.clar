;; vehicle-health-monitor
;; Comprehensive spacecraft systems monitoring and pre-flight safety checks
;; This contract manages vehicle health monitoring and safety assessments

;; Error constants
(define-constant ERR-OWNER-ONLY (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-INVALID-INPUT (err u202))
(define-constant ERR-SYSTEM-FAILURE (err u203))
(define-constant ERR-UNAUTHORIZED (err u204))
(define-constant ERR-CRITICAL-FAILURE (err u205))

;; System health thresholds
(define-constant MIN-FUEL-LEVEL u85) ;; Minimum fuel percentage
(define-constant MIN-OXYGEN-LEVEL u90) ;; Minimum oxygen level percentage
(define-constant MAX-ENGINE-TEMP u750) ;; Maximum engine temperature in Celsius
(define-constant MIN-BATTERY-CHARGE u95) ;; Minimum battery charge percentage
(define-constant MAX-VIBRATION-LEVEL u50) ;; Maximum vibration level
(define-constant MIN-PRESSURE-LEVEL u95) ;; Minimum cabin pressure percentage

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var monitoring-active bool true)
(define-data-var total-vehicles uint u0)
(define-data-var emergency-protocol bool false)
(define-data-var maintenance-mode bool false)

;; Vehicle registration map
(define-map registered-vehicles
  { vehicle-id: (string-ascii 30) }
  {
    vehicle-name: (string-ascii 100),
    manufacturer: (string-ascii 50),
    model: (string-ascii 50),
    registration-date: uint,
    last-inspection: uint,
    flight-hours: uint,
    operational-status: (string-ascii 20),
    owner: principal
  }
)

;; System health monitoring map
(define-map system-health
  { vehicle-id: (string-ascii 30), timestamp: uint }
  {
    fuel-level: uint,
    oxygen-level: uint,
    engine-temperature: uint,
    battery-charge: uint,
    vibration-level: uint,
    cabin-pressure: uint,
    navigation-status: (string-ascii 20),
    communication-status: (string-ascii 20),
    life-support-status: (string-ascii 20),
    overall-health: uint,
    monitored-by: principal
  }
)

;; Safety inspection records
(define-map safety-inspections
  { inspection-id: uint }
  {
    vehicle-id: (string-ascii 30),
    inspection-type: (string-ascii 30),
    inspector: principal,
    inspection-date: uint,
    systems-checked: (list 20 (string-ascii 30)),
    passed-checks: uint,
    failed-checks: uint,
    overall-status: (string-ascii 20),
    next-inspection-due: uint,
    notes: (string-ascii 500)
  }
)

;; System alerts and warnings
(define-map system-alerts
  { alert-id: uint }
  {
    vehicle-id: (string-ascii 30),
    alert-type: (string-ascii 30),
    severity: (string-ascii 20),
    system-affected: (string-ascii 50),
    description: (string-ascii 200),
    triggered-at: uint,
    resolved: bool,
    resolved-at: uint,
    action-taken: (string-ascii 300)
  }
)

;; Authorized technicians and inspectors
(define-map authorized-personnel
  { technician-address: principal }
  {
    name: (string-ascii 100),
    certification-level: (string-ascii 30),
    specializations: (list 10 (string-ascii 30)),
    active: bool,
    last-activity: uint
  }
)

;; Maintenance records
(define-map maintenance-records
  { maintenance-id: uint }
  {
    vehicle-id: (string-ascii 30),
    maintenance-type: (string-ascii 50),
    performed-by: principal,
    performed-date: uint,
    systems-serviced: (list 15 (string-ascii 30)),
    parts-replaced: (list 10 (string-ascii 50)),
    cost: uint,
    next-service-due: uint,
    warranty-period: uint
  }
)

;; Public functions

;; Register a new vehicle for monitoring
(define-public (register-vehicle
  (vehicle-id (string-ascii 30))
  (vehicle-name (string-ascii 100))
  (manufacturer (string-ascii 50))
  (model (string-ascii 50))
  (owner principal))
  (let (
    (current-time stacks-block-height)
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (is-none (map-get? registered-vehicles { vehicle-id: vehicle-id })) ERR-INVALID-INPUT)
    (var-set total-vehicles (+ (var-get total-vehicles) u1))
    (ok (map-set registered-vehicles
      { vehicle-id: vehicle-id }
      {
        vehicle-name: vehicle-name,
        manufacturer: manufacturer,
        model: model,
        registration-date: current-time,
        last-inspection: u0,
        flight-hours: u0,
        operational-status: "ACTIVE",
        owner: owner
      }
    ))
  )
)

;; Update system health metrics
(define-public (update-system-health
  (vehicle-id (string-ascii 30))
  (fuel-level uint)
  (oxygen-level uint)
  (engine-temperature uint)
  (battery-charge uint)
  (vibration-level uint)
  (cabin-pressure uint)
  (navigation-status (string-ascii 20))
  (communication-status (string-ascii 20))
  (life-support-status (string-ascii 20)))
  (let (
    (current-time stacks-block-height)
    (overall-health (calculate-overall-health fuel-level oxygen-level engine-temperature battery-charge vibration-level cabin-pressure))
  )
    (asserts! (is-authorized-technician tx-sender) ERR-UNAUTHORIZED)
    (asserts! (var-get monitoring-active) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? registered-vehicles { vehicle-id: vehicle-id })) ERR-NOT-FOUND)
    
    ;; Check for critical failures and trigger alerts if necessary
    (try! (check-critical-systems vehicle-id fuel-level oxygen-level engine-temperature battery-charge cabin-pressure))
    
    (ok (map-set system-health
      { vehicle-id: vehicle-id, timestamp: current-time }
      {
        fuel-level: fuel-level,
        oxygen-level: oxygen-level,
        engine-temperature: engine-temperature,
        battery-charge: battery-charge,
        vibration-level: vibration-level,
        cabin-pressure: cabin-pressure,
        navigation-status: navigation-status,
        communication-status: communication-status,
        life-support-status: life-support-status,
        overall-health: overall-health,
        monitored-by: tx-sender
      }
    ))
  )
)

;; Conduct safety inspection
(define-public (conduct-safety-inspection
  (vehicle-id (string-ascii 30))
  (inspection-type (string-ascii 30))
  (systems-checked (list 20 (string-ascii 30)))
  (passed-checks uint)
  (failed-checks uint)
  (notes (string-ascii 500)))
  (let (
    (inspection-id (+ (get-total-inspections) u1))
    (current-time stacks-block-height)
    (overall-status (if (is-eq failed-checks u0) "PASSED" "FAILED"))
    (next-inspection (+ current-time u2592000)) ;; 30 days from now
  )
    (asserts! (is-authorized-technician tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? registered-vehicles { vehicle-id: vehicle-id })) ERR-NOT-FOUND)
    
    ;; Update vehicle's last inspection date
    (try! (update-vehicle-inspection-date vehicle-id current-time))
    
    (ok (map-set safety-inspections
      { inspection-id: inspection-id }
      {
        vehicle-id: vehicle-id,
        inspection-type: inspection-type,
        inspector: tx-sender,
        inspection-date: current-time,
        systems-checked: systems-checked,
        passed-checks: passed-checks,
        failed-checks: failed-checks,
        overall-status: overall-status,
        next-inspection-due: next-inspection,
        notes: notes
      }
    ))
  )
)

;; Create system alert
(define-public (create-system-alert
  (vehicle-id (string-ascii 30))
  (alert-type (string-ascii 30))
  (severity (string-ascii 20))
  (system-affected (string-ascii 50))
  (description (string-ascii 200)))
  (let (
    (alert-id (+ (get-total-alerts) u1))
    (current-time stacks-block-height)
  )
    (asserts! (is-authorized-technician tx-sender) ERR-UNAUTHORIZED)
    (ok (map-set system-alerts
      { alert-id: alert-id }
      {
        vehicle-id: vehicle-id,
        alert-type: alert-type,
        severity: severity,
        system-affected: system-affected,
        description: description,
        triggered-at: current-time,
        resolved: false,
        resolved-at: u0,
        action-taken: ""
      }
    ))
  )
)

;; Add authorized technician
(define-public (add-authorized-technician
  (technician-address principal)
  (name (string-ascii 100))
  (certification-level (string-ascii 30))
  (specializations (list 10 (string-ascii 30))))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (ok (map-set authorized-personnel
      { technician-address: technician-address }
      {
        name: name,
        certification-level: certification-level,
        specializations: specializations,
        active: true,
        last-activity: u0
      }
    ))
  )
)

;; Record maintenance activity
(define-public (record-maintenance
  (vehicle-id (string-ascii 30))
  (maintenance-type (string-ascii 50))
  (systems-serviced (list 15 (string-ascii 30)))
  (parts-replaced (list 10 (string-ascii 50)))
  (cost uint)
  (warranty-period uint))
  (let (
    (maintenance-id (+ (get-total-maintenance) u1))
    (current-time stacks-block-height)
    (next-service (+ current-time u7776000)) ;; 90 days from now
  )
    (asserts! (is-authorized-technician tx-sender) ERR-UNAUTHORIZED)
    (ok (map-set maintenance-records
      { maintenance-id: maintenance-id }
      {
        vehicle-id: vehicle-id,
        maintenance-type: maintenance-type,
        performed-by: tx-sender,
        performed-date: current-time,
        systems-serviced: systems-serviced,
        parts-replaced: parts-replaced,
        cost: cost,
        next-service-due: next-service,
        warranty-period: warranty-period
      }
    ))
  )
)

;; Read-only functions

;; Get vehicle registration details
(define-read-only (get-vehicle-details (vehicle-id (string-ascii 30)))
  (map-get? registered-vehicles { vehicle-id: vehicle-id })
)

;; Get current system health
(define-read-only (get-system-health (vehicle-id (string-ascii 30)) (timestamp uint))
  (map-get? system-health { vehicle-id: vehicle-id, timestamp: timestamp })
)

;; Get safety inspection record
(define-read-only (get-inspection-record (inspection-id uint))
  (map-get? safety-inspections { inspection-id: inspection-id })
)

;; Get system alert details
(define-read-only (get-system-alert (alert-id uint))
  (map-get? system-alerts { alert-id: alert-id })
)

;; Check if technician is authorized
(define-read-only (is-authorized-technician (technician-address principal))
  (match (map-get? authorized-personnel { technician-address: technician-address })
    personnel-info (get active personnel-info)
    false
  )
)

;; Get vehicle flight readiness status
(define-read-only (get-flight-readiness (vehicle-id (string-ascii 30)))
  (let (
    (current-time stacks-block-height)
    (latest-health (get-latest-health-reading vehicle-id))
  )
    (if (is-some latest-health)
      (get overall-health (unwrap-panic latest-health))
      u0
    )
  )
)

;; Private functions

;; Calculate overall health score based on system metrics
(define-private (calculate-overall-health 
  (fuel-level uint)
  (oxygen-level uint)
  (engine-temperature uint)
  (battery-charge uint)
  (vibration-level uint)
  (cabin-pressure uint))
  (let (
    (fuel-score (if (>= fuel-level MIN-FUEL-LEVEL) u100 (* fuel-level u1)))
    (oxygen-score (if (>= oxygen-level MIN-OXYGEN-LEVEL) u100 (* oxygen-level u1)))
    (temp-score (if (<= engine-temperature MAX-ENGINE-TEMP) u100 u50))
    (battery-score (if (>= battery-charge MIN-BATTERY-CHARGE) u100 (* battery-charge u1)))
    (vibration-score (if (<= vibration-level MAX-VIBRATION-LEVEL) u100 u60))
    (pressure-score (if (>= cabin-pressure MIN-PRESSURE-LEVEL) u100 (* cabin-pressure u1)))
  )
    (/ (+ fuel-score oxygen-score temp-score battery-score vibration-score pressure-score) u6)
  )
)

;; Check for critical system failures
(define-private (check-critical-systems
  (vehicle-id (string-ascii 30))
  (fuel-level uint)
  (oxygen-level uint)
  (engine-temperature uint)
  (battery-charge uint)
  (cabin-pressure uint))
  (let (
    (critical-fuel (< fuel-level u20))
    (critical-oxygen (< oxygen-level u30))
    (critical-temp (> engine-temperature u800))
    (critical-battery (< battery-charge u15))
    (critical-pressure (< cabin-pressure u50))
  )
    (if (or critical-fuel critical-oxygen critical-temp critical-battery critical-pressure)
      (create-system-alert 
        vehicle-id 
        "CRITICAL" 
        "EMERGENCY" 
        "MULTIPLE" 
        "Critical system failure detected")
      (ok true)
    )
  )
)

;; Update vehicle inspection date
(define-private (update-vehicle-inspection-date (vehicle-id (string-ascii 30)) (inspection-date uint))
  (match (map-get? registered-vehicles { vehicle-id: vehicle-id })
    vehicle-data
      (ok (map-set registered-vehicles
        { vehicle-id: vehicle-id }
        (merge vehicle-data { last-inspection: inspection-date })
      ))
    ERR-NOT-FOUND
  )
)

;; Get latest health reading for a vehicle
(define-private (get-latest-health-reading (vehicle-id (string-ascii 30)))
  (let (
    (current-time stacks-block-height)
  )
    (map-get? system-health { vehicle-id: vehicle-id, timestamp: current-time })
  )
)

;; Helper functions for counting records
(define-private (get-total-inspections)
  (fold + (map get-inspection-count (list u1 u2 u3 u4 u5)) u0)
)

(define-private (get-total-alerts)
  (fold + (map get-alert-count (list u1 u2 u3 u4 u5)) u0)
)

(define-private (get-total-maintenance)
  (fold + (map get-maintenance-count (list u1 u2 u3 u4 u5)) u0)
)

(define-private (get-inspection-count (id uint))
  (if (is-some (map-get? safety-inspections { inspection-id: id })) u1 u0)
)

(define-private (get-alert-count (id uint))
  (if (is-some (map-get? system-alerts { alert-id: id })) u1 u0)
)

(define-private (get-maintenance-count (id uint))
  (if (is-some (map-get? maintenance-records { maintenance-id: id })) u1 u0)
)
