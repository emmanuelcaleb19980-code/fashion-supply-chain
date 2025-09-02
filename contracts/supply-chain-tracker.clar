
;; title: Fashion Supply Chain Transparency Tracker
;; version: 1.0.0
;; summary: A smart contract to track clothing manufacturing with labor condition verification, environmental impact monitoring, and consumer transparency
;; description: This contract enables transparent tracking of fashion products through their supply chain journey, recording labor conditions, environmental metrics, and providing verifiable product information for consumers

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STAGE (err u102))
(define-constant ERR-PRODUCT-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-INPUT (err u104))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Product structure with comprehensive tracking
(define-map products
  { product-id: uint }
  {
    manufacturer: principal,
    product-name: (string-ascii 100),
    raw-material-origin: (string-ascii 100),
    manufacturing-location: (string-ascii 100),
    labor-certification: (string-ascii 100),
    environmental-score: uint,
    carbon-footprint: uint,
    water-usage: uint,
    waste-generated: uint,
    fair-trade-certified: bool,
    organic-certified: bool,
    creation-timestamp: uint,
    current-stage: (string-ascii 50),
    is-completed: bool
  }
)

;; Supply chain stages tracking
(define-map supply-chain-stages
  { product-id: uint, stage-number: uint }
  {
    stage-name: (string-ascii 50),
    location: (string-ascii 100),
    responsible-party: principal,
    labor-conditions-score: uint,
    environmental-impact: uint,
    timestamp: uint,
    verification-hash: (string-ascii 64),
    notes: (string-ascii 200)
  }
)

;; Labor condition assessments
(define-map labor-assessments
  { assessment-id: uint }
  {
    product-id: uint,
    facility-location: (string-ascii 100),
    assessor: principal,
    worker-safety-score: uint,
    fair-wage-compliance: bool,
    working-hours-compliance: bool,
    child-labor-free: bool,
    overall-score: uint,
    assessment-timestamp: uint,
    notes: (string-ascii 300)
  }
)

;; Product counters
(define-data-var next-product-id uint u1)
(define-data-var next-assessment-id uint u1)

;; Authorization check
(define-private (is-authorized-user (user principal) (product-id uint))
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) false)))
    (or 
      (is-eq user (var-get contract-owner))
      (is-eq user (get manufacturer product-data))
    )
  )
)

;; Create new product entry
(define-public (create-product 
    (product-name (string-ascii 100))
    (raw-material-origin (string-ascii 100))
    (manufacturing-location (string-ascii 100))
    (labor-certification (string-ascii 100))
    (environmental-score uint)
    (carbon-footprint uint)
    (water-usage uint)
    (fair-trade-certified bool)
    (organic-certified bool)
  )
  (let 
    (
      (product-id (var-get next-product-id))
      (waste-calc (+ carbon-footprint (/ water-usage u100)))
    )
    (asserts! (> (len product-name) u0) ERR-INVALID-INPUT)
    (asserts! (<= environmental-score u100) ERR-INVALID-INPUT)
    (asserts! (is-none (map-get? products { product-id: product-id })) ERR-PRODUCT-ALREADY-EXISTS)
    
    (map-set products
      { product-id: product-id }
      {
        manufacturer: tx-sender,
        product-name: product-name,
        raw-material-origin: raw-material-origin,
        manufacturing-location: manufacturing-location,
        labor-certification: labor-certification,
        environmental-score: environmental-score,
        carbon-footprint: carbon-footprint,
        water-usage: water-usage,
        waste-generated: waste-calc,
        fair-trade-certified: fair-trade-certified,
        organic-certified: organic-certified,
        creation-timestamp: stacks-block-height,
        current-stage: "created",
        is-completed: false
      }
    )
    
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Add supply chain stage
(define-public (add-supply-chain-stage
    (product-id uint)
    (stage-number uint)
    (stage-name (string-ascii 50))
    (location (string-ascii 100))
    (labor-conditions-score uint)
    (environmental-impact uint)
    (verification-hash (string-ascii 64))
    (notes (string-ascii 200))
  )
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (asserts! (is-authorized-user tx-sender product-id) ERR-NOT-AUTHORIZED)
    (asserts! (<= labor-conditions-score u100) ERR-INVALID-INPUT)
    (asserts! (<= environmental-impact u100) ERR-INVALID-INPUT)
    (asserts! (> (len stage-name) u0) ERR-INVALID-INPUT)
    
    (map-set supply-chain-stages
      { product-id: product-id, stage-number: stage-number }
      {
        stage-name: stage-name,
        location: location,
        responsible-party: tx-sender,
        labor-conditions-score: labor-conditions-score,
        environmental-impact: environmental-impact,
        timestamp: stacks-block-height,
        verification-hash: verification-hash,
        notes: notes
      }
    )
    
    ;; Update product current stage
    (map-set products
      { product-id: product-id }
      (merge product-data { current-stage: stage-name })
    )
    
    (ok true)
  )
)

;; Record labor assessment
(define-public (record-labor-assessment
    (product-id uint)
    (facility-location (string-ascii 100))
    (worker-safety-score uint)
    (fair-wage-compliance bool)
    (working-hours-compliance bool)
    (child-labor-free bool)
    (notes (string-ascii 300))
  )
  (let 
    (
      (assessment-id (var-get next-assessment-id))
      (overall-score (calculate-labor-score worker-safety-score fair-wage-compliance working-hours-compliance child-labor-free))
    )
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-PRODUCT-NOT-FOUND)
    (asserts! (<= worker-safety-score u100) ERR-INVALID-INPUT)
    (asserts! (> (len facility-location) u0) ERR-INVALID-INPUT)
    
    (map-set labor-assessments
      { assessment-id: assessment-id }
      {
        product-id: product-id,
        facility-location: facility-location,
        assessor: tx-sender,
        worker-safety-score: worker-safety-score,
        fair-wage-compliance: fair-wage-compliance,
        working-hours-compliance: working-hours-compliance,
        child-labor-free: child-labor-free,
        overall-score: overall-score,
        assessment-timestamp: stacks-block-height,
        notes: notes
      }
    )
    
    (var-set next-assessment-id (+ assessment-id u1))
    (ok assessment-id)
  )
)

;; Mark product as completed
(define-public (complete-product (product-id uint))
  (let ((product-data (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (asserts! (is-authorized-user tx-sender product-id) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-completed product-data)) ERR-INVALID-STAGE)
    
    (map-set products
      { product-id: product-id }
      (merge product-data 
        { 
          is-completed: true,
          current-stage: "completed"
        }
      )
    )
    (ok true)
  )
)

;; Calculate labor score based on compliance factors
(define-private (calculate-labor-score 
    (safety-score uint)
    (fair-wage bool)
    (working-hours bool)
    (child-labor-free bool)
  )
  (let 
    (
      (wage-points (if fair-wage u25 u0))
      (hours-points (if working-hours u25 u0))
      (child-labor-points (if child-labor-free u25 u0))
      (safety-points (/ safety-score u4))
    )
    (+ safety-points wage-points hours-points child-labor-points)
  )
)

;; Read-only functions for transparency
(define-read-only (get-product-info (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-supply-chain-stage (product-id uint) (stage-number uint))
  (map-get? supply-chain-stages { product-id: product-id, stage-number: stage-number })
)

(define-read-only (get-labor-assessment (assessment-id uint))
  (map-get? labor-assessments { assessment-id: assessment-id })
)

(define-read-only (get-product-sustainability-score (product-id uint))
  (match (map-get? products { product-id: product-id })
    product-data
    (ok 
      {
        environmental-score: (get environmental-score product-data),
        carbon-footprint: (get carbon-footprint product-data),
        water-usage: (get water-usage product-data),
        fair-trade-certified: (get fair-trade-certified product-data),
        organic-certified: (get organic-certified product-data),
        sustainability-rating: (calculate-sustainability-rating 
          (get environmental-score product-data)
          (get fair-trade-certified product-data)
          (get organic-certified product-data)
        )
      }
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

(define-read-only (get-next-product-id)
  (var-get next-product-id)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Helper function to get minimum of two values
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Calculate sustainability rating
(define-private (calculate-sustainability-rating
    (env-score uint)
    (fair-trade bool)
    (organic bool)
  )
  (let 
    (
      (base-score env-score)
      (fair-trade-bonus (if fair-trade u10 u0))
      (organic-bonus (if organic u10 u0))
    )
    (min-uint u100 (+ base-score fair-trade-bonus organic-bonus))
  )
)

