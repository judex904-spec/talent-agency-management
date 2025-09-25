(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-data (err u104))

(define-map talent-profiles
  { talent-id: uint }
  {
    name: (string-ascii 100),
    talent-principal: principal,
    category: (string-ascii 50),
    base-rate: uint,
    availability-status: (string-ascii 20),
    agent: principal,
    total-bookings: uint,
    total-earnings: uint
  }
)

(define-map bookings
  { booking-id: uint }
  {
    talent-id: uint,
    client: principal,
    booking-date: uint,
    event-type: (string-ascii 50),
    agreed-rate: uint,
    booking-status: (string-ascii 20),
    contract-terms: (string-ascii 200),
    completion-date: (optional uint)
  }
)

(define-map negotiations
  { booking-id: uint, negotiation-round: uint }
  {
    proposed-rate: uint,
    terms-modification: (string-ascii 200),
    negotiator: principal,
    timestamp: uint,
    status: (string-ascii 20)
  }
)

(define-map schedules
  { talent-id: uint, schedule-id: uint }
  {
    start-date: uint,
    end-date: uint,
    booking-id: (optional uint),
    availability-type: (string-ascii 20),
    notes: (string-ascii 100)
  }
)

(define-map commission-tracking
  { booking-id: uint }
  {
    total-amount: uint,
    agency-commission: uint,
    talent-payment: uint,
    commission-rate: uint,
    payment-status: (string-ascii 20),
    payment-date: (optional uint)
  }
)

(define-data-var next-talent-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var next-schedule-id uint u1)
(define-data-var default-commission-rate uint u1500)

(define-public (register-talent
  (name (string-ascii 100))
  (talent-principal principal)
  (category (string-ascii 50))
  (base-rate uint)
)
  (let
    (
      (talent-id (var-get next-talent-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set talent-profiles
      { talent-id: talent-id }
      {
        name: name,
        talent-principal: talent-principal,
        category: category,
        base-rate: base-rate,
        availability-status: "available",
        agent: tx-sender,
        total-bookings: u0,
        total-earnings: u0
      }
    )
    (var-set next-talent-id (+ talent-id u1))
    (ok talent-id)
  )
)

(define-public (create-booking
  (talent-id uint)
  (client principal)
  (booking-date uint)
  (event-type (string-ascii 50))
  (agreed-rate uint)
  (contract-terms (string-ascii 200))
)
  (let
    (
      (booking-id (var-get next-booking-id))
      (talent (unwrap! (map-get? talent-profiles { talent-id: talent-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set bookings
      { booking-id: booking-id }
      {
        talent-id: talent-id,
        client: client,
        booking-date: booking-date,
        event-type: event-type,
        agreed-rate: agreed-rate,
        booking-status: "confirmed",
        contract-terms: contract-terms,
        completion-date: none
      }
    )
    (map-set talent-profiles
      { talent-id: talent-id }
      (merge talent { total-bookings: (+ (get total-bookings talent) u1) })
    )
    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)
  )
)

(define-public (negotiate-contract
  (booking-id uint)
  (proposed-rate uint)
  (terms-modification (string-ascii 200))
)
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (negotiation-round u1)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set negotiations
      { booking-id: booking-id, negotiation-round: negotiation-round }
      {
        proposed-rate: proposed-rate,
        terms-modification: terms-modification,
        negotiator: tx-sender,
        timestamp: stacks-block-height,
        status: "pending"
      }
    )
    (ok negotiation-round)
  )
)

(define-public (update-booking-rate
  (booking-id uint)
  (new-rate uint)
)
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set bookings
      { booking-id: booking-id }
      (merge booking { agreed-rate: new-rate })
    )
    (ok true)
  )
)

(define-public (add-schedule-block
  (talent-id uint)
  (start-date uint)
  (end-date uint)
  (booking-id (optional uint))
  (availability-type (string-ascii 20))
  (notes (string-ascii 100))
)
  (let
    (
      (schedule-id (var-get next-schedule-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? talent-profiles { talent-id: talent-id })) err-not-found)
    (asserts! (< start-date end-date) err-invalid-data)
    (map-set schedules
      { talent-id: talent-id, schedule-id: schedule-id }
      {
        start-date: start-date,
        end-date: end-date,
        booking-id: booking-id,
        availability-type: availability-type,
        notes: notes
      }
    )
    (var-set next-schedule-id (+ schedule-id u1))
    (ok schedule-id)
  )
)

(define-public (complete-booking
  (booking-id uint)
)
  (let
    (
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) err-not-found))
      (talent-id (get talent-id booking))
      (talent (unwrap! (map-get? talent-profiles { talent-id: talent-id }) err-not-found))
      (commission-rate (var-get default-commission-rate))
      (total-amount (get agreed-rate booking))
      (agency-commission (/ (* total-amount commission-rate) u10000))
      (talent-payment (- total-amount agency-commission))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set bookings
      { booking-id: booking-id }
      (merge booking {
        booking-status: "completed",
        completion-date: (some stacks-block-height)
      })
    )
    (map-set commission-tracking
      { booking-id: booking-id }
      {
        total-amount: total-amount,
        agency-commission: agency-commission,
        talent-payment: talent-payment,
        commission-rate: commission-rate,
        payment-status: "pending",
        payment-date: none
      }
    )
    (map-set talent-profiles
      { talent-id: talent-id }
      (merge talent { total-earnings: (+ (get total-earnings talent) talent-payment) })
    )
    (ok true)
  )
)

(define-public (process-payment
  (booking-id uint)
)
  (let
    (
      (commission (unwrap! (map-get? commission-tracking { booking-id: booking-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set commission-tracking
      { booking-id: booking-id }
      (merge commission {
        payment-status: "paid",
        payment-date: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (update-talent-availability
  (talent-id uint)
  (availability-status (string-ascii 20))
)
  (let
    (
      (talent (unwrap! (map-get? talent-profiles { talent-id: talent-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set talent-profiles
      { talent-id: talent-id }
      (merge talent { availability-status: availability-status })
    )
    (ok true)
  )
)

(define-read-only (get-talent-profile (talent-id uint))
  (map-get? talent-profiles { talent-id: talent-id })
)

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

(define-read-only (get-negotiation (booking-id uint) (negotiation-round uint))
  (map-get? negotiations { booking-id: booking-id, negotiation-round: negotiation-round })
)

(define-read-only (get-schedule (talent-id uint) (schedule-id uint))
  (map-get? schedules { talent-id: talent-id, schedule-id: schedule-id })
)

(define-read-only (get-commission-details (booking-id uint))
  (map-get? commission-tracking { booking-id: booking-id })
)

(define-read-only (get-talent-earnings (talent-id uint))
  (match (map-get? talent-profiles { talent-id: talent-id })
    talent (ok {
      total-bookings: (get total-bookings talent),
      total-earnings: (get total-earnings talent),
      base-rate: (get base-rate talent)
    })
    err-not-found
  )
)

(define-read-only (get-next-talent-id)
  (var-get next-talent-id)
)

(define-read-only (get-next-booking-id)
  (var-get next-booking-id)
)


;; title: talent-manager
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

