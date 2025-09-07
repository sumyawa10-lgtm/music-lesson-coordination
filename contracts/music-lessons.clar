;; Music Lesson Coordination Contract
;; A smart contract for managing music instructors, students, lesson scheduling, and progress tracking

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-INPUT (err u104))
(define-constant ERR-LESSON-CONFLICT (err u105))

;; Data Variables
(define-data-var next-instructor-id uint u1)
(define-data-var next-student-id uint u1)
(define-data-var next-lesson-id uint u1)

;; Data Maps

;; Instructors: id -> {name, specialty, rate-per-hour, active, wallet}
(define-map instructors uint {
    name: (string-ascii 50),
    specialty: (string-ascii 30),
    rate-per-hour: uint,
    active: bool,
    wallet: principal
})

;; Students: id -> {name, level, preferred-instrument, wallet, instructor-id}
(define-map students uint {
    name: (string-ascii 50),
    level: (string-ascii 20),
    preferred-instrument: (string-ascii 30),
    wallet: principal,
    instructor-id: (optional uint)
})

;; Lessons: id -> {instructor-id, student-id, scheduled-time, duration, status, notes}
(define-map lessons uint {
    instructor-id: uint,
    student-id: uint,
    scheduled-time: uint,
    duration: uint,
    status: (string-ascii 20),
    notes: (string-ascii 200)
})

;; Progress tracking: student-id -> {total-lessons, skill-level, last-assessment}
(define-map student-progress uint {
    total-lessons: uint,
    skill-level: uint,
    last-assessment: (string-ascii 100)
})

;; Public Functions

;; Register a new instructor
(define-public (register-instructor (name (string-ascii 50)) (specialty (string-ascii 30)) (rate-per-hour uint))
    (let (
        (instructor-id (var-get next-instructor-id))
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> rate-per-hour u0) ERR-INVALID-INPUT)
        
        (map-set instructors instructor-id {
            name: name,
            specialty: specialty,
            rate-per-hour: rate-per-hour,
            active: true,
            wallet: tx-sender
        })
        
        (var-set next-instructor-id (+ instructor-id u1))
        (ok instructor-id)
    )
)

;; Register a new student
(define-public (register-student (name (string-ascii 50)) (level (string-ascii 20)) (preferred-instrument (string-ascii 30)))
    (let (
        (student-id (var-get next-student-id))
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        
        (map-set students student-id {
            name: name,
            level: level,
            preferred-instrument: preferred-instrument,
            wallet: tx-sender,
            instructor-id: none
        })
        
        ;; Initialize progress tracking
        (map-set student-progress student-id {
            total-lessons: u0,
            skill-level: u1,
            last-assessment: "Initial registration"
        })
        
        (var-set next-student-id (+ student-id u1))
        (ok student-id)
    )
)

;; Assign instructor to student
(define-public (assign-instructor (student-id uint) (instructor-id uint))
    (let (
        (student (unwrap! (map-get? students student-id) ERR-NOT-FOUND))
        (instructor (unwrap! (map-get? instructors instructor-id) ERR-NOT-FOUND))
    )
        ;; Only contract owner or the student can assign instructor
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get wallet student))) ERR-UNAUTHORIZED)
        (asserts! (get active instructor) ERR-INVALID-INPUT)
        
        (map-set students student-id (merge student {instructor-id: (some instructor-id)}))
        (ok true)
    )
)

;; Schedule a lesson
(define-public (schedule-lesson (instructor-id uint) (student-id uint) (scheduled-time uint) (duration uint))
    (let (
        (lesson-id (var-get next-lesson-id))
        (instructor (unwrap! (map-get? instructors instructor-id) ERR-NOT-FOUND))
        (student (unwrap! (map-get? students student-id) ERR-NOT-FOUND))
    )
        ;; Only instructor or student can schedule
        (asserts! (or (is-eq tx-sender (get wallet instructor)) (is-eq tx-sender (get wallet student))) ERR-UNAUTHORIZED)
        (asserts! (get active instructor) ERR-INVALID-INPUT)
        (asserts! (> duration u0) ERR-INVALID-INPUT)
        
        (map-set lessons lesson-id {
            instructor-id: instructor-id,
            student-id: student-id,
            scheduled-time: scheduled-time,
            duration: duration,
            status: "scheduled",
            notes: ""
        })
        
        (var-set next-lesson-id (+ lesson-id u1))
        (ok lesson-id)
    )
)

;; Complete a lesson and add notes
(define-public (complete-lesson (lesson-id uint) (notes (string-ascii 200)))
    (let (
        (lesson (unwrap! (map-get? lessons lesson-id) ERR-NOT-FOUND))
        (instructor (unwrap! (map-get? instructors (get instructor-id lesson)) ERR-NOT-FOUND))
        (student-progress-data (unwrap! (map-get? student-progress (get student-id lesson)) ERR-NOT-FOUND))
    )
        ;; Only the instructor can mark lesson as complete
        (asserts! (is-eq tx-sender (get wallet instructor)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status lesson) "scheduled") ERR-INVALID-INPUT)
        
        ;; Update lesson status
        (map-set lessons lesson-id (merge lesson {
            status: "completed",
            notes: notes
        }))
        
        ;; Update student progress with properly typed assessment
        (map-set student-progress (get student-id lesson) {
            total-lessons: (+ (get total-lessons student-progress-data) u1),
            skill-level: (get skill-level student-progress-data),
            last-assessment: (if (> (len notes) u100)
                               (unwrap-panic (as-max-len? notes u100))
                               (unwrap-panic (as-max-len? notes u100)))
        })
        
        (ok true)
    )
)

;; Update student skill level
(define-public (update-skill-level (student-id uint) (new-level uint))
    (let (
        (student (unwrap! (map-get? students student-id) ERR-NOT-FOUND))
        (progress (unwrap! (map-get? student-progress student-id) ERR-NOT-FOUND))
        (instructor-id (unwrap! (get instructor-id student) ERR-NOT-FOUND))
        (instructor (unwrap! (map-get? instructors instructor-id) ERR-NOT-FOUND))
    )
        ;; Only assigned instructor can update skill level
        (asserts! (is-eq tx-sender (get wallet instructor)) ERR-UNAUTHORIZED)
        (asserts! (and (>= new-level u1) (<= new-level u10)) ERR-INVALID-INPUT)
        
        (map-set student-progress student-id (merge progress {skill-level: new-level}))
        (ok true)
    )
)

;; Read-only Functions

;; Get instructor details
(define-read-only (get-instructor (instructor-id uint))
    (map-get? instructors instructor-id)
)

;; Get student details
(define-read-only (get-student (student-id uint))
    (map-get? students student-id)
)

;; Get lesson details
(define-read-only (get-lesson (lesson-id uint))
    (map-get? lessons lesson-id)
)

;; Get student progress
(define-read-only (get-student-progress (student-id uint))
    (map-get? student-progress student-id)
)

;; Get next IDs for reference
(define-read-only (get-next-ids)
    {
        next-instructor-id: (var-get next-instructor-id),
        next-student-id: (var-get next-student-id),
        next-lesson-id: (var-get next-lesson-id)
    }
)
