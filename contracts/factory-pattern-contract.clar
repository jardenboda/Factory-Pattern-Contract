(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PRODUCT_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PARAMETERS (err u105))
(define-constant ERR_FACTORY_DISABLED (err u106))
(define-constant ERR_INVALID_TEMPLATE (err u107))
(define-constant ERR_DEPLOYMENT_FAILED (err u108))
(define-constant ERR_VERSION_NOT_FOUND (err u109))
(define-constant ERR_INVALID_VERSION (err u110))
(define-constant ERR_INVALID_ROYALTY (err u111))
(define-constant ERR_NO_EARNINGS (err u112))
(define-constant ERR_INVALID_RATING (err u113))
(define-constant ERR_ALREADY_RATED (err u114))
(define-constant ERR_NOT_DEPLOYER (err u115))
(define-constant ERR_ACCESS_DENIED (err u116))

(define-data-var contract-owner principal tx-sender)
(define-data-var factory-enabled bool true)
(define-data-var deployment-fee uint u1000000)
(define-data-var next-product-id uint u1)
(define-data-var total-revenue uint u0)
(define-data-var next-version-id uint u1)
(define-data-var marketplace-fee-percentage uint u500)
(define-data-var max-royalty-percentage uint u5000)

(define-map product-templates
  { template-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    template-code: (string-ascii 10000),
    creation-fee: uint,
    creator: principal,
    active: bool,
    deployments: uint
  }
)

(define-map deployed-contracts
  { contract-id: uint }
  {
    template-id: uint,
    deployer: principal,
    contract-address: principal,
    deployment-block: uint,
    initialization-data: (string-ascii 500),
    active: bool
  }
)

(define-map user-deployments
  { user: principal, template-id: uint }
  { contract-ids: (list 100 uint) }
)

(define-map template-statistics
  { template-id: uint }
  {
    total-deployments: uint,
    total-revenue: uint,
    last-deployment-block: uint
  }
)

(define-map template-versions
  { template-id: uint, version: uint }
  {
    template-code: (string-ascii 10000),
    version-notes: (string-ascii 500),
    created-at: uint,
    active: bool,
    deployments: uint
  }
)

(define-map template-version-info
  { template-id: uint }
  {
    current-version: uint,
    total-versions: uint,
    latest-version: uint
  }
)

(define-map template-royalties
  { template-id: uint }
  {
    royalty-percentage: uint,
    total-earnings: uint,
    withdrawable-earnings: uint
  }
)

(define-map creator-earnings
  { creator: principal }
  {
    total-earned: uint,
    total-withdrawn: uint,
    pending-earnings: uint
  }
)

(define-map template-ratings
  { template-id: uint }
  {
    total-ratings: uint,
    sum-ratings: uint,
    average-rating: uint,
    five-star: uint,
    four-star: uint,
    three-star: uint,
    two-star: uint,
    one-star: uint
  }
)

(define-map user-ratings
  { template-id: uint, user: principal }
  {
    rating: uint,
    review: (string-ascii 500),
    deployment-id: uint,
    rated-at: uint
  }
)

(define-map creator-reputation
  { creator: principal }
  {
    total-templates: uint,
    total-deployments: uint,
    average-rating: uint,
    total-ratings: uint
  }
)

(define-map template-access-mode
  { template-id: uint }
  { mode: uint }
)

(define-map template-whitelist
  { template-id: uint, user: principal }
  { allowed: bool }
)

(define-map template-blacklist
  { template-id: uint, user: principal }
  { blocked: bool }
)

(define-map template-credits
  { template-id: uint, user: principal }
  { credits: uint }
)

(define-public (create-template (name (string-ascii 50)) 
                               (description (string-ascii 200))
                               (template-code (string-ascii 10000))
                               (creation-fee uint)
                               (royalty-percentage uint))
  (let ((template-id (var-get next-product-id)))
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> (len template-code) u0) ERR_INVALID_PARAMETERS)
    (asserts! (>= creation-fee u0) ERR_INVALID_PARAMETERS)
    (asserts! (<= royalty-percentage (var-get max-royalty-percentage)) ERR_INVALID_ROYALTY)
    
    (map-set product-templates
      { template-id: template-id }
      {
        name: name,
        description: description,
        template-code: template-code,
        creation-fee: creation-fee,
        creator: tx-sender,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: u0,
        total-revenue: u0,
        last-deployment-block: u0
      }
    )
    
    (map-set template-versions
      { template-id: template-id, version: u1 }
      {
        template-code: template-code,
        version-notes: "Initial version",
        created-at: stacks-block-height,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-version-info
      { template-id: template-id }
      {
        current-version: u1,
        total-versions: u1,
        latest-version: u1
      }
    )
    
    (map-set template-royalties
      { template-id: template-id }
      {
        royalty-percentage: royalty-percentage,
        total-earnings: u0,
        withdrawable-earnings: u0
      }
    )
    
    (let ((current-earnings (default-to
            { total-earned: u0, total-withdrawn: u0, pending-earnings: u0 }
            (map-get? creator-earnings { creator: tx-sender })))
          (current-reputation (default-to
            { total-templates: u0, total-deployments: u0, average-rating: u0, total-ratings: u0 }
            (map-get? creator-reputation { creator: tx-sender }))))
      (map-set creator-earnings
        { creator: tx-sender }
        current-earnings
      )
      (map-set creator-reputation
        { creator: tx-sender }
        (merge current-reputation { total-templates: (+ (get total-templates current-reputation) u1) })
      )
    )
    
    (map-set template-ratings
      { template-id: template-id }
      {
        total-ratings: u0,
        sum-ratings: u0,
        average-rating: u0,
        five-star: u0,
        four-star: u0,
        three-star: u0,
        two-star: u0,
        one-star: u0
      }
    )
    
    (var-set next-product-id (+ template-id u1))
    (ok template-id)
  )
)

(define-public (deploy-contract (template-id uint)
                               (initialization-data (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (contract-id (var-get next-product-id))
        (total-fee (+ (get creation-fee template) (var-get deployment-fee)))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))))
    
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (asserts! (>= (stx-get-balance tx-sender) total-fee) ERR_INSUFFICIENT_FUNDS)
    (try! (verify-template-access template-id tx-sender))
    
    (try! (process-deployment-payment template-id total-fee))
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      {
        template-id: template-id,
        deployer: tx-sender,
        contract-address: tx-sender,
        deployment-block: stacks-block-height,
        initialization-data: initialization-data,
        active: true
      }
    )
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { deployments: (+ (get deployments template) u1) })
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (+ (get total-deployments current-stats) u1),
        total-revenue: (+ (get total-revenue current-stats) (get creation-fee template)),
        last-deployment-block: stacks-block-height
      }
    )
    
    (let ((current-user-deployments (default-to
            { contract-ids: (list) }
            (map-get? user-deployments { user: tx-sender, template-id: template-id }))))
      (map-set user-deployments
        { user: tx-sender, template-id: template-id }
        { contract-ids: (unwrap! (as-max-len? 
                                  (append (get contract-ids current-user-deployments) contract-id) 
                                  u100) ERR_INVALID_PARAMETERS) }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) total-fee))
    (var-set next-product-id (+ contract-id u1))
    
    (ok contract-id)
  )
)

(define-public (toggle-template-status (template-id uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { active: (not (get active template)) })
    )
    
    (ok (not (get active template)))
  )
)

(define-public (update-template-fee (template-id uint) (new-fee uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (>= new-fee u0) ERR_INVALID_PARAMETERS)
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { creation-fee: new-fee })
    )
    
    (ok new-fee)
  )
)

(define-public (deactivate-deployment (contract-id uint))
  (let ((deployment (unwrap! (map-get? deployed-contracts { contract-id: contract-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get deployer deployment)) ERR_UNAUTHORIZED)
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      (merge deployment { active: false })
    )
    
    (ok true)
  )
)

(define-public (set-factory-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set factory-enabled enabled)
    (ok enabled)
  )
)

(define-public (set-deployment-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (>= fee u0) ERR_INVALID_PARAMETERS)
    (var-set deployment-fee fee)
    (ok fee)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok new-owner)
  )
)

(define-public (withdraw-revenue (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get total-revenue)) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
    (var-set total-revenue (- (var-get total-revenue) amount))
    (ok amount)
  )
)

(define-public (create-template-version (template-id uint)
                                       (template-code (string-ascii 10000))
                                       (version-notes (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-info (unwrap! (map-get? template-version-info { template-id: template-id }) ERR_VERSION_NOT_FOUND))
        (new-version (+ (get latest-version version-info) u1)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (> (len template-code) u0) ERR_INVALID_PARAMETERS)
    
    (map-set template-versions
      { template-id: template-id, version: new-version }
      {
        template-code: template-code,
        version-notes: version-notes,
        created-at: stacks-block-height,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-version-info
      { template-id: template-id }
      {
        current-version: new-version,
        total-versions: (+ (get total-versions version-info) u1),
        latest-version: new-version
      }
    )
    
    (ok new-version)
  )
)

(define-public (deploy-contract-version (template-id uint)
                                       (version uint)
                                       (initialization-data (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND))
        (contract-id (var-get next-product-id))
        (total-fee (+ (get creation-fee template) (var-get deployment-fee)))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))))
    
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (asserts! (get active version-data) ERR_INVALID_VERSION)
    (asserts! (>= (stx-get-balance tx-sender) total-fee) ERR_INSUFFICIENT_FUNDS)
    (try! (verify-template-access template-id tx-sender))
    
    (try! (process-deployment-payment template-id total-fee))
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      {
        template-id: template-id,
        deployer: tx-sender,
        contract-address: tx-sender,
        deployment-block: stacks-block-height,
        initialization-data: initialization-data,
        active: true
      }
    )
    
    (map-set template-versions
      { template-id: template-id, version: version }
      (merge version-data { deployments: (+ (get deployments version-data) u1) })
    )
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { deployments: (+ (get deployments template) u1) })
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (+ (get total-deployments current-stats) u1),
        total-revenue: (+ (get total-revenue current-stats) (get creation-fee template)),
        last-deployment-block: stacks-block-height
      }
    )
    
    (let ((current-user-deployments (default-to
            { contract-ids: (list) }
            (map-get? user-deployments { user: tx-sender, template-id: template-id }))))
      (map-set user-deployments
        { user: tx-sender, template-id: template-id }
        { contract-ids: (unwrap! (as-max-len? 
                                  (append (get contract-ids current-user-deployments) contract-id) 
                                  u100) ERR_INVALID_PARAMETERS) }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) total-fee))
    (var-set next-product-id (+ contract-id u1))
    
    (ok contract-id)
  )
)

(define-public (set-current-version (template-id uint) (version uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND))
        (version-info (unwrap! (map-get? template-version-info { template-id: template-id }) ERR_VERSION_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (get active version-data) ERR_INVALID_VERSION)
    
    (map-set template-version-info
      { template-id: template-id }
      (merge version-info { current-version: version })
    )
    
    (ok version)
  )
)

(define-public (toggle-version-status (template-id uint) (version uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    
    (map-set template-versions
      { template-id: template-id, version: version }
      (merge version-data { active: (not (get active version-data)) })
    )
    
    (ok (not (get active version-data)))
  )
)

(define-private (process-deployment-payment (template-id uint) (total-fee uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (royalty-info (unwrap! (map-get? template-royalties { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (creator (get creator template))
        (royalty-amount (/ (* total-fee (get royalty-percentage royalty-info)) u10000))
        (marketplace-fee (/ (* total-fee (var-get marketplace-fee-percentage)) u10000))
        (factory-amount (- total-fee (+ royalty-amount marketplace-fee)))
        (current-creator-earnings (default-to
          { total-earned: u0, total-withdrawn: u0, pending-earnings: u0 }
          (map-get? creator-earnings { creator: creator }))))
    
    (try! (stx-transfer? factory-amount tx-sender (var-get contract-owner)))
    (try! (stx-transfer? royalty-amount tx-sender creator))
    (try! (stx-transfer? marketplace-fee tx-sender (as-contract tx-sender)))
    
    (map-set template-royalties
      { template-id: template-id }
      {
        royalty-percentage: (get royalty-percentage royalty-info),
        total-earnings: (+ (get total-earnings royalty-info) royalty-amount),
        withdrawable-earnings: (+ (get withdrawable-earnings royalty-info) royalty-amount)
      }
    )
    
    (map-set creator-earnings
      { creator: creator }
      {
        total-earned: (+ (get total-earned current-creator-earnings) royalty-amount),
        total-withdrawn: (get total-withdrawn current-creator-earnings),
        pending-earnings: (+ (get pending-earnings current-creator-earnings) royalty-amount)
      }
    )
    
    (ok true)
  )
)

(define-private (verify-template-access (template-id uint) (user principal))
  (let ((mode-info (default-to { mode: u0 } (map-get? template-access-mode { template-id: template-id })))
        (white-info (default-to { allowed: false } (map-get? template-whitelist { template-id: template-id, user: user })))
        (black-info (default-to { blocked: false } (map-get? template-blacklist { template-id: template-id, user: user })))
        (mode (get mode mode-info))
        (is-allowed (get allowed white-info))
        (is-blocked (get blocked black-info)))
    (if (is-eq mode u0)
        (if is-blocked ERR_ACCESS_DENIED (ok true))
        (if (is-eq mode u1)
            (if is-allowed (ok true) ERR_ACCESS_DENIED)
            (if is-blocked ERR_ACCESS_DENIED (ok true))))
  )
)

(define-public (set-template-access-mode (template-id uint) (mode uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq mode u0) (or (is-eq mode u1) (is-eq mode u2))) ERR_INVALID_PARAMETERS)
    (map-set template-access-mode { template-id: template-id } { mode: mode })
    (ok mode)
  )
)

(define-public (set-template-whitelist (template-id uint) (user principal) (allowed bool))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (map-set template-whitelist { template-id: template-id, user: user } { allowed: allowed })
    (ok allowed)
  )
)

(define-public (set-template-blacklist (template-id uint) (user principal) (blocked bool))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (map-set template-blacklist { template-id: template-id, user: user } { blocked: blocked })
    (ok blocked)
  )
)

(define-public (purchase-deployment-credits (template-id uint) (quantity uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (unit-fee (+ (get creation-fee template) (var-get deployment-fee)))
        (total-fee (* unit-fee quantity))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))
        )
        (current-credits (default-to
          { credits: u0 }
          (map-get? template-credits { template-id: template-id, user: tx-sender }))
        ))
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) total-fee) ERR_INSUFFICIENT_FUNDS)
    (try! (process-deployment-payment template-id total-fee))
    (map-set template-credits
      { template-id: template-id, user: tx-sender }
      { credits: (+ (get credits current-credits) quantity) }
    )
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (get total-deployments current-stats),
        total-revenue: (+ (get total-revenue current-stats) (* (get creation-fee template) quantity)),
        last-deployment-block: stacks-block-height
      }
    )
    (var-set total-revenue (+ (var-get total-revenue) total-fee))
    (ok quantity)
  )
)

(define-public (transfer-deployment-credits (template-id uint) (to principal) (quantity uint))
  (let ((from-credits (unwrap! (map-get? template-credits { template-id: template-id, user: tx-sender }) ERR_INVALID_AMOUNT))
        (to-credits (default-to { credits: u0 } (map-get? template-credits { template-id: template-id, user: to }))))
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (get credits from-credits) quantity) ERR_INSUFFICIENT_FUNDS)
    (map-set template-credits { template-id: template-id, user: tx-sender } { credits: (- (get credits from-credits) quantity) })
    (map-set template-credits { template-id: template-id, user: to } { credits: (+ (get credits to-credits) quantity) })
    (ok quantity)
  )
)

(define-public (deploy-contract-with-credit (template-id uint)
                                           (initialization-data (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (credits-info (unwrap! (map-get? template-credits { template-id: template-id, user: tx-sender }) ERR_INSUFFICIENT_FUNDS))
        (contract-id (var-get next-product-id))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))))
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (try! (verify-template-access template-id tx-sender))
    (asserts! (> (get credits credits-info) u0) ERR_INSUFFICIENT_FUNDS)
    (map-set template-credits
      { template-id: template-id, user: tx-sender }
      { credits: (- (get credits credits-info) u1) }
    )
    (map-set deployed-contracts
      { contract-id: contract-id }
      {
        template-id: template-id,
        deployer: tx-sender,
        contract-address: tx-sender,
        deployment-block: stacks-block-height,
        initialization-data: initialization-data,
        active: true
      }
    )
    (map-set product-templates
      { template-id: template-id }
      (merge template { deployments: (+ (get deployments template) u1) })
    )
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (+ (get total-deployments current-stats) u1),
        total-revenue: (get total-revenue current-stats),
        last-deployment-block: stacks-block-height
      }
    )
    (let ((current-user-deployments (default-to
            { contract-ids: (list) }
            (map-get? user-deployments { user: tx-sender, template-id: template-id }))))
      (map-set user-deployments
        { user: tx-sender, template-id: template-id }
        { contract-ids: (unwrap! (as-max-len? 
                                  (append (get contract-ids current-user-deployments) contract-id) 
                                  u100) ERR_INVALID_PARAMETERS) }
      )
    )
    (var-set next-product-id (+ contract-id u1))
    (ok contract-id)
  )
)

(define-read-only (get-user-credits (template-id uint) (user principal))
  (map-get? template-credits { template-id: template-id, user: user })
)

(define-public (set-template-royalty (template-id uint) (new-royalty-percentage uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (royalty-info (unwrap! (map-get? template-royalties { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (<= new-royalty-percentage (var-get max-royalty-percentage)) ERR_INVALID_ROYALTY)
    
    (map-set template-royalties
      { template-id: template-id }
      (merge royalty-info { royalty-percentage: new-royalty-percentage })
    )
    
    (ok new-royalty-percentage)
  )
)

(define-public (withdraw-creator-earnings (amount uint))
  (let ((current-earnings (unwrap! (map-get? creator-earnings { creator: tx-sender }) ERR_NO_EARNINGS)))
    
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get pending-earnings current-earnings)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    (map-set creator-earnings
      { creator: tx-sender }
      {
        total-earned: (get total-earned current-earnings),
        total-withdrawn: (+ (get total-withdrawn current-earnings) amount),
        pending-earnings: (- (get pending-earnings current-earnings) amount)
      }
    )
    
    (ok amount)
  )
)

(define-public (set-marketplace-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u1000) ERR_INVALID_PARAMETERS)
    (var-set marketplace-fee-percentage new-fee-percentage)
    (ok new-fee-percentage)
  )
)

(define-public (set-max-royalty (new-max-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= new-max-percentage u10000) ERR_INVALID_PARAMETERS)
    (var-set max-royalty-percentage new-max-percentage)
    (ok new-max-percentage)
  )
)

(define-public (rate-template (template-id uint) 
                             (contract-id uint) 
                             (rating uint) 
                             (review (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (deployment (unwrap! (map-get? deployed-contracts { contract-id: contract-id }) ERR_PRODUCT_NOT_FOUND))
        (existing-rating (map-get? user-ratings { template-id: template-id, user: tx-sender }))
        (current-ratings (default-to
          { total-ratings: u0, sum-ratings: u0, average-rating: u0, five-star: u0, four-star: u0, three-star: u0, two-star: u0, one-star: u0 }
          (map-get? template-ratings { template-id: template-id })))
        (creator (get creator template))
        (creator-rep (default-to
          { total-templates: u0, total-deployments: u0, average-rating: u0, total-ratings: u0 }
          (map-get? creator-reputation { creator: creator }))))
    
    (asserts! (is-eq (get deployer deployment) tx-sender) ERR_NOT_DEPLOYER)
    (asserts! (is-eq (get template-id deployment) template-id) ERR_INVALID_PARAMETERS)
    (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    
    (map-set user-ratings
      { template-id: template-id, user: tx-sender }
      {
        rating: rating,
        review: review,
        deployment-id: contract-id,
        rated-at: stacks-block-height
      }
    )
    
    (let ((new-total (+ (get total-ratings current-ratings) u1))
          (new-sum (+ (get sum-ratings current-ratings) rating))
          (new-five (if (is-eq rating u5) (+ (get five-star current-ratings) u1) (get five-star current-ratings)))
          (new-four (if (is-eq rating u4) (+ (get four-star current-ratings) u1) (get four-star current-ratings)))
          (new-three (if (is-eq rating u3) (+ (get three-star current-ratings) u1) (get three-star current-ratings)))
          (new-two (if (is-eq rating u2) (+ (get two-star current-ratings) u1) (get two-star current-ratings)))
          (new-one (if (is-eq rating u1) (+ (get one-star current-ratings) u1) (get one-star current-ratings)))
          (new-average (/ (* new-sum u100) new-total)))
      
      (map-set template-ratings
        { template-id: template-id }
        {
          total-ratings: new-total,
          sum-ratings: new-sum,
          average-rating: new-average,
          five-star: new-five,
          four-star: new-four,
          three-star: new-three,
          two-star: new-two,
          one-star: new-one
        }
      )
      
      (let ((creator-new-total (+ (get total-ratings creator-rep) u1))
            (creator-new-sum (+ (* (get average-rating creator-rep) (get total-ratings creator-rep)) rating))
            (creator-new-average (if (> creator-new-total u0) (/ (* creator-new-sum u100) creator-new-total) u0)))
        
        (map-set creator-reputation
          { creator: creator }
          {
            total-templates: (get total-templates creator-rep),
            total-deployments: (get total-deployments creator-rep),
            average-rating: creator-new-average,
            total-ratings: creator-new-total
          }
        )
      )
      
      (ok true)
    )
  )
)

(define-public (update-rating (template-id uint) (rating uint) (review (string-ascii 500)))
  (let ((existing-rating (unwrap! (map-get? user-ratings { template-id: template-id, user: tx-sender }) ERR_PRODUCT_NOT_FOUND))
        (template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (current-ratings (unwrap! (map-get? template-ratings { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (creator (get creator template))
        (creator-rep (unwrap! (map-get? creator-reputation { creator: creator }) ERR_PRODUCT_NOT_FOUND))
        (old-rating (get rating existing-rating)))
    
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    
    (map-set user-ratings
      { template-id: template-id, user: tx-sender }
      (merge existing-rating { rating: rating, review: review, rated-at: stacks-block-height })
    )
    
    (let ((adjusted-sum (+ (- (get sum-ratings current-ratings) old-rating) rating))
          (new-average (/ (* adjusted-sum u100) (get total-ratings current-ratings)))
          (old-five (if (is-eq old-rating u5) (- (get five-star current-ratings) u1) (get five-star current-ratings)))
          (old-four (if (is-eq old-rating u4) (- (get four-star current-ratings) u1) (get four-star current-ratings)))
          (old-three (if (is-eq old-rating u3) (- (get three-star current-ratings) u1) (get three-star current-ratings)))
          (old-two (if (is-eq old-rating u2) (- (get two-star current-ratings) u1) (get two-star current-ratings)))
          (old-one (if (is-eq old-rating u1) (- (get one-star current-ratings) u1) (get one-star current-ratings)))
          (new-five (if (is-eq rating u5) (+ old-five u1) old-five))
          (new-four (if (is-eq rating u4) (+ old-four u1) old-four))
          (new-three (if (is-eq rating u3) (+ old-three u1) old-three))
          (new-two (if (is-eq rating u2) (+ old-two u1) old-two))
          (new-one (if (is-eq rating u1) (+ old-one u1) old-one)))
      
      (map-set template-ratings
        { template-id: template-id }
        {
          total-ratings: (get total-ratings current-ratings),
          sum-ratings: adjusted-sum,
          average-rating: new-average,
          five-star: new-five,
          four-star: new-four,
          three-star: new-three,
          two-star: new-two,
          one-star: new-one
        }
      )
      
      (let ((creator-adjusted-sum (+ (- (* (get average-rating creator-rep) (get total-ratings creator-rep)) old-rating) rating))
            (creator-new-average (if (> (get total-ratings creator-rep) u0) 
                                    (/ (* creator-adjusted-sum u100) (get total-ratings creator-rep)) 
                                    u0)))
        
        (map-set creator-reputation
          { creator: creator }
          (merge creator-rep { average-rating: creator-new-average })
        )
      )
      
      (ok true)
    )
  )
)

(define-read-only (get-template (template-id uint))
  (map-get? product-templates { template-id: template-id })
)

(define-read-only (get-deployment (contract-id uint))
  (map-get? deployed-contracts { contract-id: contract-id })
)

(define-read-only (get-user-deployments (user principal) (template-id uint))
  (map-get? user-deployments { user: user, template-id: template-id })
)

(define-read-only (get-template-stats (template-id uint))
  (map-get? template-statistics { template-id: template-id })
)

(define-read-only (get-factory-info)
  {
    owner: (var-get contract-owner),
    enabled: (var-get factory-enabled),
    deployment-fee: (var-get deployment-fee),
    next-product-id: (var-get next-product-id),
    total-revenue: (var-get total-revenue)
  }
)

(define-read-only (get-factory-stats)
  {
    owner: (var-get contract-owner),
    enabled: (var-get factory-enabled),
    deployment-fee: (var-get deployment-fee),
    next-product-id: (var-get next-product-id),
    total-revenue: (var-get total-revenue)
  }
)

(define-read-only (get-template-version (template-id uint) (version uint))
  (map-get? template-versions { template-id: template-id, version: version })
)

(define-read-only (get-template-version-info (template-id uint))
  (map-get? template-version-info { template-id: template-id })
)

(define-read-only (get-current-template-version (template-id uint))
  (let ((version-info (map-get? template-version-info { template-id: template-id })))
    (match version-info
      info (map-get? template-versions { template-id: template-id, version: (get current-version info) })
      none
    )
  )
)

(define-read-only (get-latest-template-version (template-id uint))
  (let ((version-info (map-get? template-version-info { template-id: template-id })))
    (match version-info
      info (map-get? template-versions { template-id: template-id, version: (get latest-version info) })
      none
    )
  )
)

(define-read-only (get-template-royalty (template-id uint))
  (map-get? template-royalties { template-id: template-id })
)

(define-read-only (get-creator-earnings (creator principal))
  (map-get? creator-earnings { creator: creator })
)

(define-read-only (get-marketplace-info)
  {
    marketplace-fee-percentage: (var-get marketplace-fee-percentage),
    max-royalty-percentage: (var-get max-royalty-percentage),
    factory-owner: (var-get contract-owner)
  }
)

(define-read-only (calculate-deployment-costs (template-id uint))
  (let ((template (map-get? product-templates { template-id: template-id }))
        (royalty-info (map-get? template-royalties { template-id: template-id })))
    (match template
      template-data (match royalty-info
        royalty-data (let ((base-fee (get creation-fee template-data))
                           (deploy-fee (var-get deployment-fee))
                           (total-fee (+ base-fee deploy-fee))
                           (royalty-amount (/ (* total-fee (get royalty-percentage royalty-data)) u10000))
                           (marketplace-fee (/ (* total-fee (var-get marketplace-fee-percentage)) u10000))
                           (factory-amount (- total-fee (+ royalty-amount marketplace-fee))))
                       (some { 
                         total-cost: total-fee,
                         creator-royalty: royalty-amount,
                         marketplace-fee: marketplace-fee,
                         factory-revenue: factory-amount
                       }))
                       none)
                       none
                       )
                       )
)

(define-read-only (get-template-rating (template-id uint))
  (map-get? template-ratings { template-id: template-id })
)

(define-read-only (get-user-rating (template-id uint) (user principal))
  (map-get? user-ratings { template-id: template-id, user: user })
)

(define-read-only (get-creator-reputation (creator principal))
  (map-get? creator-reputation { creator: creator })
)

(define-read-only (get-template-rating-summary (template-id uint))
  (match (map-get? template-ratings { template-id: template-id })
    ratings (some {
      average: (get average-rating ratings),
      total: (get total-ratings ratings),
      distribution: {
        five-star: (get five-star ratings),
        four-star: (get four-star ratings),
        three-star: (get three-star ratings),
        two-star: (get two-star ratings),
        one-star: (get one-star ratings)
      }
    })
    none
  )
)

(define-read-only (has-user-rated (template-id uint) (user principal))
  (is-some (map-get? user-ratings { template-id: template-id, user: user }))
)
